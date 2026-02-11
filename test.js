// ==== Prologue: fake web worker bits ====
globalThis.self = globalThis;
globalThis.location = { href: "about:blank" };

// Very simple timer + microtask shims
globalThis.queueMicrotask = cb => Promise.resolve().then(cb);
globalThis.setTimeout = (fn, ms) => { fn(); return 1; };
globalThis.clearTimeout = id => {};

// ==== testharness.js ====
(function (global_scope)
{
    // default timeout is 10 seconds, test can override if needed
    var settings = {
        output:true,
        harness_timeout:{
            "normal":10000,
            "long":60000
        },
        test_timeout:null,
        message_events: ["start", "test_state", "result", "completion"],
        debug: false,
    };

    var xhtml_ns = "http://www.w3.org/1999/xhtml";

    /*
     * TestEnvironment is an abstraction for the environment in which the test
     * harness is used. Each implementation of a test environment has to provide
     * the following interface:
     *
     * interface TestEnvironment {
     *   // Invoked after the global 'tests' object has been created and it's
     *   // safe to call add_*_callback() to register event handlers.
     *   void on_tests_ready();
     *
     *   // Invoked after setup() has been called to notify the test environment
     *   // of changes to the test harness properties.
     *   void on_new_harness_properties(object properties);
     *
     *   // Should return a new unique default test name.
     *   DOMString next_default_test_name();
     *
     *   // Should return the test harness timeout duration in milliseconds.
     *   float test_timeout();
     * };
     */

    /*
     * JavaScript shells.
     *
     * This class is used as the test_environment when testharness is running
     * inside a JavaScript shell.
     */
    function ShellTestEnvironment() {
        this.name_counter = 0;
        this.all_loaded = false;
        this.on_loaded_callback = null;
        Promise.resolve().then(function() {
            this.all_loaded = true;
            if (this.on_loaded_callback) {
                this.on_loaded_callback();
            }
        }.bind(this));
        this.message_list = [];
        this.message_ports = [];
    }

    ShellTestEnvironment.prototype.next_default_test_name = function() {
        var suffix = this.name_counter > 0 ? " " + this.name_counter : "";
        this.name_counter++;
        return get_title() + suffix;
    };

    ShellTestEnvironment.prototype.on_new_harness_properties = function() {};

    ShellTestEnvironment.prototype.on_tests_ready = function() {};

    ShellTestEnvironment.prototype.add_on_loaded_callback = function(callback) {
        if (this.all_loaded) {
            callback();
        } else {
            this.on_loaded_callback = callback;
        }
    };

    ShellTestEnvironment.prototype.test_timeout = function() {
        // Tests running in a shell don't have a default timeout, so behave as
        // if settings.explicit_timeout is true.
        return null;
    };

    function create_test_environment() {
        return new ShellTestEnvironment();
    }

    var test_environment = create_test_environment();

    function is_shared_worker(worker) {
        return 'SharedWorker' in global_scope && worker instanceof SharedWorker;
    }

    function is_service_worker(worker) {
        // The worker object may be from another execution context,
        // so do not use instanceof here.
        return 'ServiceWorker' in global_scope &&
            Object.prototype.toString.call(worker) === '[object ServiceWorker]';
    }

    var seen_func_name = Object.create(null);

    /**
     * @callback TestFunction
     * @param {Test} test - The test currnetly being run.
     * @param {Any[]} args - Additional args to pass to function.
     *
     */

    /**
     * Create a synchronous test
     *
     * @param {TestFunction} func - Test function. This is executed
     * immediately. If it returns without error, the test status is
     * set to ``PASS``. If it throws an :js:class:`AssertionError`, or
     * any other exception, the test status is set to ``FAIL``
     * (typically from an `assert` function).
     * @param {String} name - Test name. This must be unique in a
     * given file and must be invariant between runs.
     */
    function test(func, name, properties)
    {
        if (tests.promise_setup_called) {
            tests.status.status = tests.status.ERROR;
            tests.status.message = '`test` invoked after `promise_setup`';
            tests.complete();
        }
        var test_name = ""
        if (name !== undefined) test_name = name;
        var test_obj = new Test(test_name, properties);
        var value = test_obj.step(func, test_obj, test_obj);

        if (value !== undefined) {
            var msg = 'Test named "' + test_name +
                '" passed a function to `test` that returned a value.';

            try {
                if (value && typeof value.then === 'function') {
                    msg += ' Consider using `promise_test` instead when ' +
                        'using Promises or async/await.';
                }
            } catch (err) {}

            tests.status.status = tests.status.ERROR;
            tests.status.message = msg;
        }

        if (test_obj.phase === test_obj.phases.STARTED) {
            test_obj.done();
        }
    }

    /**
     * Create an asynchronous test
     *
     * @param {TestFunction|string} funcOrName - Initial step function
     * to call immediately with the test name as an argument (if any),
     * or name of the test.
     * @param {String} name - Test name (if a test function was
     * provided). This must be unique in a given file and must be
     * invariant between runs.
     * @returns {Test} An object representing the ongoing test.
     */
    function async_test(func, name, properties)
    {
        if (tests.promise_setup_called) {
            tests.status.status = tests.status.ERROR;
            tests.status.message = '`async_test` invoked after `promise_setup`';
            tests.complete();
        }
        if (typeof func !== "function") {
            properties = name;
            name = func;
            func = null;
        }
        var test_name = ""
        if (name !== undefined) test_name = name;
        var test_obj = new Test(test_name, properties);
        if (func) {
            var value = test_obj.step(func, test_obj, test_obj);

            // Test authors sometimes return values to async_test, expecting us
            // to handle the value somehow. Make doing so a harness error to be
            // clear this is invalid, and point authors to promise_test if it
            // may be appropriate.
            //
            // Note that we only perform this check on the initial function
            // passed to async_test, not on any later steps - we haven't seen a
            // consistent problem with those (and it's harder to check).
            if (value !== undefined) {
                var msg = 'Test named "' + test_name +
                    '" passed a function to `async_test` that returned a value.';

                try {
                    if (value && typeof value.then === 'function') {
                        msg += ' Consider using `promise_test` instead when ' +
                            'using Promises or async/await.';
                    }
                } catch (err) {}

                tests.set_status(tests.status.ERROR, msg);
                tests.complete();
            }
        }
        return test_obj;
    }

    /**
     * Create a promise test.
     *
     * Promise tests are tests which are represented by a promise
     * object. If the promise is fulfilled the test passes, if it's
     * rejected the test fails, otherwise the test passes.
     *
     * @param {TestFunction} func - Test function. This must return a
     * promise. The test is automatically marked as complete once the
     * promise settles.
     * @param {String} name - Test name. This must be unique in a
     * given file and must be invariant between runs.
     */
    function promise_test(func, name, properties) {
        if (typeof func !== "function") {
            properties = name;
            name = func;
            func = null;
        }
        var test_name = name;
        var test = new Test(test_name, properties);
        test._is_promise_test = true;

        // If there is no promise tests queue make one.
        if (!tests.promise_tests) {
            tests.promise_tests = Promise.resolve();
        }
        tests.promise_tests = tests.promise_tests.then(function() {
            return new Promise(function(resolve) {
                var promise = test.step(func, test, test);

                test.step(function() {
                    assert(!!promise, "promise_test", null,
                           "test body must return a 'thenable' object (received ${value})",
                           {value:promise});
                    assert(typeof promise.then === "function", "promise_test", null,
                           "test body must return a 'thenable' object (received an object with no `then` method)",
                           null);
                });

                // Test authors may use the `step` method within a
                // `promise_test` even though this reflects a mixture of
                // asynchronous control flow paradigms. The "done" callback
                // should be registered prior to the resolution of the
                // user-provided Promise to avoid timeouts in cases where the
                // Promise does not settle but a `step` function has thrown an
                // error.
                add_test_done_callback(test, resolve);

                Promise.resolve(promise)
                    .catch(test.step_func(
                        function(value) {
                            if (value instanceof AssertionError) {
                                throw value;
                            }
                            assert(false, "promise_test", null,
                                   "Unhandled rejection with value: ${value}", {value:value});
                        }))
                    .then(function() {
                        test.done();
                    });
                });
        });
    }

    /**
     * Make a copy of a Promise in the current realm.
     *
     * @param {Promise} promise the given promise that may be from a different
     *                          realm
     * @returns {Promise}
     *
     * An arbitrary promise provided by the caller may have originated
     * in another frame that have since navigated away, rendering the
     * frame's document inactive. Such a promise cannot be used with
     * `await` or Promise.resolve(), as microtasks associated with it
     * may be prevented from being run. See `issue
     * 5319<https://github.com/whatwg/html/issues/5319>`_ for a
     * particular case.
     *
     * In functions we define here, there is an expectation from the caller
     * that the promise is from the current realm, that can always be used with
     * `await`, etc. We therefore create a new promise in this realm that
     * inherit the value and status from the given promise.
     */

    function bring_promise_to_current_realm(promise) {
        return new Promise(promise.then.bind(promise));
    }

    /**
     * Assert that a Promise is rejected with the right ECMAScript exception.
     *
     * @param {Test} test - the `Test` to use for the assertion.
     * @param {Function} constructor - The expected exception constructor.
     * @param {Promise} promise - The promise that's expected to
     * reject with the given exception.
     * @param {string} [description] Error message to add to assert in case of
     *                               failure.
     */
    function promise_rejects_js(test, constructor, promise, description) {
        return bring_promise_to_current_realm(promise)
            .then(test.unreached_func("Should have rejected: " + description))
            .catch(function(e) {
                assert_throws_js_impl(constructor, function() { throw e; },
                                      description, "promise_rejects_js");
            });
    }

    /**
     * @typedef {Object} SettingsObject
     * @property {bool} single_test - Use the single-page-test
     * mode. In this mode the Document represents a single
     * `async_test`. Asserts may be used directly without requiring
     * `Test.step` or similar wrappers, and any exceptions set the
     * status of the test rather than the status of the harness.
     * @property {bool} allow_uncaught_exception - don't treat an
     * uncaught exception as an error; needed when e.g. testing the
     * `window.onerror` handler.
     * @property {boolean} explicit_done - Wait for a call to `done()`
     * before declaring all tests complete (this is always true for
     * single-page tests).
     * @property hide_test_state - hide the test state output while
     * the test is running; This is helpful when the output of the test state
     * may interfere the test results.
     * @property {bool} explicit_timeout - disable file timeout; only
     * stop waiting for results when the `timeout()` function is
     * called This should typically only be set for manual tests, or
     * by a test runner that providees its own timeout mechanism.
     * @property {number} timeout_multiplier - Multiplier to apply to
     * per-test timeouts. This should only be set by a test runner.
     * @property {Document} output_document - The document to which
     * results should be logged. By default this is the current
     * document but could be an ancestor document in some cases e.g. a
     * SVG test loaded in an HTML wrapper
     *
     */

    /**
     * Configure the harness
     *
     * @param {Function|SettingsObject} funcOrProperties - Either a
     * setup function to run, or a set of properties. If this is a
     * function that function is run synchronously. Any exception in
     * the function will set the overall harness status to `ERROR`.
     * @param {SettingsObject} maybeProperties - An object containing
     * the settings to use, if the first argument is a function.
     *
     */
    function setup(func_or_properties, maybe_properties)
    {
        var func = null;
        var properties = {};
        if (arguments.length === 2) {
            func = func_or_properties;
            properties = maybe_properties;
        } else if (func_or_properties instanceof Function) {
            func = func_or_properties;
        } else {
            properties = func_or_properties;
        }
        tests.setup(func, properties);
        test_environment.on_new_harness_properties(properties);
    }

    /**
     * Configure the harness, waiting for a promise to resolve
     * before running any `promise_test` tests.
     *
     * @param {Function} func - Function returning a promise that's
     * run synchronously. Promise tests are not run until after this
     * function has resolved.
     * @param {SettingsObject} [properties] - An object containing
     * the harness settings to use.
     *
     */
    function promise_setup(func, properties={})
    {
        if (typeof func !== "function") {
            tests.set_status(tests.status.ERROR,
                             "`promise_setup` invoked without a function");
            tests.complete();
            return;
        }
        tests.promise_setup_called = true;

        if (!tests.promise_tests) {
            tests.promise_tests = Promise.resolve();
        }

        tests.promise_tests = tests.promise_tests
            .then(function()
                  {
                      var result;

                      tests.setup(null, properties);
                      result = func();
                      test_environment.on_new_harness_properties(properties);

                      if (!result || typeof result.then !== "function") {
                          throw "Non-thenable returned by function passed to `promise_setup`";
                      }
                      return result;
                  })
            .catch(function(e)
                   {
                       tests.set_status(tests.status.ERROR,
                                        String(e),
                                        e && e.stack);
                       tests.complete();
                   });
    }

    /**
     * Mark test loading as complete.
     *
     * Typically this function is called implicitly on page load; it's
     * only necessary for users to call this when either the
     * ``explicit_done`` or ``single_test`` properties have been set
     * via the :js:func:`setup` function.
     *
     * For single page tests this marks the test as complete and sets its status.
     * For other tests, this marks test loading as complete, but doesn't affect ongoing tests.
     */
    function done() {
        if (tests.tests.length === 0) {
            // `done` is invoked after handling uncaught exceptions, so if the
            // harness status is already set, the corresponding message is more
            // descriptive than the generic message defined here.
            if (tests.status.status === null) {
                tests.status.status = tests.status.ERROR;
                tests.status.message = "done() was called without first defining any tests";
            }

            tests.complete();
            return;
        }
        if (tests.file_is_test) {
            // file is test files never have asynchronous cleanup logic,
            // meaning the fully-synchronous `done` function can be used here.
            tests.tests[0].done();
        }
        tests.end_wait();
    }

    /**
     * @deprecated generate a list of tests from a function and list of arguments
     *
     * This is deprecated because it runs all the tests outside of the test functions
     * and as a result any test throwing an exception will result in no tests being
     * run. In almost all cases, you should simply call test within the loop you would
     * use to generate the parameter list array.
     *
     * @param {Function} func - The function that will be called for each generated tests.
     * @param {Any[][]} args - An array of arrays. Each nested array
     * has the structure `[testName, ...testArgs]`. For each of these nested arrays
     * array, a test is generated with name `testName` and test function equivalent to
     * `func(..testArgs)`.
     */
    function generate_tests(func, args, properties) {
        forEach(args, function(x, i)
                {
                    var name = x[0];
                    test(function()
                         {
                             func.apply(this, x.slice(1));
                         },
                         name,
                         Array.isArray(properties) ? properties[i] : properties);
                });
    }

    /**
     * @deprecated
     *
     * Register a function as a DOM event listener to the
     * given object for the event bubbling phase.
     *
     * @param {EventTarget} object - Event target
     * @param {string} event - Event name
     * @param {Function} callback - Event handler.
     */
    function on_event(object, event, callback)
    {
        object.addEventListener(event, callback, false);
    }

    // Internal helper function to provide timeout-like functionality in
    // environments where there is no setTimeout(). (No timeout ID or
    // clearTimeout().)
    function fake_set_timeout(callback, delay) {
        var p = Promise.resolve();
        var start = Date.now();
        var end = start + delay;
        function check() {
            if ((end - Date.now()) > 0) {
                p.then(check);
            } else {
                callback();
            }
        }
        p.then(check);
    }

    /**
     * Global version of :js:func:`Test.step_timeout` for use in single page tests.
     *
     * @param {Function} func - Function to run after the timeout
     * @param {number} timeout - Time in ms to wait before running the
     * test step. The actual wait time is ``timeout`` x
     * ``timeout_multiplier``.
     */
    function step_timeout(func, timeout) {
        var outer_this = this;
        var args = Array.prototype.slice.call(arguments, 2);
        var local_set_timeout = typeof global_scope.setTimeout === "undefined" ? fake_set_timeout : setTimeout;
        return local_set_timeout(function() {
            func.apply(outer_this, args);
        }, timeout * tests.timeout_multiplier);
    }

    expose(test, 'test');
    expose(async_test, 'async_test');
    expose(promise_test, 'promise_test');
    expose(promise_rejects_js, 'promise_rejects_js');
    expose(generate_tests, 'generate_tests');
    expose(setup, 'setup');
    expose(promise_setup, 'promise_setup');
    expose(done, 'done');
    expose(on_event, 'on_event');
    expose(step_timeout, 'step_timeout');

    /*
     * Return a string truncated to the given length, with ... added at the end
     * if it was longer.
     */
    function truncate(s, len)
    {
        if (s.length > len) {
            return s.substring(0, len - 3) + "...";
        }
        return s;
    }

    /*
     * Return true if object is probably a Node object.
     */
    function is_node(object)
    {
        // I use duck-typing instead of instanceof, because
        // instanceof doesn't work if the node is from another window (like an
        // iframe's contentWindow):
        // http://www.w3.org/Bugs/Public/show_bug.cgi?id=12295
        try {
            var has_node_properties = ("nodeType" in object &&
                                       "nodeName" in object &&
                                       "nodeValue" in object &&
                                       "childNodes" in object);
        } catch (e) {
            // We're probably cross-origin, which means we aren't a node
            return false;
        }

        if (has_node_properties) {
            try {
                object.nodeType;
            } catch (e) {
                // The object is probably Node.prototype or another prototype
                // object that inherits from it, and not a Node instance.
                return false;
            }
            return true;
        }
        return false;
    }

    /**
     * Convert a value to a nice, human-readable string
     *
     * When many JavaScript Object values are coerced to a String, the
     * resulting value will be ``"[object Object]"``. This obscures
     * helpful information, making the coerced value unsuitable for
     * use in assertion messages, test names, and debugging
     * statements. `format_value` produces more distinctive string
     * representations of many kinds of objects, including arrays and
     * the more important DOM Node types. It also translates String
     * values containing control characters to include human-readable
     * representations.
     *
     * @example
     * // "Document node with 2 children"
     * format_value(document);
     * @example
     * // "\"foo\\uffffbar\""
     * format_value("foo\uffffbar");
     * @example
     * // "[-0, Infinity]"
     * format_value([-0, Infinity]);
     * @param {Any} val - The value to convert to a string.
     * @returns {string} - A string representation of ``val``, optimised for human readability.
     */
    function format_value(val, seen)
    {
        if (!seen) {
            seen = [];
        }
        if (typeof val === "object" && val !== null) {
            if (seen.indexOf(val) >= 0) {
                return "[...]";
            }
            seen.push(val);
        }
        if (Array.isArray(val)) {
            let output = "[";
            if (val.beginEllipsis !== undefined) {
                output += "…, ";
            }
            output += val.map(function(x) {return format_value(x, seen);}).join(", ");
            if (val.endEllipsis !== undefined) {
                output += ", …";
            }
            return output + "]";
        }

        switch (typeof val) {
        case "string":
            return '"' + val + '"';
        case "boolean":
        case "undefined":
            return String(val);
        case "number":
            // In JavaScript, -0 === 0 and String(-0) == "0", so we have to
            // special-case.
            if (val === -0 && 1/val === -Infinity) {
                return "-0";
            }
            return String(val);
        case "bigint":
            return String(val) + 'n';
        case "object":
            if (val === null) {
                return "null";
            }

            // Special-case Node objects, since those come up a lot in my tests.  I
            // ignore namespaces.
            if (is_node(val)) {
                switch (val.nodeType) {
                case Node.ELEMENT_NODE:
                    var ret = "<" + val.localName;
                    for (var i = 0; i < val.attributes.length; i++) {
                        ret += " " + val.attributes[i].name + '="' + val.attributes[i].value + '"';
                    }
                    ret += ">" + val.innerHTML + "</" + val.localName + ">";
                    return "Element node " + truncate(ret, 60);
                case Node.TEXT_NODE:
                    return 'Text node "' + truncate(val.data, 60) + '"';
                case Node.PROCESSING_INSTRUCTION_NODE:
                    return "ProcessingInstruction node with target " + format_value(truncate(val.target, 60)) + " and data " + format_value(truncate(val.data, 60));
                case Node.COMMENT_NODE:
                    return "Comment node <!--" + truncate(val.data, 60) + "-->";
                case Node.DOCUMENT_NODE:
                    return "Document node with " + val.childNodes.length + (val.childNodes.length === 1 ? " child" : " children");
                case Node.DOCUMENT_TYPE_NODE:
                    return "DocumentType node";
                case Node.DOCUMENT_FRAGMENT_NODE:
                    return "DocumentFragment node with " + val.childNodes.length + (val.childNodes.length === 1 ? " child" : " children");
                default:
                    return "Node object of unknown type";
                }
            }

        /* falls through */
        default:
            try {
                return typeof val + ' "' + truncate(String(val), 1000) + '"';
            } catch(e) {
                return ("[stringifying object threw " + String(e) +
                        " with type " + String(typeof e) + "]");
            }
        }
    }
    expose(format_value, "format_value");

    /*
     * Assertions
     */

    function expose_assert(f, name) {
        function assert_wrapper(...args) {
            let status = Test.statuses.TIMEOUT;
            let stack = null;
            let new_assert_index = null;
            try {
                if (settings.debug) {
                    console.debug("ASSERT", name, tests.current_test && tests.current_test.name, args);
                }
                if (tests.output) {
                    tests.set_assert(name, args);
                    // Remember the newly pushed assert's index, because `apply`
                    // below might push new asserts.
                    new_assert_index = tests.asserts_run.length - 1;
                }
                const rv = f.apply(undefined, args);
                status = Test.statuses.PASS;
                return rv;
            } catch(e) {
                status = Test.statuses.FAIL;
                stack = e.stack ? e.stack : null;
                throw e;
            } finally {
                if (tests.output && !stack) {
                    stack = get_stack();
                }
                if (tests.output) {
                    tests.set_assert_status(new_assert_index, status, stack);
                }
            }
        }
        expose(assert_wrapper, name);
    }

    /**
     * Assert that ``actual`` is strictly true
     *
     * @param {Any} actual - Value that is asserted to be true
     * @param {string} [description] - Description of the condition being tested
     */
    function assert_true(actual, description)
    {
        assert(actual === true, "assert_true", description,
                                "expected true got ${actual}", {actual:actual});
    }
    expose_assert(assert_true, "assert_true");

    /**
     * Assert that ``actual`` is strictly false
     *
     * @param {Any} actual - Value that is asserted to be false
     * @param {string} [description] - Description of the condition being tested
     */
    function assert_false(actual, description)
    {
        assert(actual === false, "assert_false", description,
                                 "expected false got ${actual}", {actual:actual});
    }
    expose_assert(assert_false, "assert_false");

    function same_value(x, y) {
        if (y !== y) {
            //NaN case
            return x !== x;
        }
        if (x === 0 && y === 0) {
            //Distinguish +0 and -0
            return 1/x === 1/y;
        }
        return x === y;
    }

    /**
     * Assert that ``actual`` is the same value as ``expected``.
     *
     * For objects this compares by object identity; for primitives
     * this distinguishes between 0 and -0, and has correct handling
     * of NaN.
     *
     * @param {Any} actual - Test value.
     * @param {Any} expected - Expected value.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_equals(actual, expected, description)
    {
         /*
          * Test if two primitives are equal or two objects
          * are the same object
          */
        if (typeof actual != typeof expected) {
            assert(false, "assert_equals", description,
                          "expected (" + typeof expected + ") ${expected} but got (" + typeof actual + ") ${actual}",
                          {expected:expected, actual:actual});
            return;
        }
        assert(same_value(actual, expected), "assert_equals", description,
                                             "expected ${expected} but got ${actual}",
                                             {expected:expected, actual:actual});
    }
    expose_assert(assert_equals, "assert_equals");

    /**
     * Assert that ``actual`` is not the same value as ``expected``.
     *
     * Comparison is as for :js:func:`assert_equals`.
     *
     * @param {Any} actual - Test value.
     * @param {Any} expected - The value ``actual`` is expected to be different to.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_not_equals(actual, expected, description)
    {
        assert(!same_value(actual, expected), "assert_not_equals", description,
                                              "got disallowed value ${actual}",
                                              {actual:actual});
    }
    expose_assert(assert_not_equals, "assert_not_equals");

    /**
     * Assert that ``expected`` is an array and ``actual`` is one of the members.
     * This is implemented using ``indexOf``, so doesn't handle NaN or ±0 correctly.
     *
     * @param {Any} actual - Test value.
     * @param {Array} expected - An array that ``actual`` is expected to
     * be a member of.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_in_array(actual, expected, description)
    {
        assert(expected.indexOf(actual) != -1, "assert_in_array", description,
                                               "value ${actual} not in array ${expected}",
                                               {actual:actual, expected:expected});
    }
    expose_assert(assert_in_array, "assert_in_array");

    // This function was deprecated in July of 2015.
    // See https://github.com/web-platform-tests/wpt/issues/2033
    /**
     * @deprecated
     * Recursively compare two objects for equality.
     *
     * See `Issue 2033
     * <https://github.com/web-platform-tests/wpt/issues/2033>`_ for
     * more information.
     *
     * @param {Object} actual - Test value.
     * @param {Object} expected - Expected value.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_object_equals(actual, expected, description)
    {
         assert(typeof actual === "object" && actual !== null, "assert_object_equals", description,
                                                               "value is ${actual}, expected object",
                                                               {actual: actual});
         //This needs to be improved a great deal
         function check_equal(actual, expected, stack)
         {
             stack.push(actual);

             var p;
             for (p in actual) {
                 assert(expected.hasOwnProperty(p), "assert_object_equals", description,
                                                    "unexpected property ${p}", {p:p});

                 if (typeof actual[p] === "object" && actual[p] !== null) {
                     if (stack.indexOf(actual[p]) === -1) {
                         check_equal(actual[p], expected[p], stack);
                     }
                 } else {
                     assert(same_value(actual[p], expected[p]), "assert_object_equals", description,
                                                       "property ${p} expected ${expected} got ${actual}",
                                                       {p:p, expected:expected[p], actual:actual[p]});
                 }
             }
             for (p in expected) {
                 assert(actual.hasOwnProperty(p),
                        "assert_object_equals", description,
                        "expected property ${p} missing", {p:p});
             }
             stack.pop();
         }
         check_equal(actual, expected, []);
    }
    expose_assert(assert_object_equals, "assert_object_equals");

    /**
     * Assert that ``actual`` and ``expected`` are both arrays, and that the array properties of
     * ``actual`` and ``expected`` are all the same value (as for :js:func:`assert_equals`).
     *
     * @param {Array} actual - Test array.
     * @param {Array} expected - Array that is expected to contain the same values as ``actual``.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_array_equals(actual, expected, description)
    {
        const max_array_length = 20;
        function shorten_array(arr, offset = 0) {
            // Make ", …" only show up when it would likely reduce the length, not accounting for
            // fonts.
            if (arr.length < max_array_length + 2) {
                return arr;
            }
            // By default we want half the elements after the offset and half before
            // But if that takes us past the end of the array, we have more before, and
            // if it takes us before the start we have more after.
            const length_after_offset = Math.floor(max_array_length / 2);
            let upper_bound = Math.min(length_after_offset + offset, arr.length);
            const lower_bound = Math.max(upper_bound - max_array_length, 0);

            if (lower_bound === 0) {
                upper_bound = max_array_length;
            }

            const output = arr.slice(lower_bound, upper_bound);
            if (lower_bound > 0) {
                output.beginEllipsis = true;
            }
            if (upper_bound < arr.length) {
                output.endEllipsis = true;
            }
            return output;
        }

        assert(typeof actual === "object" && actual !== null && "length" in actual,
               "assert_array_equals", description,
               "value is ${actual}, expected array",
               {actual:actual});
        assert(actual.length === expected.length,
               "assert_array_equals", description,
               "lengths differ, expected array ${expected} length ${expectedLength}, got ${actual} length ${actualLength}",
               {expected:shorten_array(expected, expected.length - 1), expectedLength:expected.length,
                actual:shorten_array(actual, actual.length - 1), actualLength:actual.length
               });

        for (var i = 0; i < actual.length; i++) {
            assert(actual.hasOwnProperty(i) === expected.hasOwnProperty(i),
                   "assert_array_equals", description,
                   "expected property ${i} to be ${expected} but was ${actual} (expected array ${arrayExpected} got ${arrayActual})",
                   {i:i, expected:expected.hasOwnProperty(i) ? "present" : "missing",
                    actual:actual.hasOwnProperty(i) ? "present" : "missing",
                    arrayExpected:shorten_array(expected, i), arrayActual:shorten_array(actual, i)});
            assert(same_value(expected[i], actual[i]),
                   "assert_array_equals", description,
                   "expected property ${i} to be ${expected} but got ${actual} (expected array ${arrayExpected} got ${arrayActual})",
                   {i:i, expected:expected[i], actual:actual[i],
                    arrayExpected:shorten_array(expected, i), arrayActual:shorten_array(actual, i)});
        }
    }
    expose_assert(assert_array_equals, "assert_array_equals");

    /**
     * Assert that each array property in ``actual`` is a number within
     * ± `epsilon` of the corresponding property in `expected`.
     *
     * @param {Array} actual - Array of test values.
     * @param {Array} expected - Array of values expected to be close to the values in ``actual``.
     * @param {number} epsilon - Magnitude of allowed difference
     * between each value in ``actual`` and ``expected``.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_array_approx_equals(actual, expected, epsilon, description)
    {
        /*
         * Test if two primitive arrays are equal within +/- epsilon
         */
        assert(actual.length === expected.length,
               "assert_array_approx_equals", description,
               "lengths differ, expected ${expected} got ${actual}",
               {expected:expected.length, actual:actual.length});

        for (var i = 0; i < actual.length; i++) {
            assert(actual.hasOwnProperty(i) === expected.hasOwnProperty(i),
                   "assert_array_approx_equals", description,
                   "property ${i}, property expected to be ${expected} but was ${actual}",
                   {i:i, expected:expected.hasOwnProperty(i) ? "present" : "missing",
                    actual:actual.hasOwnProperty(i) ? "present" : "missing"});
            assert(typeof actual[i] === "number",
                   "assert_array_approx_equals", description,
                   "property ${i}, expected a number but got a ${type_actual}",
                   {i:i, type_actual:typeof actual[i]});
            assert(Math.abs(actual[i] - expected[i]) <= epsilon,
                   "assert_array_approx_equals", description,
                   "property ${i}, expected ${expected} +/- ${epsilon}, expected ${expected} but got ${actual}",
                   {i:i, expected:expected[i], actual:actual[i], epsilon:epsilon});
        }
    }
    expose_assert(assert_array_approx_equals, "assert_array_approx_equals");

    /**
     * Assert that ``actual`` is within ± ``epsilon`` of ``expected``.
     *
     * @param {number} actual - Test value.
     * @param {number} expected - Value number is expected to be close to.
     * @param {number} epsilon - Magnitude of allowed difference between ``actual`` and ``expected``.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_approx_equals(actual, expected, epsilon, description)
    {
        /*
         * Test if two primitive numbers are equal within +/- epsilon
         */
        assert(typeof actual === "number",
               "assert_approx_equals", description,
               "expected a number but got a ${type_actual}",
               {type_actual:typeof actual});

        // The epsilon math below does not place nice with NaN and Infinity
        // But in this case Infinity = Infinity and NaN = NaN
        if (isFinite(actual) || isFinite(expected)) {
            assert(Math.abs(actual - expected) <= epsilon,
                   "assert_approx_equals", description,
                   "expected ${expected} +/- ${epsilon} but got ${actual}",
                   {expected:expected, actual:actual, epsilon:epsilon});
        } else {
            assert_equals(actual, expected);
        }
    }
    expose_assert(assert_approx_equals, "assert_approx_equals");

    /**
     * Assert that ``actual`` is a number less than ``expected``.
     *
     * @param {number|bigint} actual - Test value.
     * @param {number|bigint} expected - Value that ``actual`` must be less than.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_less_than(actual, expected, description)
    {
        /*
         * Test if a primitive number (or bigint) is less than another
         */
        assert(typeof actual === "number" || typeof actual === "bigint",
               "assert_less_than", description,
               "expected a number but got a ${type_actual}",
               {type_actual:typeof actual});

        assert(typeof actual === typeof expected,
               "assert_less_than", description,
               "expected a ${type_expected} but got a ${type_actual}",
               {type_expected:typeof expected, type_actual:typeof actual});

        assert(actual < expected,
               "assert_less_than", description,
               "expected a number less than ${expected} but got ${actual}",
               {expected:expected, actual:actual});
    }
    expose_assert(assert_less_than, "assert_less_than");

    /**
     * Assert that ``actual`` is a number greater than ``expected``.
     *
     * @param {number|bigint} actual - Test value.
     * @param {number|bigint} expected - Value that ``actual`` must be greater than.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_greater_than(actual, expected, description)
    {
        /*
         * Test if a primitive number (or bigint) is greater than another
         */
        assert(typeof actual === "number" || typeof actual === "bigint",
               "assert_greater_than", description,
               "expected a number but got a ${type_actual}",
               {type_actual:typeof actual});

        assert(typeof actual === typeof expected,
               "assert_greater_than", description,
               "expected a ${type_expected} but got a ${type_actual}",
               {type_expected:typeof expected, type_actual:typeof actual});

        assert(actual > expected,
               "assert_greater_than", description,
               "expected a number greater than ${expected} but got ${actual}",
               {expected:expected, actual:actual});
    }
    expose_assert(assert_greater_than, "assert_greater_than");

    /**
     * Assert that ``actual`` is a number greater than ``lower`` and less
     * than ``upper`` but not equal to either.
     *
     * @param {number|bigint} actual - Test value.
     * @param {number|bigint} lower - Value that ``actual`` must be greater than.
     * @param {number|bigint} upper - Value that ``actual`` must be less than.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_between_exclusive(actual, lower, upper, description)
    {
        /*
         * Test if a primitive number (or bigint) is between two others
         */
        assert(typeof lower === typeof upper,
               "assert_between_exclusive", description,
               "expected lower (${type_lower}) and upper (${type_upper}) types to match (test error)",
               {type_lower:typeof lower, type_upper:typeof upper});

        assert(typeof actual === "number" || typeof actual === "bigint",
               "assert_between_exclusive", description,
               "expected a number but got a ${type_actual}",
               {type_actual:typeof actual});

        assert(typeof actual === typeof lower,
               "assert_between_exclusive", description,
               "expected a ${type_lower} but got a ${type_actual}",
               {type_lower:typeof lower, type_actual:typeof actual});

        assert(actual > lower && actual < upper,
               "assert_between_exclusive", description,
               "expected a number greater than ${lower} " +
               "and less than ${upper} but got ${actual}",
               {lower:lower, upper:upper, actual:actual});
    }
    expose_assert(assert_between_exclusive, "assert_between_exclusive");

    /**
     * Assert that ``actual`` is a number less than or equal to ``expected``.
     *
     * @param {number|bigint} actual - Test value.
     * @param {number|bigint} expected - Value that ``actual`` must be less
     * than or equal to.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_less_than_equal(actual, expected, description)
    {
        /*
         * Test if a primitive number (or bigint) is less than or equal to another
         */
        assert(typeof actual === "number" || typeof actual === "bigint",
               "assert_less_than_equal", description,
               "expected a number but got a ${type_actual}",
               {type_actual:typeof actual});

        assert(typeof actual === typeof expected,
               "assert_less_than_equal", description,
               "expected a ${type_expected} but got a ${type_actual}",
               {type_expected:typeof expected, type_actual:typeof actual});

        assert(actual <= expected,
               "assert_less_than_equal", description,
               "expected a number less than or equal to ${expected} but got ${actual}",
               {expected:expected, actual:actual});
    }
    expose_assert(assert_less_than_equal, "assert_less_than_equal");

    /**
     * Assert that ``actual`` is a number greater than or equal to ``expected``.
     *
     * @param {number|bigint} actual - Test value.
     * @param {number|bigint} expected - Value that ``actual`` must be greater
     * than or equal to.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_greater_than_equal(actual, expected, description)
    {
        /*
         * Test if a primitive number (or bigint) is greater than or equal to another
         */
        assert(typeof actual === "number" || typeof actual === "bigint",
               "assert_greater_than_equal", description,
               "expected a number but got a ${type_actual}",
               {type_actual:typeof actual});

        assert(typeof actual === typeof expected,
               "assert_greater_than_equal", description,
               "expected a ${type_expected} but got a ${type_actual}",
               {type_expected:typeof expected, type_actual:typeof actual});

        assert(actual >= expected,
               "assert_greater_than_equal", description,
               "expected a number greater than or equal to ${expected} but got ${actual}",
               {expected:expected, actual:actual});
    }
    expose_assert(assert_greater_than_equal, "assert_greater_than_equal");

    /**
     * Assert that ``actual`` is a number greater than or equal to ``lower`` and less
     * than or equal to ``upper``.
     *
     * @param {number|bigint} actual - Test value.
     * @param {number|bigint} lower - Value that ``actual`` must be greater than or equal to.
     * @param {number|bigint} upper - Value that ``actual`` must be less than or equal to.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_between_inclusive(actual, lower, upper, description)
    {
        /*
         * Test if a primitive number (or bigint) is between to two others or equal to either of them
         */
        assert(typeof lower === typeof upper,
               "assert_between_inclusive", description,
               "expected lower (${type_lower}) and upper (${type_upper}) types to match (test error)",
               {type_lower:typeof lower, type_upper:typeof upper});

        assert(typeof actual === "number" || typeof actual === "bigint",
               "assert_between_inclusive", description,
               "expected a number but got a ${type_actual}",
               {type_actual:typeof actual});

        assert(typeof actual === typeof lower,
               "assert_between_inclusive", description,
               "expected a ${type_lower} but got a ${type_actual}",
               {type_lower:typeof lower, type_actual:typeof actual});

        assert(actual >= lower && actual <= upper,
               "assert_between_inclusive", description,
               "expected a number greater than or equal to ${lower} " +
               "and less than or equal to ${upper} but got ${actual}",
               {lower:lower, upper:upper, actual:actual});
    }
    expose_assert(assert_between_inclusive, "assert_between_inclusive");

    /**
     * Assert that the class string of ``object`` as returned in
     * ``Object.prototype.toString`` is equal to ``class_name``.
     *
     * @param {Object} object - Object to stringify.
     * @param {string} class_string - Expected class string for ``object``.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_class_string(object, class_string, description) {
        var actual = {}.toString.call(object);
        var expected = "[object " + class_string + "]";
        assert(same_value(actual, expected), "assert_class_string", description,
                                             "expected ${expected} but got ${actual}",
                                             {expected:expected, actual:actual});
    }
    expose_assert(assert_class_string, "assert_class_string");

    /**
     * Assert that ``object`` has an own property with name ``property_name``.
     *
     * @param {Object} object - Object that should have the given property.
     * @param {string} property_name - Expected property name.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_own_property(object, property_name, description) {
        assert(object.hasOwnProperty(property_name),
               "assert_own_property", description,
               "expected property ${p} missing", {p:property_name});
    }
    expose_assert(assert_own_property, "assert_own_property");

    /**
     * Assert that ``object`` does not have an own property with name ``property_name``.
     *
     * @param {Object} object - Object that should not have the given property.
     * @param {string} property_name - Property name to test.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_not_own_property(object, property_name, description) {
        assert(!object.hasOwnProperty(property_name),
               "assert_not_own_property", description,
               "unexpected property ${p} is found on object", {p:property_name});
    }
    expose_assert(assert_not_own_property, "assert_not_own_property");

    function _assert_inherits(name) {
        return function (object, property_name, description)
        {
            assert((typeof object === "object" && object !== null) ||
                   typeof object === "function" ||
                   // Or has [[IsHTMLDDA]] slot
                   String(object) === "[object HTMLAllCollection]",
                   name, description,
                   "provided value is not an object");

            assert("hasOwnProperty" in object,
                   name, description,
                   "provided value is an object but has no hasOwnProperty method");

            assert(!object.hasOwnProperty(property_name),
                   name, description,
                   "property ${p} found on object expected in prototype chain",
                   {p:property_name});

            assert(property_name in object,
                   name, description,
                   "property ${p} not found in prototype chain",
                   {p:property_name});
        };
    }

    /**
     * Assert that ``object`` does not have an own property with name
     * ``property_name``, but inherits one through the prototype chain.
     *
     * @param {Object} object - Object that should have the given property in its prototype chain.
     * @param {string} property_name - Expected property name.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_inherits(object, property_name, description) {
        return _assert_inherits("assert_inherits")(object, property_name, description);
    }
    expose_assert(assert_inherits, "assert_inherits");

    /**
     * Alias for :js:func:`insert_inherits`.
     *
     * @param {Object} object - Object that should have the given property in its prototype chain.
     * @param {string} property_name - Expected property name.
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_idl_attribute(object, property_name, description) {
        return _assert_inherits("assert_idl_attribute")(object, property_name, description);
    }
    expose_assert(assert_idl_attribute, "assert_idl_attribute");

    /**
     * Assert a JS Error with the expected constructor is thrown.
     *
     * @param {object} constructor The expected exception constructor.
     * @param {Function} func Function which should throw.
     * @param {string} [description] Error description for the case that the error is not thrown.
     */
    function assert_throws_js(constructor, func, description)
    {
        assert_throws_js_impl(constructor, func, description,
                              "assert_throws_js");
    }
    expose_assert(assert_throws_js, "assert_throws_js");

    /**
     * Like assert_throws_js but allows specifying the assertion type
     * (assert_throws_js or promise_rejects_js, in practice).
     */
    function assert_throws_js_impl(constructor, func, description,
                                   assertion_type)
    {
        try {
            func.call(this);
            assert(false, assertion_type, description,
                   "${func} did not throw", {func:func});
        } catch (e) {
            if (e instanceof AssertionError) {
                throw e;
            }

            // Basic sanity-checks on the thrown exception.
            assert(typeof e === "object",
                   assertion_type, description,
                   "${func} threw ${e} with type ${type}, not an object",
                   {func:func, e:e, type:typeof e});

            assert(e !== null,
                   assertion_type, description,
                   "${func} threw null, not an object",
                   {func:func});

            // Basic sanity-check on the passed-in constructor
            assert(typeof constructor === "function",
                   assertion_type, description,
                   "${constructor} is not a constructor",
                   {constructor:constructor});
            var obj = constructor;
            while (obj) {
                if (typeof obj === "function" &&
                    obj.name === "Error") {
                    break;
                }
                obj = Object.getPrototypeOf(obj);
            }
            assert(obj != null,
                   assertion_type, description,
                   "${constructor} is not an Error subtype",
                   {constructor:constructor});

            // And checking that our exception is reasonable
            assert(e.constructor === constructor &&
                   e.name === constructor.name,
                   assertion_type, description,
                   "${func} threw ${actual} (${actual_name}) expected instance of ${expected} (${expected_name})",
                   {func:func, actual:e, actual_name:e.name,
                    expected:constructor,
                    expected_name:constructor.name});
        }
    }

    /**
     * Asserts if called. Used to ensure that a specific codepath is
     * not taken e.g. that an error event isn't fired.
     *
     * @param {string} [description] - Description of the condition being tested.
     */
    function assert_unreached(description) {
         assert(false, "assert_unreached", description,
                "Reached unreachable code");
    }
    expose_assert(assert_unreached, "assert_unreached");

    /**
     * @callback AssertFunc
     * @param {Any} actual
     * @param {Any} expected
     * @param {Any[]} args
     */

    /**
     * Asserts that ``actual`` matches at least one value of ``expected``
     * according to a comparison defined by ``assert_func``.
     *
     * Note that tests with multiple allowed pass conditions are bad
     * practice unless the spec specifically allows multiple
     * behaviours. Test authors should not use this method simply to
     * hide UA bugs.
     *
     * @param {AssertFunc} assert_func - Function to compare actual
     * and expected. It must throw when the comparison fails and
     * return when the comparison passes.
     * @param {Any} actual - Test value.
     * @param {Array} expected_array - Array of possible expected values.
     * @param {Any[]} args - Additional arguments to pass to ``assert_func``.
     */
    function assert_any(assert_func, actual, expected_array, ...args)
    {
        var errors = [];
        var passed = false;
        forEach(expected_array,
                function(expected)
                {
                    try {
                        assert_func.apply(this, [actual, expected].concat(args));
                        passed = true;
                    } catch (e) {
                        errors.push(e.message);
                    }
                });
        if (!passed) {
            throw new AssertionError(errors.join("\n\n"));
        }
    }
    // FIXME: assert_any cannot use expose_assert, because assert_wrapper does
    // not support nested assert calls (e.g. to assert_func). We need to
    // support bypassing assert_wrapper for the inner asserts here.
    expose(assert_any, "assert_any");

    /**
     * Assert that a feature is implemented, based on a 'truthy' condition.
     *
     * This function should be used to early-exit from tests in which there is
     * no point continuing without support for a non-optional spec or spec
     * feature. For example:
     *
     *     assert_implements(window.Foo, 'Foo is not supported');
     *
     * @param {object} condition The truthy value to test
     * @param {string} [description] Error description for the case that the condition is not truthy.
     */
    function assert_implements(condition, description) {
        assert(!!condition, "assert_implements", description);
    }
    expose_assert(assert_implements, "assert_implements");

    /**
     * Assert that an optional feature is implemented, based on a 'truthy' condition.
     *
     * This function should be used to early-exit from tests in which there is
     * no point continuing without support for an explicitly optional spec or
     * spec feature. For example:
     *
     *     assert_implements_optional(video.canPlayType("video/webm"),
     *                                "webm video playback not supported");
     *
     * @param {object} condition The truthy value to test
     * @param {string} [description] Error description for the case that the condition is not truthy.
     */
    function assert_implements_optional(condition, description) {
        if (!condition) {
            throw new OptionalFeatureUnsupportedError(description);
        }
    }
    expose_assert(assert_implements_optional, "assert_implements_optional");

    /**
     * @class
     *
     * A single subtest. A Test is not constructed directly but via the
     * :js:func:`test`, :js:func:`async_test` or :js:func:`promise_test` functions.
     *
     * @param {string} name - This must be unique in a given file and must be
     * invariant between runs.
     *
     */
    function Test(name, properties)
    {
        if (tests.file_is_test && tests.tests.length) {
            throw new Error("Tried to create a test with file_is_test");
        }
        /** The test name. */
        this.name = name;

        this.phase = (tests.is_aborted || tests.phase === tests.phases.COMPLETE) ?
            this.phases.COMPLETE : this.phases.INITIAL;

        /** The test status code.*/
        this.status = this.NOTRUN;
        this.timeout_id = null;
        this.index = null;

        this.properties = properties || {};
        this.timeout_length = settings.test_timeout;
        if (this.timeout_length !== null) {
            this.timeout_length *= tests.timeout_multiplier;
        }

        /** A message indicating the reason for test failure. */
        this.message = null;
        /** Stack trace in case of failure. */
        this.stack = null;

        this.steps = [];
        this._is_promise_test = false;

        this.cleanup_callbacks = [];
        this._user_defined_cleanup_count = 0;
        this._done_callbacks = [];

        if (typeof AbortController === "function") {
            this._abortController = new AbortController();
        }

        // Tests declared following harness completion are likely an indication
        // of a programming error, but they cannot be reported
        // deterministically.
        if (tests.phase === tests.phases.COMPLETE) {
            return;
        }

        tests.push(this);
    }

    /**
     * Enum of possible test statuses.
     *
     * :values:
     *   - ``PASS``
     *   - ``FAIL``
     *   - ``TIMEOUT``
     *   - ``NOTRUN``
     *   - ``PRECONDITION_FAILED``
     */
    Test.statuses = {
        PASS:0,
        FAIL:1,
        TIMEOUT:2,
        NOTRUN:3,
        PRECONDITION_FAILED:4
    };

    Test.prototype = merge({}, Test.statuses);

    Test.prototype.phases = {
        INITIAL:0,
        STARTED:1,
        HAS_RESULT:2,
        CLEANING:3,
        COMPLETE:4
    };

    Test.prototype.status_formats = {
        0: "Pass",
        1: "Fail",
        2: "Timeout",
        3: "Not Run",
        4: "Optional Feature Unsupported",
    };

    Test.prototype.format_status = function() {
        return this.status_formats[this.status];
    };

    Test.prototype.structured_clone = function()
    {
        if (!this._structured_clone) {
            var msg = this.message;
            msg = msg ? String(msg) : msg;
            this._structured_clone = merge({
                name:String(this.name),
                properties:merge({}, this.properties),
                phases:merge({}, this.phases)
            }, Test.statuses);
        }
        this._structured_clone.status = this.status;
        this._structured_clone.message = this.message;
        this._structured_clone.stack = this.stack;
        this._structured_clone.index = this.index;
        this._structured_clone.phase = this.phase;
        return this._structured_clone;
    };

    /**
     * Run a single step of an ongoing test.
     *
     * @param {string} func - Callback function to run as a step. If
     * this throws an :js:func:`AssertionError`, or any other
     * exception, the :js:class:`Test` status is set to ``FAIL``.
     * @param {Object} [this_obj] - The object to use as the this
     * value when calling ``func``. Defaults to the  :js:class:`Test` object.
     */
    Test.prototype.step = function(func, this_obj)
    {
        if (this.phase > this.phases.STARTED) {
            return;
        }

        if (settings.debug && this.phase !== this.phases.STARTED) {
            print("TEST START", this.name);
        }
        this.phase = this.phases.STARTED;
        //If we don't get a result before the harness times out that will be a test timeout
        this.set_status(this.TIMEOUT, "Test timed out");

        tests.started = true;
        tests.current_test = this;
        tests.notify_test_state(this);

        if (this.timeout_id === null) {
            this.set_timeout();
        }

        this.steps.push(func);

        if (arguments.length === 1) {
            this_obj = this;
        }

        if (settings.debug) {
            console.debug("TEST STEP", this.name);
        }

        try {
            return func.apply(this_obj, Array.prototype.slice.call(arguments, 2));
        } catch (e) {
            if (this.phase >= this.phases.HAS_RESULT) {
                return;
            }
            var status = e instanceof OptionalFeatureUnsupportedError ? this.PRECONDITION_FAILED : this.FAIL;
            var message = String((typeof e === "object" && e !== null) ? e.message : e);
            var stack = e.stack ? e.stack : null;

            this.set_status(status, message, stack);
            this.phase = this.phases.HAS_RESULT;
            this.done();
        } finally {
            this.current_test = null;
        }
    };

    /**
     * Wrap a function so that it runs as a step of the current test.
     *
     * This allows creating a callback function that will run as a
     * test step.
     *
     * @example
     * let t = async_test("Example");
     * onload = t.step_func(e => {
     *   assert_equals(e.name, "load");
     *   // Mark the test as complete.
     *   t.done();
     * })
     *
     * @param {string} func - Function to run as a step. If this
     * throws an :js:func:`AssertionError`, or any other exception,
     * the :js:class:`Test` status is set to ``FAIL``.
     * @param {Object} [this_obj] - The object to use as the this
     * value when calling ``func``. Defaults to the :js:class:`Test` object.
     */
    Test.prototype.step_func = function(func, this_obj)
    {
        var test_this = this;

        if (arguments.length === 1) {
            this_obj = test_this;
        }

        return function()
        {
            return test_this.step.apply(test_this, [func, this_obj].concat(
                Array.prototype.slice.call(arguments)));
        };
    };

    /**
     * Wrap a function so that it runs as a step of the current test,
     * and automatically marks the test as complete if the function
     * returns without error.
     *
     * @param {string} func - Function to run as a step. If this
     * throws an :js:func:`AssertionError`, or any other exception,
     * the :js:class:`Test` status is set to ``FAIL``. If it returns
     * without error the status is set to ``PASS``.
     * @param {Object} [this_obj] - The object to use as the this
     * value when calling `func`. Defaults to the :js:class:`Test` object.
     */
    Test.prototype.step_func_done = function(func, this_obj)
    {
        var test_this = this;

        if (arguments.length === 1) {
            this_obj = test_this;
        }

        return function()
        {
            if (func) {
                test_this.step.apply(test_this, [func, this_obj].concat(
                    Array.prototype.slice.call(arguments)));
            }
            test_this.done();
        };
    };

    /**
     * Return a function that automatically sets the current test to
     * ``FAIL`` if it's called.
     *
     * @param {string} [description] - Error message to add to assert
     * in case of failure.
     *
     */
    Test.prototype.unreached_func = function(description)
    {
        return this.step_func(function() {
            assert_unreached(description);
        });
    };

    /**
     * Run a function as a step of the test after a given timeout.
     *
     * This multiplies the timeout by the global timeout multiplier to
     * account for the expected execution speed of the current test
     * environment. For example ``test.step_timeout(f, 2000)`` with a
     * timeout multiplier of 2 will wait for 4000ms before calling ``f``.
     *
     * In general it's encouraged to use :js:func:`Test.step_wait` or
     * :js:func:`step_wait_func` in preference to this function where possible,
     * as they provide better test performance.
     *
     * @param {Function} func - Function to run as a test
     * step.
     * @param {number} timeout - Time in ms to wait before running the
     * test step. The actual wait time is ``timeout`` x
     * ``timeout_multiplier``.
     *
     */
    Test.prototype.step_timeout = function(func, timeout) {
        var test_this = this;
        var args = Array.prototype.slice.call(arguments, 2);
        var local_set_timeout = typeof global_scope.setTimeout === "undefined" ? fake_set_timeout : setTimeout;
        return local_set_timeout(this.step_func(function() {
            return func.apply(test_this, args);
        }), timeout * tests.timeout_multiplier);
    };

    /**
     * Poll for a function to return true, and call a callback
     * function once it does, or assert if a timeout is
     * reached. This is preferred over a simple step_timeout
     * whenever possible since it allows the timeout to be longer
     * to reduce intermittents without compromising test execution
     * speed when the condition is quickly met.
     *
     * @param {Function} cond A function taking no arguments and
     *                        returning a boolean or a Promise. The callback is
     *                        called when this function returns true, or the
     *                        returned Promise is resolved with true.
     * @param {Function} func A function taking no arguments to call once
     *                        the condition is met.
     * @param {string} [description] Error message to add to assert in case of
     *                               failure.
     * @param {number} timeout Timeout in ms. This is multiplied by the global
     *                         timeout_multiplier
     * @param {number} interval Polling interval in ms
     *
     */
    Test.prototype.step_wait_func = function(cond, func, description,
                                             timeout=3000, interval=100) {
        var timeout_full = timeout * tests.timeout_multiplier;
        var remaining = Math.ceil(timeout_full / interval);
        var test_this = this;
        var local_set_timeout = typeof global_scope.setTimeout === 'undefined' ? fake_set_timeout : setTimeout;

        const step = test_this.step_func((result) => {
            if (result) {
                func();
            } else {
                if (remaining === 0) {
                    assert(false, "step_wait_func", description,
                           "Timed out waiting on condition");
                }
                remaining--;
                local_set_timeout(wait_for_inner, interval);
            }
        });

        var wait_for_inner = test_this.step_func(() => {
            Promise.resolve(cond()).then(
                step,
                test_this.unreached_func("step_wait_func"));
        });

        wait_for_inner();
    };

    /**
     * Poll for a function to return true, and invoke a callback
     * followed by this.done() once it does, or assert if a timeout
     * is reached. This is preferred over a simple step_timeout
     * whenever possible since it allows the timeout to be longer
     * to reduce intermittents without compromising test execution speed
     * when the condition is quickly met.
     *
     * @example
     * async_test(t => {
     *  const popup = window.open("resources/coop-coep.py?coop=same-origin&coep=&navigate=about:blank");
     *  t.add_cleanup(() => popup.close());
     *  assert_equals(window, popup.opener);
     *
     *  popup.onload = t.step_func(() => {
     *    assert_true(popup.location.href.endsWith("&navigate=about:blank"));
     *    // Use step_wait_func_done as about:blank cannot message back.
     *    t.step_wait_func_done(() => popup.location.href === "about:blank");
     *  });
     * }, "Navigating a popup to about:blank");
     *
     * @param {Function} cond A function taking no arguments and
     *                        returning a boolean or a Promise. The callback is
     *                        called when this function returns true, or the
     *                        returned Promise is resolved with true.
     * @param {Function} func A function taking no arguments to call once
     *                        the condition is met.
     * @param {string} [description] Error message to add to assert in case of
     *                               failure.
     * @param {number} timeout Timeout in ms. This is multiplied by the global
     *                         timeout_multiplier
     * @param {number} interval Polling interval in ms
     *
     */
    Test.prototype.step_wait_func_done = function(cond, func, description,
                                                  timeout=3000, interval=100) {
         this.step_wait_func(cond, () => {
            if (func) {
                func();
            }
            this.done();
         }, description, timeout, interval);
    };

    /**
     * Poll for a function to return true, and resolve a promise
     * once it does, or assert if a timeout is reached. This is
     * preferred over a simple step_timeout whenever possible
     * since it allows the timeout to be longer to reduce
     * intermittents without compromising test execution speed
     * when the condition is quickly met.
     *
     * @example
     * promise_test(async t => {
     *  // …
     * await t.step_wait(() => frame.contentDocument === null, "Frame navigated to a cross-origin document");
     * // …
     * }, "");
     *
     * @param {Function} cond A function taking no arguments and
     *                        returning a boolean or a Promise.
     * @param {string} [description] Error message to add to assert in case of
     *                              failure.
     * @param {number} timeout Timeout in ms. This is multiplied by the global
     *                         timeout_multiplier
     * @param {number} interval Polling interval in ms
     * @returns {Promise} Promise resolved once cond is met.
     *
     */
    Test.prototype.step_wait = function(cond, description, timeout=3000, interval=100) {
        return new Promise(resolve => {
            this.step_wait_func(cond, resolve, description, timeout, interval);
        });
    };

    /*
     * Private method for registering cleanup functions. `testharness.js`
     * internals should use this method instead of the public `add_cleanup`
     * method in order to hide implementation details from the harness status
     * message in the case errors.
     */
    Test.prototype._add_cleanup = function(callback) {
        this.cleanup_callbacks.push(callback);
    };

    /**
     * Schedule a function to be run after the test result is known, regardless
     * of passing or failing state.
     *
     * The behavior of this function will not
     * influence the result of the test, but if an exception is thrown, the
     * test harness will report an error.
     *
     * @param {Function} callback - The cleanup function to run. This
     * is called with no arguments.
     */
    Test.prototype.add_cleanup = function(callback) {
        this._user_defined_cleanup_count += 1;
        this._add_cleanup(callback);
    };

    Test.prototype.set_timeout = function()
    {
        if (this.timeout_length !== null) {
            var this_obj = this;
            this.timeout_id = setTimeout(function()
                                         {
                                             this_obj.timeout();
                                         }, this.timeout_length);
        }
    };

    Test.prototype.set_status = function(status, message, stack)
    {
        this.status = status;
        this.message = message;
        this.stack = stack ? stack : null;
    };

    /**
     * Manually set the test status to ``TIMEOUT``.
     */
    Test.prototype.timeout = function()
    {
        this.timeout_id = null;
        this.set_status(this.TIMEOUT, "Test timed out");
        this.phase = this.phases.HAS_RESULT;
        this.done();
    };

    /**
     * Manually set the test status to ``TIMEOUT``.
     *
     * Alias for `Test.timeout <#Test.timeout>`_.
     */
    Test.prototype.force_timeout = function() {
        return this.timeout();
    };

    /**
     * Mark the test as complete.
     *
     * This sets the test status to ``PASS`` if no other status was
     * already recorded. Any subsequent attempts to run additional
     * test steps will be ignored.
     *
     * After setting the test status any test cleanup functions will
     * be run.
     */
    Test.prototype.done = function()
    {
        if (this.phase >= this.phases.CLEANING) {
            return;
        }

        if (this.phase <= this.phases.STARTED) {
            this.set_status(this.PASS, null);
        }

        if (global_scope.clearTimeout) {
            clearTimeout(this.timeout_id);
        }

        if (settings.debug) {
            print("TEST DONE",
                        this.status,
                        this.name);
        }

        this.cleanup();
    };

    function add_test_done_callback(test, callback)
    {
        if (test.phase === test.phases.COMPLETE) {
            callback();
            return;
        }

        test._done_callbacks.push(callback);
    }

    /*
     * Invoke all specified cleanup functions. If one or more produce an error,
     * the context is in an unpredictable state, so all further testing should
     * be cancelled.
     */
    Test.prototype.cleanup = function() {
        var errors = [];
        var bad_value_count = 0;
        function on_error(e) {
            errors.push(e);
            // Abort tests immediately so that tests declared within subsequent
            // cleanup functions are not run.
            tests.abort();
        }
        var this_obj = this;
        var results = [];

        this.phase = this.phases.CLEANING;

        if (this._abortController) {
            this._abortController.abort("Test cleanup");
        }

        forEach(this.cleanup_callbacks,
                function(cleanup_callback) {
                    var result;

                    try {
                        result = cleanup_callback();
                    } catch (e) {
                        on_error(e);
                        return;
                    }

                    if (!is_valid_cleanup_result(this_obj, result)) {
                        bad_value_count += 1;
                        // Abort tests immediately so that tests declared
                        // within subsequent cleanup functions are not run.
                        tests.abort();
                    }

                    results.push(result);
                });

        if (!this._is_promise_test) {
            cleanup_done(this_obj, errors, bad_value_count);
        } else {
            all_async(results,
                      function(result, done) {
                          if (result && typeof result.then === "function") {
                              result
                                  .then(null, on_error)
                                  .then(done);
                          } else {
                              done();
                          }
                      },
                      function() {
                          cleanup_done(this_obj, errors, bad_value_count);
                      });
        }
    };

    /*
     * Determine if the return value of a cleanup function is valid for a given
     * test. Any test may return the value `undefined`. Tests created with
     * `promise_test` may alternatively return "thenable" object values.
     */
    function is_valid_cleanup_result(test, result) {
        if (result === undefined) {
            return true;
        }

        if (test._is_promise_test) {
            return result && typeof result.then === "function";
        }

        return false;
    }

    function cleanup_done(test, errors, bad_value_count) {
        if (errors.length || bad_value_count) {
            var total = test._user_defined_cleanup_count;

            tests.status.status = tests.status.ERROR;
            tests.status.stack = null;
            tests.status.message = "Test named '" + test.name +
                "' specified " + total +
                " 'cleanup' function" + (total > 1 ? "s" : "");

            if (errors.length) {
                tests.status.message += ", and " + errors.length + " failed";
                tests.status.stack = ((typeof errors[0] === "object" &&
                                       errors[0].hasOwnProperty("stack")) ?
                                      errors[0].stack : null);
            }

            if (bad_value_count) {
                var type = test._is_promise_test ?
                   "non-thenable" : "non-undefined";
                tests.status.message += ", and " + bad_value_count +
                    " returned a " + type + " value";
            }

            tests.status.message += ".";
        }

        test.phase = test.phases.COMPLETE;
        tests.result(test);
        forEach(test._done_callbacks,
                function(callback) {
                    callback();
                });
        test._done_callbacks.length = 0;
    }

    /**
     * Gives an AbortSignal that will be aborted when the test finishes.
     */
    Test.prototype.get_signal = function() {
        if (!this._abortController) {
            throw new Error("AbortController is not supported in this browser");
        }
        return this._abortController.signal;
    };

    /**
     * @class
     * Status of the overall harness
     */
    function TestsStatus()
    {
        /** The status code */
        this.status = null;
        /** Message in case of failure */
        this.message = null;
        /** Stack trace in case of an exception. */
        this.stack = null;
    }

    /**
     * Enum of possible harness statuses.
     *
     * :values:
     *   - ``OK``
     *   - ``ERROR``
     *   - ``TIMEOUT``
     *   - ``PRECONDITION_FAILED``
     */
    TestsStatus.statuses = {
        OK:0,
        ERROR:1,
        TIMEOUT:2,
        PRECONDITION_FAILED:3
    };

    TestsStatus.prototype = merge({}, TestsStatus.statuses);

    TestsStatus.prototype.formats = {
        0: "OK",
        1: "Error",
        2: "Timeout",
        3: "Optional Feature Unsupported"
    };

    TestsStatus.prototype.structured_clone = function()
    {
        if (!this._structured_clone) {
            var msg = this.message;
            msg = msg ? String(msg) : msg;
            this._structured_clone = merge({
                status:this.status,
                message:msg,
                stack:this.stack
            }, TestsStatus.statuses);
        }
        return this._structured_clone;
    };

    TestsStatus.prototype.format_status = function() {
        return this.formats[this.status];
    };

    function Tests()
    {
        this.tests = [];
        this.num_pending = 0;

        this.phases = {
            INITIAL:0,
            SETUP:1,
            HAVE_TESTS:2,
            HAVE_RESULTS:3,
            COMPLETE:4
        };
        this.phase = this.phases.INITIAL;

        this.properties = {};

        this.wait_for_finish = false;
        this.processing_callbacks = false;

        this.allow_uncaught_exception = false;

        this.file_is_test = false;
        // This value is lazily initialized in order to avoid introducing a
        // dependency on ECMAScript 2015 Promises to all tests.
        this.promise_tests = null;
        this.promise_setup_called = false;

        this.timeout_multiplier = 1;
        this.timeout_length = test_environment.test_timeout();
        this.timeout_id = null;

        this.start_callbacks = [];
        this.test_state_callbacks = [];
        this.test_done_callbacks = [];
        this.all_done_callbacks = [];

        this.hide_test_state = false;
        this.remotes = [];

        this.current_test = null;
        this.asserts_run = [];

        // Track whether output is enabled, and thus whether or not we should
        // track asserts.
        //
        // On workers we don't get properties set from testharnessreport.js, so
        // we don't know whether or not to track asserts. To avoid the
        // resulting performance hit, we assume we are not meant to. This means
        // that assert tracking does not function on workers.
        this.output = settings.output && 'document' in global_scope;

        this.status = new TestsStatus();

        var this_obj = this;

        test_environment.add_on_loaded_callback(function() {
            if (this_obj.all_done()) {
                this_obj.complete();
            }
        });

        this.set_timeout();
    }

    Tests.prototype.setup = function(func, properties)
    {
        if (this.phase >= this.phases.HAVE_RESULTS) {
            return;
        }

        if (this.phase < this.phases.SETUP) {
            this.phase = this.phases.SETUP;
        }

        this.properties = properties;

        for (var p in properties) {
            if (properties.hasOwnProperty(p)) {
                var value = properties[p];
                if (p === "allow_uncaught_exception") {
                    this.allow_uncaught_exception = value;
                } else if (p === "explicit_done" && value) {
                    this.wait_for_finish = true;
                } else if (p === "explicit_timeout" && value) {
                    this.timeout_length = null;
                    if (this.timeout_id)
                    {
                        clearTimeout(this.timeout_id);
                    }
                } else if (p === "single_test" && value) {
                    this.set_file_is_test();
                } else if (p === "timeout_multiplier") {
                    this.timeout_multiplier = value;
                    if (this.timeout_length) {
                         this.timeout_length *= this.timeout_multiplier;
                    }
                } else if (p === "hide_test_state") {
                    this.hide_test_state = value;
                } else if (p === "output") {
                    this.output = value;
                } else if (p === "debug") {
                    settings.debug = value;
                }
            }
        }

        if (func) {
            try {
                func();
            } catch (e) {
                this.status.status = e instanceof OptionalFeatureUnsupportedError ? this.status.PRECONDITION_FAILED : this.status.ERROR;
                this.status.message = String(e);
                this.status.stack = e.stack ? e.stack : null;
                this.complete();
            }
        }
        this.set_timeout();
    };

    Tests.prototype.set_file_is_test = function() {
        if (this.tests.length > 0) {
            throw new Error("Tried to set file as test after creating a test");
        }
        this.wait_for_finish = true;
        this.file_is_test = true;
        // Create the test, which will add it to the list of tests
        tests.current_test = async_test();
    };

    Tests.prototype.set_status = function(status, message, stack)
    {
        this.status.status = status;
        this.status.message = message;
        this.status.stack = stack ? stack : null;
    };

    Tests.prototype.set_timeout = function() {
        if (global_scope.clearTimeout) {
            var this_obj = this;
            clearTimeout(this.timeout_id);
            if (this.timeout_length !== null) {
                this.timeout_id = setTimeout(function() {
                                                 this_obj.timeout();
                                             }, this.timeout_length);
            }
        }
    };

    Tests.prototype.timeout = function() {
        var test_in_cleanup = null;

        if (this.status.status === null) {
            forEach(this.tests,
                    function(test) {
                        // No more than one test is expected to be in the
                        // "CLEANUP" phase at any time
                        if (test.phase === test.phases.CLEANING) {
                            test_in_cleanup = test;
                        }

                        test.phase = test.phases.COMPLETE;
                    });

            // Timeouts that occur while a test is in the "cleanup" phase
            // indicate that some global state was not properly reverted. This
            // invalidates the overall test execution, so the timeout should be
            // reported as an error and cancel the execution of any remaining
            // tests.
            if (test_in_cleanup) {
                this.status.status = this.status.ERROR;
                this.status.message = "Timeout while running cleanup for " +
                    "test named \"" + test_in_cleanup.name + "\".";
                tests.status.stack = null;
            } else {
                this.status.status = this.status.TIMEOUT;
            }
        }

        this.complete();
    };

    Tests.prototype.end_wait = function()
    {
        this.wait_for_finish = false;
        if (this.all_done()) {
            this.complete();
        }
    };

    Tests.prototype.push = function(test)
    {
        if (this.phase === this.phases.COMPLETE) {
            return;
        }
        if (this.phase < this.phases.HAVE_TESTS) {
            this.start();
        }
        this.num_pending++;
        test.index = this.tests.push(test) - 1;
        this.notify_test_state(test);
    };

    Tests.prototype.notify_test_state = function(test) {
        var this_obj = this;
        forEach(this.test_state_callbacks,
                function(callback) {
                    callback(test, this_obj);
                });
    };

    Tests.prototype.all_done = function() {
        return (this.tests.length > 0 || this.remotes.length > 0) &&
                test_environment.all_loaded &&
                (this.num_pending === 0 || this.is_aborted) && !this.wait_for_finish &&
                !this.processing_callbacks &&
                !this.remotes.some(function(w) { return w.running; });
    };

    Tests.prototype.start = function() {
        this.phase = this.phases.HAVE_TESTS;
        this.notify_start();
    };

    Tests.prototype.notify_start = function() {
        var this_obj = this;
        forEach (this.start_callbacks,
                 function(callback)
                 {
                     callback(this_obj.properties);
                 });
    };

    Tests.prototype.result = function(test)
    {
        // If the harness has already transitioned beyond the `HAVE_RESULTS`
        // phase, subsequent tests should not cause it to revert.
        if (this.phase <= this.phases.HAVE_RESULTS) {
            this.phase = this.phases.HAVE_RESULTS;
        }
        this.num_pending--;
        this.notify_result(test);
    };

    Tests.prototype.notify_result = function(test) {
        var this_obj = this;
        this.processing_callbacks = true;
        forEach(this.test_done_callbacks,
                function(callback)
                {
                    callback(test, this_obj);
                });
        this.processing_callbacks = false;
        if (this_obj.all_done()) {
            this_obj.complete();
        }
    };

    Tests.prototype.complete = function() {
        if (this.phase === this.phases.COMPLETE) {
            return;
        }
        var this_obj = this;
        var all_complete = function() {
            this_obj.phase = this_obj.phases.COMPLETE;
            this_obj.notify_complete();
        };
        var incomplete = filter(this.tests,
                                function(test) {
                                    return test.phase < test.phases.COMPLETE;
                                });

        /**
         * To preserve legacy behavior, overall test completion must be
         * signaled synchronously.
         */
        if (incomplete.length === 0) {
            all_complete();
            return;
        }

        all_async(incomplete,
                  function(test, testDone)
                  {
                      if (test.phase === test.phases.INITIAL) {
                          test.phase = test.phases.HAS_RESULT;
                          test.done();
                          testDone();
                      } else {
                          add_test_done_callback(test, testDone);
                          test.cleanup();
                      }
                  },
                  all_complete);
    };

    Tests.prototype.set_assert = function(assert_name, args) {
        this.asserts_run.push(new AssertRecord(this.current_test, assert_name, args));
    };

    Tests.prototype.set_assert_status = function(index, status, stack) {
        let assert_record = this.asserts_run[index];
        assert_record.status = status;
        assert_record.stack = stack;
    };

    /**
     * Update the harness status to reflect an unrecoverable harness error that
     * should cancel all further testing. Update all previously-defined tests
     * which have not yet started to indicate that they will not be executed.
     */
    Tests.prototype.abort = function() {
        this.status.status = this.status.ERROR;
        this.is_aborted = true;

        forEach(this.tests,
                function(test) {
                    if (test.phase === test.phases.INITIAL) {
                        test.phase = test.phases.COMPLETE;
                    }
                });
    };

    /*
     * Determine if any tests share the same `name` property. Return an array
     * containing the names of any such duplicates.
     */
    Tests.prototype.find_duplicates = function() {
        var names = Object.create(null);
        var duplicates = [];

        forEach (this.tests,
                 function(test)
                 {
                     if (test.name in names && duplicates.indexOf(test.name) === -1) {
                        duplicates.push(test.name);
                     }
                     names[test.name] = true;
                 });

        return duplicates;
    };

    function code_unit_str(char) {
        return 'U+' + char.charCodeAt(0).toString(16);
    }

    function sanitize_unpaired_surrogates(str) {
        var out = "";
        for (var i = 0; i < str.length; i++) {
            var ch = str.charCodeAt(i);
    
            // High surrogate range
            if (0xD800 <= ch && ch <= 0xDBFF) {
                // If next exists and is low surrogate → valid pair
                if (i + 1 < str.length) {
                    var next = str.charCodeAt(i + 1);
                    if (0xDC00 <= next && next <= 0xDFFF) {
                        out += str[i] + str[i + 1]; // keep pair
                        i++; // skip the next
                        continue;
                    }
                }
                // Otherwise unpaired high surrogate
                out += code_unit_str(str[i]);
            }
            // Low surrogate range
            else if (0xDC00 <= ch && ch <= 0xDFFF) {
                // Unpaired low surrogate
                out += code_unit_str(str[i]);
            }
            else {
                // Normal BMP char
                out += str[i];
            }
        }
        return out;
    }


    function sanitize_all_unpaired_surrogates(tests) {
        forEach (tests,
                 function (test)
                 {
                     var sanitized = sanitize_unpaired_surrogates(test.name);

                     if (test.name !== sanitized) {
                         test.name = sanitized;
                         delete test._structured_clone;
                     }
                 });
    }

    Tests.prototype.notify_complete = function() {
        var this_obj = this;
        var duplicates;

        if (this.status.status === null) {
            duplicates = this.find_duplicates();

            // Some transports adhere to UTF-8's restriction on unpaired
            // surrogates. Sanitize the titles so that the results can be
            // consistently sent via all transports.
            sanitize_all_unpaired_surrogates(this.tests);

            // Test names are presumed to be unique within test files--this
            // allows consumers to use them for identification purposes.
            // Duplicated names violate this expectation and should therefore
            // be reported as an error.
            if (duplicates.length) {
                this.status.status = this.status.ERROR;
                this.status.message =
                   duplicates.length + ' duplicate test name' +
                   (duplicates.length > 1 ? 's' : '') + ': "' +
                   duplicates.join('", "') + '"';
            } else {
                this.status.status = this.status.OK;
            }
        }

        forEach (this.all_done_callbacks,
                 function(callback)
                 {
                     callback(this_obj.tests, this_obj.status, this_obj.asserts_run);
                 });
    };

    /**
     * Timeout the tests.
     *
     * This only has an effect when ``explicit_timeout`` has been set
     * in :js:func:`setup`. In other cases any call is a no-op.
     *
     */
    function timeout() {
        if (tests.timeout_length === null) {
            tests.timeout();
        }
    }
    expose(timeout, 'timeout');

    /**
     * Add a callback that's triggered when the first :js:class:`Test` is created.
     *
     * @param {Function} callback - Callback function. This is called
     * without arguments.
     */
    function add_start_callback(callback) {
        tests.start_callbacks.push(callback);
    }

    /**
     * Add a callback that's triggered when a test state changes.
     *
     * @param {Function} callback - Callback function, called with the
     * :js:class:`Test` as the only argument.
     */
    function add_test_state_callback(callback) {
        tests.test_state_callbacks.push(callback);
    }

    /**
     * Add a callback that's triggered when a test result is received.
     *
     * @param {Function} callback - Callback function, called with the
     * :js:class:`Test` as the only argument.
     */
    function add_result_callback(callback) {
        tests.test_done_callbacks.push(callback);
    }

    /**
     * Add a callback that's triggered when all tests are complete.
     *
     * @param {Function} callback - Callback function, called with an
     * array of :js:class:`Test` objects, a :js:class:`TestsStatus`
     * object and an array of :js:class:`AssertRecord` objects. If the
     * debug setting is ``false`` the final argument will be an empty
     * array.
     *
     * For performance reasons asserts are only tracked when the debug
     * setting is ``true``. In other cases the array of asserts will be
     * empty.
     */
    function add_completion_callback(callback) {
        tests.all_done_callbacks.push(callback);
    }

    expose(add_start_callback, 'add_start_callback');
    expose(add_test_state_callback, 'add_test_state_callback');
    expose(add_result_callback, 'add_result_callback');
    expose(add_completion_callback, 'add_completion_callback');

    function remove(array, item) {
        var index = array.indexOf(item);
        if (index > -1) {
            array.splice(index, 1);
        }
    }

    function remove_start_callback(callback) {
        remove(tests.start_callbacks, callback);
    }

    function remove_test_state_callback(callback) {
        remove(tests.test_state_callbacks, callback);
    }

    function remove_result_callback(callback) {
        remove(tests.test_done_callbacks, callback);
    }

    function remove_completion_callback(callback) {
       remove(tests.all_done_callbacks, callback);
    }

    /*
     * Utility functions
     */
    function assert(expected_true, function_name, description, error, substitutions)
    {
        if (expected_true !== true) {
            var msg = `${function_name}: ${description}${error}`
            throw new AssertionError(msg);
        }
    }

    /**
     * @class
     * Exception type that represents a failing assert.
     *
     * @param {string} message - Error message.
     */
    function AssertionError(message)
    {
        if (typeof message === "string") {
            message = sanitize_unpaired_surrogates(message);
        }
        this.message = message;
        this.stack = get_stack();
    }
    expose(AssertionError, "AssertionError");

    AssertionError.prototype = Object.create(Error.prototype);

    const get_stack = function() {
        return new Error().stack;
    };

    function OptionalFeatureUnsupportedError(message)
    {
        AssertionError.call(this, message);
    }
    OptionalFeatureUnsupportedError.prototype = Object.create(AssertionError.prototype);
    expose(OptionalFeatureUnsupportedError, "OptionalFeatureUnsupportedError");


    function filter(array, callable, thisObj) {
        var rv = [];
        for (var i = 0; i < array.length; i++) {
            if (array.hasOwnProperty(i)) {
                var pass = callable.call(thisObj, array[i], i, array);
                if (pass) {
                    rv.push(array[i]);
                }
            }
        }
        return rv;
    }

    function map(array, callable, thisObj)
    {
        var rv = [];
        rv.length = array.length;
        for (var i = 0; i < array.length; i++) {
            if (array.hasOwnProperty(i)) {
                rv[i] = callable.call(thisObj, array[i], i, array);
            }
        }
        return rv;
    }

    function extend(array, items)
    {
        Array.prototype.push.apply(array, items);
    }

    function forEach(array, callback, thisObj)
    {
        for (var i = 0; i < array.length; i++) {
            if (array.hasOwnProperty(i)) {
                callback.call(thisObj, array[i], i, array);
            }
        }
    }

    /**
     * Immediately invoke a "iteratee" function with a series of values in
     * parallel and invoke a final "done" function when all of the "iteratee"
     * invocations have signaled completion.
     *
     * If all callbacks complete synchronously (or if no callbacks are
     * specified), the ``done_callback`` will be invoked synchronously. It is the
     * responsibility of the caller to ensure asynchronicity in cases where
     * that is desired.
     *
     * @param {array} value Zero or more values to use in the invocation of
     *                      ``iter_callback``
     * @param {function} iter_callback A function that will be invoked
     *                                 once for each of the values min
     *                                 ``value``. Two arguments will
     *                                 be available in each
     *                                 invocation: the value from
     *                                 ``value`` and a function that
     *                                 must be invoked to signal
     *                                 completion
     * @param {function} done_callback A function that will be invoked after
     *                                 all operations initiated by the
     *                                 ``iter_callback`` function have signaled
     *                                 completion
     */
    function all_async(values, iter_callback, done_callback)
    {
        var remaining = values.length;

        if (remaining === 0) {
            done_callback();
        }

        forEach(values,
                function(element) {
                    var invoked = false;
                    var elDone = function() {
                        if (invoked) {
                            return;
                        }

                        invoked = true;
                        remaining -= 1;

                        if (remaining === 0) {
                            done_callback();
                        }
                    };

                    iter_callback(element, elDone);
                });
    }

    function merge(a,b)
    {
        var rv = {};
        var p;
        for (p in a) {
            rv[p] = a[p];
        }
        for (p in b) {
            rv[p] = b[p];
        }
        return rv;
    }

    function expose(object, name)
    {
        var components = name.split(".");
        var target = global_scope;
        for (var i = 0; i < components.length - 1; i++) {
            if (!(components[i] in target)) {
                target[components[i]] = {};
            }
            target = target[components[i]];
        }
        target[components[components.length - 1]] = object;
    }
    /**
     * Setup globals
     */

    var tests = new Tests();
})(self);

// ==== wasm-module-builder ====

// Copyright 2016 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// Used for encoding f32 and double constants to bits.
let byte_view = new Uint8Array(8);
// let data_view = new DataView(byte_view.buffer);

// The bytes function receives one of
//  - several arguments, each of which is either a number or a string of length
//    1; if it's a string, the charcode of the contained character is used.
//  - a single array argument containing the actual arguments
//  - a single string; the returned buffer will contain the char codes of all
//    contained characters.
function bytes(...input) {
  if (input.length == 1 && typeof input[0] == 'array') input = input[0];
  if (input.length == 1 && typeof input[0] == 'string') {
    let len = input[0].length;
    let view = new Uint8Array(len);
    for (let i = 0; i < len; i++) view[i] = input[0].charCodeAt(i);
    return view.buffer;
  }
  let view = new Uint8Array(input.length);
  for (let i = 0; i < input.length; i++) {
    let val = input[i];
    if (typeof val == 'string') {
      assertEquals(1, val.length, 'string inputs must have length 1');
      val = val.charCodeAt(0);
    }
    view[i] = val | 0;
  }
  return view.buffer;
}

// Header declaration constants
var kWasmH0 = 0;
var kWasmH1 = 0x61;
var kWasmH2 = 0x73;
var kWasmH3 = 0x6d;

var kWasmV0 = 0x1;
var kWasmV1 = 0;
var kWasmV2 = 0;
var kWasmV3 = 0;

var kHeaderSize = 8;
var kPageSize = 65536;
var kSpecMaxPages = 65535;
var kMaxVarInt32Size = 5;
var kMaxVarInt64Size = 10;

let kDeclNoLocals = 0;

// Section declaration constants
let kUnknownSectionCode = 0;
let kTypeSectionCode = 1;        // Function signature declarations
let kImportSectionCode = 2;      // Import declarations
let kFunctionSectionCode = 3;    // Function declarations
let kTableSectionCode = 4;       // Indirect function table and other tables
let kMemorySectionCode = 5;      // Memory attributes
let kGlobalSectionCode = 6;      // Global declarations
let kExportSectionCode = 7;      // Exports
let kStartSectionCode = 8;       // Start function declaration
let kElementSectionCode = 9;     // Elements section
let kCodeSectionCode = 10;       // Function code
let kDataSectionCode = 11;       // Data segments
let kDataCountSectionCode = 12;  // Data segment count (between Element & Code)
let kTagSectionCode = 13;        // Tag section (between Memory & Global)

// Name section types
let kModuleNameCode = 0;
let kFunctionNamesCode = 1;
let kLocalNamesCode = 2;

let kWasmFunctionTypeForm = 0x60;
let kWasmAnyFunctionTypeForm = 0x70;

let kHasMaximumFlag = 1;
let kSharedHasMaximumFlag = 3;

// Segment flags
let kActiveNoIndex = 0;
let kPassive = 1;
let kActiveWithIndex = 2;
let kPassiveWithElements = 5;

// Function declaration flags
let kDeclFunctionName   = 0x01;
let kDeclFunctionImport = 0x02;
let kDeclFunctionLocals = 0x04;
let kDeclFunctionExport = 0x08;

// Local types
let kWasmStmt = 0x40;
let kWasmI32 = 0x7f;
let kWasmI64 = 0x7e;
let kWasmF32 = 0x7d;
let kWasmF64 = 0x7c;
let kWasmS128 = 0x7b;
let kWasmAnyRef = 0x6f;
let kWasmAnyFunc = 0x70;

let kExternalFunction = 0;
let kExternalTable = 1;
let kExternalMemory = 2;
let kExternalGlobal = 3;
let kExternalTag = 4;

let kTableZero = 0;
let kMemoryZero = 0;
let kSegmentZero = 0;

let kTagAttribute = 0;

// Useful signatures
let kSig_i_i = makeSig([kWasmI32], [kWasmI32]);
let kSig_l_l = makeSig([kWasmI64], [kWasmI64]);
let kSig_i_l = makeSig([kWasmI64], [kWasmI32]);
let kSig_i_ii = makeSig([kWasmI32, kWasmI32], [kWasmI32]);
let kSig_i_iii = makeSig([kWasmI32, kWasmI32, kWasmI32], [kWasmI32]);
let kSig_v_iiii = makeSig([kWasmI32, kWasmI32, kWasmI32, kWasmI32], []);
let kSig_f_ff = makeSig([kWasmF32, kWasmF32], [kWasmF32]);
let kSig_d_dd = makeSig([kWasmF64, kWasmF64], [kWasmF64]);
let kSig_l_ll = makeSig([kWasmI64, kWasmI64], [kWasmI64]);
let kSig_i_dd = makeSig([kWasmF64, kWasmF64], [kWasmI32]);
let kSig_v_v = makeSig([], []);
let kSig_i_v = makeSig([], [kWasmI32]);
let kSig_l_v = makeSig([], [kWasmI64]);
let kSig_f_v = makeSig([], [kWasmF32]);
let kSig_d_v = makeSig([], [kWasmF64]);
let kSig_v_i = makeSig([kWasmI32], []);
let kSig_v_ii = makeSig([kWasmI32, kWasmI32], []);
let kSig_v_iii = makeSig([kWasmI32, kWasmI32, kWasmI32], []);
let kSig_v_l = makeSig([kWasmI64], []);
let kSig_v_d = makeSig([kWasmF64], []);
let kSig_v_dd = makeSig([kWasmF64, kWasmF64], []);
let kSig_v_ddi = makeSig([kWasmF64, kWasmF64, kWasmI32], []);
let kSig_ii_v = makeSig([], [kWasmI32, kWasmI32]);
let kSig_iii_v = makeSig([], [kWasmI32, kWasmI32, kWasmI32]);
let kSig_ii_i = makeSig([kWasmI32], [kWasmI32, kWasmI32]);
let kSig_iii_i = makeSig([kWasmI32], [kWasmI32, kWasmI32, kWasmI32]);
let kSig_ii_ii = makeSig([kWasmI32, kWasmI32], [kWasmI32, kWasmI32]);
let kSig_iii_ii = makeSig([kWasmI32, kWasmI32], [kWasmI32, kWasmI32, kWasmI32]);

let kSig_v_f = makeSig([kWasmF32], []);
let kSig_f_f = makeSig([kWasmF32], [kWasmF32]);
let kSig_f_d = makeSig([kWasmF64], [kWasmF32]);
let kSig_d_d = makeSig([kWasmF64], [kWasmF64]);
let kSig_r_r = makeSig([kWasmAnyRef], [kWasmAnyRef]);
let kSig_a_a = makeSig([kWasmAnyFunc], [kWasmAnyFunc]);
let kSig_i_r = makeSig([kWasmAnyRef], [kWasmI32]);
let kSig_v_r = makeSig([kWasmAnyRef], []);
let kSig_v_a = makeSig([kWasmAnyFunc], []);
let kSig_v_rr = makeSig([kWasmAnyRef, kWasmAnyRef], []);
let kSig_v_aa = makeSig([kWasmAnyFunc, kWasmAnyFunc], []);
let kSig_r_v = makeSig([], [kWasmAnyRef]);
let kSig_a_v = makeSig([], [kWasmAnyFunc]);
let kSig_a_i = makeSig([kWasmI32], [kWasmAnyFunc]);

function makeSig(params, results) {
  return {params: params, results: results};
}

function makeSig_v_x(x) {
  return makeSig([x], []);
}

function makeSig_v_xx(x) {
  return makeSig([x, x], []);
}

function makeSig_r_v(r) {
  return makeSig([], [r]);
}

function makeSig_r_x(r, x) {
  return makeSig([x], [r]);
}

function makeSig_r_xx(r, x) {
  return makeSig([x, x], [r]);
}

// Opcodes
let kExprUnreachable = 0x00;
let kExprNop = 0x01;
let kExprBlock = 0x02;
let kExprLoop = 0x03;
let kExprIf = 0x04;
let kExprElse = 0x05;
let kExprTry = 0x06;
let kExprCatch = 0x07;
let kExprCatchAll = 0x19;
let kExprThrow = 0x08;
let kExprRethrow = 0x09;
let kExprBrOnExn = 0x0a;
let kExprEnd = 0x0b;
let kExprBr = 0x0c;
let kExprBrIf = 0x0d;
let kExprBrTable = 0x0e;
let kExprReturn = 0x0f;
let kExprCallFunction = 0x10;
let kExprCallIndirect = 0x11;
let kExprReturnCall = 0x12;
let kExprReturnCallIndirect = 0x13;
let kExprDrop = 0x1a;
let kExprSelect = 0x1b;
let kExprLocalGet = 0x20;
let kExprLocalSet = 0x21;
let kExprLocalTee = 0x22;
let kExprGlobalGet = 0x23;
let kExprGlobalSet = 0x24;
let kExprTableGet = 0x25;
let kExprTableSet = 0x26;
let kExprI32LoadMem = 0x28;
let kExprI64LoadMem = 0x29;
let kExprF32LoadMem = 0x2a;
let kExprF64LoadMem = 0x2b;
let kExprI32LoadMem8S = 0x2c;
let kExprI32LoadMem8U = 0x2d;
let kExprI32LoadMem16S = 0x2e;
let kExprI32LoadMem16U = 0x2f;
let kExprI64LoadMem8S = 0x30;
let kExprI64LoadMem8U = 0x31;
let kExprI64LoadMem16S = 0x32;
let kExprI64LoadMem16U = 0x33;
let kExprI64LoadMem32S = 0x34;
let kExprI64LoadMem32U = 0x35;
let kExprI32StoreMem = 0x36;
let kExprI64StoreMem = 0x37;
let kExprF32StoreMem = 0x38;
let kExprF64StoreMem = 0x39;
let kExprI32StoreMem8 = 0x3a;
let kExprI32StoreMem16 = 0x3b;
let kExprI64StoreMem8 = 0x3c;
let kExprI64StoreMem16 = 0x3d;
let kExprI64StoreMem32 = 0x3e;
let kExprMemorySize = 0x3f;
let kExprMemoryGrow = 0x40;
let kExprI32Const = 0x41;
let kExprI64Const = 0x42;
let kExprF32Const = 0x43;
let kExprF64Const = 0x44;
let kExprI32Eqz = 0x45;
let kExprI32Eq = 0x46;
let kExprI32Ne = 0x47;
let kExprI32LtS = 0x48;
let kExprI32LtU = 0x49;
let kExprI32GtS = 0x4a;
let kExprI32GtU = 0x4b;
let kExprI32LeS = 0x4c;
let kExprI32LeU = 0x4d;
let kExprI32GeS = 0x4e;
let kExprI32GeU = 0x4f;
let kExprI64Eqz = 0x50;
let kExprI64Eq = 0x51;
let kExprI64Ne = 0x52;
let kExprI64LtS = 0x53;
let kExprI64LtU = 0x54;
let kExprI64GtS = 0x55;
let kExprI64GtU = 0x56;
let kExprI64LeS = 0x57;
let kExprI64LeU = 0x58;
let kExprI64GeS = 0x59;
let kExprI64GeU = 0x5a;
let kExprF32Eq = 0x5b;
let kExprF32Ne = 0x5c;
let kExprF32Lt = 0x5d;
let kExprF32Gt = 0x5e;
let kExprF32Le = 0x5f;
let kExprF32Ge = 0x60;
let kExprF64Eq = 0x61;
let kExprF64Ne = 0x62;
let kExprF64Lt = 0x63;
let kExprF64Gt = 0x64;
let kExprF64Le = 0x65;
let kExprF64Ge = 0x66;
let kExprI32Clz = 0x67;
let kExprI32Ctz = 0x68;
let kExprI32Popcnt = 0x69;
let kExprI32Add = 0x6a;
let kExprI32Sub = 0x6b;
let kExprI32Mul = 0x6c;
let kExprI32DivS = 0x6d;
let kExprI32DivU = 0x6e;
let kExprI32RemS = 0x6f;
let kExprI32RemU = 0x70;
let kExprI32And = 0x71;
let kExprI32Ior = 0x72;
let kExprI32Xor = 0x73;
let kExprI32Shl = 0x74;
let kExprI32ShrS = 0x75;
let kExprI32ShrU = 0x76;
let kExprI32Rol = 0x77;
let kExprI32Ror = 0x78;
let kExprI64Clz = 0x79;
let kExprI64Ctz = 0x7a;
let kExprI64Popcnt = 0x7b;
let kExprI64Add = 0x7c;
let kExprI64Sub = 0x7d;
let kExprI64Mul = 0x7e;
let kExprI64DivS = 0x7f;
let kExprI64DivU = 0x80;
let kExprI64RemS = 0x81;
let kExprI64RemU = 0x82;
let kExprI64And = 0x83;
let kExprI64Ior = 0x84;
let kExprI64Xor = 0x85;
let kExprI64Shl = 0x86;
let kExprI64ShrS = 0x87;
let kExprI64ShrU = 0x88;
let kExprI64Rol = 0x89;
let kExprI64Ror = 0x8a;
let kExprF32Abs = 0x8b;
let kExprF32Neg = 0x8c;
let kExprF32Ceil = 0x8d;
let kExprF32Floor = 0x8e;
let kExprF32Trunc = 0x8f;
let kExprF32NearestInt = 0x90;
let kExprF32Sqrt = 0x91;
let kExprF32Add = 0x92;
let kExprF32Sub = 0x93;
let kExprF32Mul = 0x94;
let kExprF32Div = 0x95;
let kExprF32Min = 0x96;
let kExprF32Max = 0x97;
let kExprF32CopySign = 0x98;
let kExprF64Abs = 0x99;
let kExprF64Neg = 0x9a;
let kExprF64Ceil = 0x9b;
let kExprF64Floor = 0x9c;
let kExprF64Trunc = 0x9d;
let kExprF64NearestInt = 0x9e;
let kExprF64Sqrt = 0x9f;
let kExprF64Add = 0xa0;
let kExprF64Sub = 0xa1;
let kExprF64Mul = 0xa2;
let kExprF64Div = 0xa3;
let kExprF64Min = 0xa4;
let kExprF64Max = 0xa5;
let kExprF64CopySign = 0xa6;
let kExprI32ConvertI64 = 0xa7;
let kExprI32SConvertF32 = 0xa8;
let kExprI32UConvertF32 = 0xa9;
let kExprI32SConvertF64 = 0xaa;
let kExprI32UConvertF64 = 0xab;
let kExprI64SConvertI32 = 0xac;
let kExprI64UConvertI32 = 0xad;
let kExprI64SConvertF32 = 0xae;
let kExprI64UConvertF32 = 0xaf;
let kExprI64SConvertF64 = 0xb0;
let kExprI64UConvertF64 = 0xb1;
let kExprF32SConvertI32 = 0xb2;
let kExprF32UConvertI32 = 0xb3;
let kExprF32SConvertI64 = 0xb4;
let kExprF32UConvertI64 = 0xb5;
let kExprF32ConvertF64 = 0xb6;
let kExprF64SConvertI32 = 0xb7;
let kExprF64UConvertI32 = 0xb8;
let kExprF64SConvertI64 = 0xb9;
let kExprF64UConvertI64 = 0xba;
let kExprF64ConvertF32 = 0xbb;
let kExprI32ReinterpretF32 = 0xbc;
let kExprI64ReinterpretF64 = 0xbd;
let kExprF32ReinterpretI32 = 0xbe;
let kExprF64ReinterpretI64 = 0xbf;
let kExprI32SExtendI8 = 0xc0;
let kExprI32SExtendI16 = 0xc1;
let kExprI64SExtendI8 = 0xc2;
let kExprI64SExtendI16 = 0xc3;
let kExprI64SExtendI32 = 0xc4;
let kExprRefNull = 0xd0;
let kExprRefIsNull = 0xd1;
let kExprRefFunc = 0xd2;

// Prefix opcodes
let kNumericPrefix = 0xfc;
let kSimdPrefix = 0xfd;
let kAtomicPrefix = 0xfe;

// Numeric opcodes.
let kExprMemoryInit = 0x08;
let kExprDataDrop = 0x09;
let kExprMemoryCopy = 0x0a;
let kExprMemoryFill = 0x0b;
let kExprTableInit = 0x0c;
let kExprElemDrop = 0x0d;
let kExprTableCopy = 0x0e;
let kExprTableGrow = 0x0f;
let kExprTableSize = 0x10;
let kExprTableFill = 0x11;

// Atomic opcodes.
let kExprAtomicNotify = 0x00;
let kExprI32AtomicWait = 0x01;
let kExprI64AtomicWait = 0x02;
let kExprI32AtomicLoad = 0x10;
let kExprI32AtomicLoad8U = 0x12;
let kExprI32AtomicLoad16U = 0x13;
let kExprI32AtomicStore = 0x17;
let kExprI32AtomicStore8U = 0x19;
let kExprI32AtomicStore16U = 0x1a;
let kExprI32AtomicAdd = 0x1e;
let kExprI32AtomicAdd8U = 0x20;
let kExprI32AtomicAdd16U = 0x21;
let kExprI32AtomicSub = 0x25;
let kExprI32AtomicSub8U = 0x27;
let kExprI32AtomicSub16U = 0x28;
let kExprI32AtomicAnd = 0x2c;
let kExprI32AtomicAnd8U = 0x2e;
let kExprI32AtomicAnd16U = 0x2f;
let kExprI32AtomicOr = 0x33;
let kExprI32AtomicOr8U = 0x35;
let kExprI32AtomicOr16U = 0x36;
let kExprI32AtomicXor = 0x3a;
let kExprI32AtomicXor8U = 0x3c;
let kExprI32AtomicXor16U = 0x3d;
let kExprI32AtomicExchange = 0x41;
let kExprI32AtomicExchange8U = 0x43;
let kExprI32AtomicExchange16U = 0x44;
let kExprI32AtomicCompareExchange = 0x48;
let kExprI32AtomicCompareExchange8U = 0x4a;
let kExprI32AtomicCompareExchange16U = 0x4b;

let kExprI64AtomicLoad = 0x11;
let kExprI64AtomicLoad8U = 0x14;
let kExprI64AtomicLoad16U = 0x15;
let kExprI64AtomicLoad32U = 0x16;
let kExprI64AtomicStore = 0x18;
let kExprI64AtomicStore8U = 0x1b;
let kExprI64AtomicStore16U = 0x1c;
let kExprI64AtomicStore32U = 0x1d;
let kExprI64AtomicAdd = 0x1f;
let kExprI64AtomicAdd8U = 0x22;
let kExprI64AtomicAdd16U = 0x23;
let kExprI64AtomicAdd32U = 0x24;
let kExprI64AtomicSub = 0x26;
let kExprI64AtomicSub8U = 0x29;
let kExprI64AtomicSub16U = 0x2a;
let kExprI64AtomicSub32U = 0x2b;
let kExprI64AtomicAnd = 0x2d;
let kExprI64AtomicAnd8U = 0x30;
let kExprI64AtomicAnd16U = 0x31;
let kExprI64AtomicAnd32U = 0x32;
let kExprI64AtomicOr = 0x34;
let kExprI64AtomicOr8U = 0x37;
let kExprI64AtomicOr16U = 0x38;
let kExprI64AtomicOr32U = 0x39;
let kExprI64AtomicXor = 0x3b;
let kExprI64AtomicXor8U = 0x3e;
let kExprI64AtomicXor16U = 0x3f;
let kExprI64AtomicXor32U = 0x40;
let kExprI64AtomicExchange = 0x42;
let kExprI64AtomicExchange8U = 0x45;
let kExprI64AtomicExchange16U = 0x46;
let kExprI64AtomicExchange32U = 0x47;
let kExprI64AtomicCompareExchange = 0x49
let kExprI64AtomicCompareExchange8U = 0x4c;
let kExprI64AtomicCompareExchange16U = 0x4d;
let kExprI64AtomicCompareExchange32U = 0x4e;

// Simd opcodes.
let kExprS128LoadMem = 0x00;
let kExprS128StoreMem = 0x01;
let kExprI32x4Splat = 0x0c;
let kExprI32x4Eq = 0x2c;
let kExprS1x4AllTrue = 0x75;
let kExprF32x4Min = 0x9e;

class Binary {
  constructor() {
    this.length = 0;
    this.buffer = new Uint8Array(8192);
  }

  ensure_space(needed) {
    if (this.buffer.length - this.length >= needed) return;
    let new_capacity = this.buffer.length * 2;
    while (new_capacity - this.length < needed) new_capacity *= 2;
    let new_buffer = new Uint8Array(new_capacity);
    new_buffer.set(this.buffer);
    this.buffer = new_buffer;
  }

  trunc_buffer() {
    return new Uint8Array(this.buffer.buffer, 0, this.length);
  }

  reset() {
    this.length = 0;
  }

  emit_u8(val) {
    this.ensure_space(1);
    this.buffer[this.length++] = val;
  }

  emit_u16(val) {
    this.ensure_space(2);
    this.buffer[this.length++] = val;
    this.buffer[this.length++] = val >> 8;
  }

  emit_u32(val) {
    this.ensure_space(4);
    this.buffer[this.length++] = val;
    this.buffer[this.length++] = val >> 8;
    this.buffer[this.length++] = val >> 16;
    this.buffer[this.length++] = val >> 24;
  }

  emit_leb_u(val, max_len) {
    this.ensure_space(max_len);
    for (let i = 0; i < max_len; ++i) {
      let v = val & 0xff;
      val = val >>> 7;
      if (val == 0) {
        this.buffer[this.length++] = v;
        return;
      }
      this.buffer[this.length++] = v | 0x80;
    }
    throw new Error("Leb value exceeds maximum length of " + max_len);
  }

  emit_u32v(val) {
    this.emit_leb_u(val, kMaxVarInt32Size);
  }

  emit_u64v(val) {
    this.emit_leb_u(val, kMaxVarInt64Size);
  }

  emit_bytes(data) {
    this.ensure_space(data.length);
    this.buffer.set(data, this.length);
    this.length += data.length;
  }

  emit_string(string) {
    // When testing illegal names, we pass a byte array directly.
    if (string instanceof Array) {
      this.emit_u32v(string.length);
      this.emit_bytes(string);
      return;
    }

    // This is the hacky way to convert a JavaScript string to a UTF8 encoded
    // string only containing single-byte characters.
    let string_utf8 = unescape(encodeURIComponent(string));
    this.emit_u32v(string_utf8.length);
    for (let i = 0; i < string_utf8.length; i++) {
      this.emit_u8(string_utf8.charCodeAt(i));
    }
  }

  emit_header() {
    this.emit_bytes([
      kWasmH0, kWasmH1, kWasmH2, kWasmH3, kWasmV0, kWasmV1, kWasmV2, kWasmV3
    ]);
  }

  emit_section(section_code, content_generator) {
    // Emit section name.
    this.emit_u8(section_code);
    // Emit the section to a temporary buffer: its full length isn't know yet.
    const section = new Binary;
    content_generator(section);
    // Emit section length.
    this.emit_u32v(section.length);
    // Copy the temporary buffer.
    // Avoid spread because {section} can be huge.
    this.emit_bytes(section.trunc_buffer());
  }
}

class WasmFunctionBuilder {
  constructor(module, name, type_index) {
    this.module = module;
    this.name = name;
    this.type_index = type_index;
    this.body = [];
    this.locals = [];
    this.local_names = [];
  }

  numLocalNames() {
    let num_local_names = 0;
    for (let loc_name of this.local_names) {
      if (loc_name !== undefined) ++num_local_names;
    }
    return num_local_names;
  }

  exportAs(name) {
    this.module.addExport(name, this.index);
    return this;
  }

  exportFunc() {
    this.exportAs(this.name);
    return this;
  }

  addBody(body) {
    for (let b of body) {
      if (typeof b !== 'number' || (b & (~0xFF)) !== 0 )
        throw new Error('invalid body (entries must be 8 bit numbers): ' + body);
    }
    this.body = body.slice();
    // Automatically add the end for the function block to the body.
    this.body.push(kExprEnd);
    return this;
  }

  addBodyWithEnd(body) {
    this.body = body;
    return this;
  }

  getNumLocals() {
    let total_locals = 0;
    for (let l of this.locals) {
      for (let type of ["i32", "i64", "f32", "f64", "s128"]) {
        total_locals += l[type + "_count"] || 0;
      }
    }
    return total_locals;
  }

  addLocals(locals, names) {
    const old_num_locals = this.getNumLocals();
    this.locals.push(locals);
    if (names) {
      const missing_names = old_num_locals - this.local_names.length;
      this.local_names.push(...new Array(missing_names), ...names);
    }
    return this;
  }

  end() {
    return this.module;
  }
}

class WasmGlobalBuilder {
  constructor(module, type, mutable) {
    this.module = module;
    this.type = type;
    this.mutable = mutable;
    this.init = 0;
  }

  exportAs(name) {
    this.module.exports.push({name: name, kind: kExternalGlobal,
                              index: this.index});
    return this;
  }
}

class WasmTableBuilder {
  constructor(module, type, initial_size, max_size) {
    this.module = module;
    this.type = type;
    this.initial_size = initial_size;
    this.has_max = max_size != undefined;
    this.max_size = max_size;
  }

  exportAs(name) {
    this.module.exports.push({name: name, kind: kExternalTable,
                              index: this.index});
    return this;
  }
}

class WasmModuleBuilder {
  constructor() {
    this.types = [];
    this.imports = [];
    this.exports = [];
    this.globals = [];
    this.tables = [];
    this.tags = [];
    this.functions = [];
    this.element_segments = [];
    this.data_segments = [];
    this.explicit = [];
    this.num_imported_funcs = 0;
    this.num_imported_globals = 0;
    this.num_imported_tables = 0;
    this.num_imported_tags = 0;
    return this;
  }

  addStart(start_index) {
    this.start_index = start_index;
    return this;
  }

  addMemory(min, max, exp, shared) {
    this.memory = {min: min, max: max, exp: exp, shared: shared};
    return this;
  }

  addExplicitSection(bytes) {
    this.explicit.push(bytes);
    return this;
  }

  stringToBytes(name) {
    var result = new Binary();
    result.emit_string(name);
    return result.trunc_buffer()
  }

  createCustomSection(name, bytes) {
    name = this.stringToBytes(name);
    var section = new Binary();
    section.emit_u8(kUnknownSectionCode);
    section.emit_u32v(name.length + bytes.length);
    section.emit_bytes(name);
    section.emit_bytes(bytes);
    return section.trunc_buffer();
  }

  addCustomSection(name, bytes) {
    this.explicit.push(this.createCustomSection(name, bytes));
  }

  addType(type) {
    this.types.push(type);
    var pl = type.params.length;  // should have params
    var rl = type.results.length; // should have results
    return this.types.length - 1;
  }

  addGlobal(local_type, mutable) {
    let glob = new WasmGlobalBuilder(this, local_type, mutable);
    glob.index = this.globals.length + this.num_imported_globals;
    this.globals.push(glob);
    return glob;
  }

  addTable(type, initial_size, max_size = undefined) {
    if (type != kWasmAnyRef && type != kWasmAnyFunc) {
      throw new Error('Tables must be of type kWasmAnyRef or kWasmAnyFunc');
    }
    let table = new WasmTableBuilder(this, type, initial_size, max_size);
    table.index = this.tables.length + this.num_imported_tables;
    this.tables.push(table);
    return table;
  }

  addTag(type) {
    let type_index = (typeof type) == "number" ? type : this.addType(type);
    let tag_index = this.tags.length + this.num_imported_tags;
    this.tags.push(type_index);
    return tag_index;
  }

  addFunction(name, type) {
    let type_index = (typeof type) == "number" ? type : this.addType(type);
    let func = new WasmFunctionBuilder(this, name, type_index);
    func.index = this.functions.length + this.num_imported_funcs;
    this.functions.push(func);
    return func;
  }

  addImport(module, name, type) {
    if (this.functions.length != 0) {
      throw new Error('Imported functions must be declared before local ones');
    }
    let type_index = (typeof type) == "number" ? type : this.addType(type);
    this.imports.push({module: module, name: name, kind: kExternalFunction,
                       type: type_index});
    return this.num_imported_funcs++;
  }

  addImportedGlobal(module, name, type, mutable = false) {
    if (this.globals.length != 0) {
      throw new Error('Imported globals must be declared before local ones');
    }
    let o = {module: module, name: name, kind: kExternalGlobal, type: type,
             mutable: mutable};
    this.imports.push(o);
    return this.num_imported_globals++;
  }

  addImportedMemory(module, name, initial = 0, maximum, shared) {
    let o = {module: module, name: name, kind: kExternalMemory,
             initial: initial, maximum: maximum, shared: shared};
    this.imports.push(o);
    return this;
  }

  addImportedTable(module, name, initial, maximum, type) {
    if (this.tables.length != 0) {
      throw new Error('Imported tables must be declared before local ones');
    }
    let o = {module: module, name: name, kind: kExternalTable, initial: initial,
             maximum: maximum, type: type || kWasmAnyFunctionTypeForm};
    this.imports.push(o);
    return this.num_imported_tables++;
  }

  addImportedTag(module, name, type) {
    if (this.tags.length != 0) {
      throw new Error('Imported tags must be declared before local ones');
    }
    let type_index = (typeof type) == "number" ? type : this.addType(type);
    let o = {module: module, name: name, kind: kExternalTag, type: type_index};
    this.imports.push(o);
    return this.num_imported_tags++;
  }

  addExport(name, index) {
    this.exports.push({name: name, kind: kExternalFunction, index: index});
    return this;
  }

  addExportOfKind(name, kind, index) {
    this.exports.push({name: name, kind: kind, index: index});
    return this;
  }

  addDataSegment(addr, data, is_global = false) {
    this.data_segments.push(
        {addr: addr, data: data, is_global: is_global, is_active: true});
    return this.data_segments.length - 1;
  }

  addPassiveDataSegment(data) {
    this.data_segments.push({data: data, is_active: false});
    return this.data_segments.length - 1;
  }

  exportMemoryAs(name) {
    this.exports.push({name: name, kind: kExternalMemory, index: 0});
  }

  addElementSegment(table, base, is_global, array) {
    this.element_segments.push({table: table, base: base, is_global: is_global,
                                    array: array, is_active: true});
    return this;
  }

  addPassiveElementSegment(array, is_import = false) {
    this.element_segments.push({array: array, is_active: false});
    return this;
  }

  appendToTable(array) {
    for (let n of array) {
      if (typeof n != 'number')
        throw new Error('invalid table (entries have to be numbers): ' + array);
    }
    if (this.tables.length == 0) {
      this.addTable(kWasmAnyFunc, 0);
    }
    // Adjust the table to the correct size.
    let table = this.tables[0];
    const base = table.initial_size;
    const table_size = base + array.length;
    table.initial_size = table_size;
    if (table.has_max && table_size > table.max_size) {
      table.max_size = table_size;
    }
    return this.addElementSegment(0, base, false, array);
  }

  setTableBounds(min, max = undefined) {
    if (this.tables.length != 0) {
      throw new Error("The table bounds of table '0' have already been set.");
    }
    this.addTable(kWasmAnyFunc, min, max);
    return this;
  }

  setName(name) {
    this.name = name;
    return this;
  }

  toBuffer(debug = false) {
    let binary = new Binary;
    let wasm = this;

    // Add header
    binary.emit_header();

    // Add type section
    if (wasm.types.length > 0) {
      if (debug) print("emitting types @ " + binary.length);
      binary.emit_section(kTypeSectionCode, section => {
        section.emit_u32v(wasm.types.length);
        for (let type of wasm.types) {
          section.emit_u8(kWasmFunctionTypeForm);
          section.emit_u32v(type.params.length);
          for (let param of type.params) {
            section.emit_u8(param);
          }
          section.emit_u32v(type.results.length);
          for (let result of type.results) {
            section.emit_u8(result);
          }
        }
      });
    }

    // Add imports section
    if (wasm.imports.length > 0) {
      if (debug) print("emitting imports @ " + binary.length);
      binary.emit_section(kImportSectionCode, section => {
        section.emit_u32v(wasm.imports.length);
        for (let imp of wasm.imports) {
          section.emit_string(imp.module);
          section.emit_string(imp.name || '');
          section.emit_u8(imp.kind);
          if (imp.kind == kExternalFunction) {
            section.emit_u32v(imp.type);
          } else if (imp.kind == kExternalGlobal) {
            section.emit_u32v(imp.type);
            section.emit_u8(imp.mutable);
          } else if (imp.kind == kExternalMemory) {
            var has_max = (typeof imp.maximum) != "undefined";
            var is_shared = (typeof imp.shared) != "undefined";
            if (is_shared) {
              section.emit_u8(has_max ? 3 : 2); // flags
            } else {
              section.emit_u8(has_max ? 1 : 0); // flags
            }
            section.emit_u32v(imp.initial); // initial
            if (has_max) section.emit_u32v(imp.maximum); // maximum
          } else if (imp.kind == kExternalTable) {
            section.emit_u8(imp.type);
            var has_max = (typeof imp.maximum) != "undefined";
            section.emit_u8(has_max ? 1 : 0); // flags
            section.emit_u32v(imp.initial); // initial
            if (has_max) section.emit_u32v(imp.maximum); // maximum
          } else if (imp.kind == kExternalTag) {
            section.emit_u32v(kTagAttribute);
            section.emit_u32v(imp.type);
          } else {
            throw new Error("unknown/unsupported import kind " + imp.kind);
          }
        }
      });
    }

    // Add functions declarations
    if (wasm.functions.length > 0) {
      if (debug) print("emitting function decls @ " + binary.length);
      binary.emit_section(kFunctionSectionCode, section => {
        section.emit_u32v(wasm.functions.length);
        for (let func of wasm.functions) {
          section.emit_u32v(func.type_index);
        }
      });
    }

    // Add table section
    if (wasm.tables.length > 0) {
      if (debug) print ("emitting tables @ " + binary.length);
      binary.emit_section(kTableSectionCode, section => {
        section.emit_u32v(wasm.tables.length);
        for (let table of wasm.tables) {
          section.emit_u8(table.type);
          section.emit_u8(table.has_max);
          section.emit_u32v(table.initial_size);
          if (table.has_max) section.emit_u32v(table.max_size);
        }
      });
    }

    // Add memory section
    if (wasm.memory !== undefined) {
      if (debug) print("emitting memory @ " + binary.length);
      binary.emit_section(kMemorySectionCode, section => {
        section.emit_u8(1);  // one memory entry
        const has_max = wasm.memory.max !== undefined;
        const is_shared = wasm.memory.shared !== undefined;
        // Emit flags (bit 0: reszeable max, bit 1: shared memory)
        if (is_shared) {
          section.emit_u8(has_max ? kSharedHasMaximumFlag : 2);
        } else {
          section.emit_u8(has_max ? kHasMaximumFlag : 0);
        }
        section.emit_u32v(wasm.memory.min);
        if (has_max) section.emit_u32v(wasm.memory.max);
      });
    }

    // Add global section.
    if (wasm.globals.length > 0) {
      if (debug) print ("emitting globals @ " + binary.length);
      binary.emit_section(kGlobalSectionCode, section => {
        section.emit_u32v(wasm.globals.length);
        for (let global of wasm.globals) {
          section.emit_u8(global.type);
          section.emit_u8(global.mutable);
          if ((typeof global.init_index) == "undefined") {
            // Emit a constant initializer.
            switch (global.type) {
            case kWasmI32:
              section.emit_u8(kExprI32Const);
              section.emit_u32v(global.init);
              break;
            case kWasmI64:
              section.emit_u8(kExprI64Const);
              section.emit_u64v(global.init);
              break;
            case kWasmF32:
              section.emit_bytes(wasmF32Const(global.init));
              break;
            case kWasmF64:
              section.emit_bytes(wasmF64Const(global.init));
              break;
            case kWasmAnyFunc:
            case kWasmAnyRef:
              if (global.function_index !== undefined) {
                section.emit_u8(kExprRefFunc);
                section.emit_u32v(global.function_index);
              } else {
                section.emit_u8(kExprRefNull);
              }
              break;
            }
          } else {
            // Emit a global-index initializer.
            section.emit_u8(kExprGlobalGet);
            section.emit_u32v(global.init_index);
          }
          section.emit_u8(kExprEnd);  // end of init expression
        }
      });
    }

    // Add tags.
    if (wasm.tags.length > 0) {
      if (debug) print("emitting tags @ " + binary.length);
      binary.emit_section(kTagSectionCode, section => {
        section.emit_u32v(wasm.tags.length);
        for (let type of wasm.tags) {
          section.emit_u32v(kTagAttribute);
          section.emit_u32v(type);
        }
      });
    }

    // Add export table.
    var mem_export = (wasm.memory !== undefined && wasm.memory.exp);
    var exports_count = wasm.exports.length + (mem_export ? 1 : 0);
    if (exports_count > 0) {
      if (debug) print("emitting exports @ " + binary.length);
      binary.emit_section(kExportSectionCode, section => {
        section.emit_u32v(exports_count);
        for (let exp of wasm.exports) {
          section.emit_string(exp.name);
          section.emit_u8(exp.kind);
          section.emit_u32v(exp.index);
        }
        if (mem_export) {
          section.emit_string("memory");
          section.emit_u8(kExternalMemory);
          section.emit_u8(0);
        }
      });
    }

    // Add start function section.
    if (wasm.start_index !== undefined) {
      if (debug) print("emitting start function @ " + binary.length);
      binary.emit_section(kStartSectionCode, section => {
        section.emit_u32v(wasm.start_index);
      });
    }

    // Add element segments
    if (wasm.element_segments.length > 0) {
      if (debug) print("emitting element segments @ " + binary.length);
      binary.emit_section(kElementSectionCode, section => {
        var inits = wasm.element_segments;
        section.emit_u32v(inits.length);

        for (let init of inits) {
          if (init.is_active) {
            // Active segment.
            if (init.table == 0) {
              section.emit_u32v(kActiveNoIndex);
            } else {
              section.emit_u32v(kActiveWithIndex);
              section.emit_u32v(init.table);
            }
            if (init.is_global) {
              section.emit_u8(kExprGlobalGet);
            } else {
              section.emit_u8(kExprI32Const);
            }
            section.emit_u32v(init.base);
            section.emit_u8(kExprEnd);
            if (init.table != 0) {
              section.emit_u8(kExternalFunction);
            }
            section.emit_u32v(init.array.length);
            for (let index of init.array) {
              section.emit_u32v(index);
            }
          } else {
            // Passive segment.
            section.emit_u8(kPassiveWithElements);  // flags
            section.emit_u8(kWasmAnyFunc);
            section.emit_u32v(init.array.length);
            for (let index of init.array) {
              if (index === null) {
                section.emit_u8(kExprRefNull);
                section.emit_u8(kExprEnd);
              } else {
                section.emit_u8(kExprRefFunc);
                section.emit_u32v(index);
                section.emit_u8(kExprEnd);
              }
            }
          }
        }
      });
    }

    // If there are any passive data segments, add the DataCount section.
    if (wasm.data_segments.some(seg => !seg.is_active)) {
      binary.emit_section(kDataCountSectionCode, section => {
        section.emit_u32v(wasm.data_segments.length);
      });
    }

    // Add function bodies.
    if (wasm.functions.length > 0) {
      // emit function bodies
      if (debug) print("emitting code @ " + binary.length);
      binary.emit_section(kCodeSectionCode, section => {
        section.emit_u32v(wasm.functions.length);
        let header = new Binary;
        for (let func of wasm.functions) {
          header.reset();
          // Function body length will be patched later.
          let local_decls = [];
          for (let l of func.locals || []) {
            if (l.i32_count > 0) {
              local_decls.push({count: l.i32_count, type: kWasmI32});
            }
            if (l.i64_count > 0) {
              local_decls.push({count: l.i64_count, type: kWasmI64});
            }
            if (l.f32_count > 0) {
              local_decls.push({count: l.f32_count, type: kWasmF32});
            }
            if (l.f64_count > 0) {
              local_decls.push({count: l.f64_count, type: kWasmF64});
            }
            if (l.s128_count > 0) {
              local_decls.push({count: l.s128_count, type: kWasmS128});
            }
            if (l.anyref_count > 0) {
              local_decls.push({count: l.anyref_count, type: kWasmAnyRef});
            }
            if (l.anyfunc_count > 0) {
              local_decls.push({count: l.anyfunc_count, type: kWasmAnyFunc});
            }
          }

          header.emit_u32v(local_decls.length);
          for (let decl of local_decls) {
            header.emit_u32v(decl.count);
            header.emit_u8(decl.type);
          }

          section.emit_u32v(header.length + func.body.length);
          section.emit_bytes(header.trunc_buffer());
          section.emit_bytes(func.body);
        }
      });
    }

    // Add data segments.
    if (wasm.data_segments.length > 0) {
      if (debug) print("emitting data segments @ " + binary.length);
      binary.emit_section(kDataSectionCode, section => {
        section.emit_u32v(wasm.data_segments.length);
        for (let seg of wasm.data_segments) {
          if (seg.is_active) {
            section.emit_u8(0);  // linear memory index 0 / flags
            if (seg.is_global) {
              // initializer is a global variable
              section.emit_u8(kExprGlobalGet);
              section.emit_u32v(seg.addr);
            } else {
              // initializer is a constant
              section.emit_u8(kExprI32Const);
              section.emit_u32v(seg.addr);
            }
            section.emit_u8(kExprEnd);
          } else {
            section.emit_u8(kPassive);  // flags
          }
          section.emit_u32v(seg.data.length);
          section.emit_bytes(seg.data);
        }
      });
    }

    // Add any explicitly added sections
    for (let exp of wasm.explicit) {
      if (debug) print("emitting explicit @ " + binary.length);
      binary.emit_bytes(exp);
    }

    // Add names.
    let num_function_names = 0;
    let num_functions_with_local_names = 0;
    for (let func of wasm.functions) {
      if (func.name !== undefined) ++num_function_names;
      if (func.numLocalNames() > 0) ++num_functions_with_local_names;
    }
    if (num_function_names > 0 || num_functions_with_local_names > 0 ||
        wasm.name !== undefined) {
      if (debug) print('emitting names @ ' + binary.length);
      binary.emit_section(kUnknownSectionCode, section => {
        section.emit_string('name');
        // Emit module name.
        if (wasm.name !== undefined) {
          section.emit_section(kModuleNameCode, name_section => {
            name_section.emit_string(wasm.name);
          });
        }
        // Emit function names.
        if (num_function_names > 0) {
          section.emit_section(kFunctionNamesCode, name_section => {
            name_section.emit_u32v(num_function_names);
            for (let func of wasm.functions) {
              if (func.name === undefined) continue;
              name_section.emit_u32v(func.index);
              name_section.emit_string(func.name);
            }
          });
        }
        // Emit local names.
        if (num_functions_with_local_names > 0) {
          section.emit_section(kLocalNamesCode, name_section => {
            name_section.emit_u32v(num_functions_with_local_names);
            for (let func of wasm.functions) {
              if (func.numLocalNames() == 0) continue;
              name_section.emit_u32v(func.index);
              name_section.emit_u32v(func.numLocalNames());
              for (let i = 0; i < func.local_names.length; ++i) {
                if (func.local_names[i] === undefined) continue;
                name_section.emit_u32v(i);
                name_section.emit_string(func.local_names[i]);
              }
            }
          });
        }
      });
    }

    return binary.trunc_buffer();
  }

  toArray(debug = false) {
    return Array.from(this.toBuffer(debug));
  }

  instantiate(ffi) {
    let module = this.toModule();
    let instance = new WebAssembly.Instance(module, ffi);
    return instance;
  }

  asyncInstantiate(ffi) {
    return WebAssembly.instantiate(this.toBuffer(), ffi)
        .then(({module, instance}) => instance);
  }

  toModule(debug = false) {
    return new WebAssembly.Module(this.toBuffer(debug));
  }
}

function wasmSignedLeb(val, max_len = 5) {
  let res = [];
  for (let i = 0; i < max_len; ++i) {
    let v = val & 0x7f;
    // If {v} sign-extended from 7 to 32 bits is equal to val, we are done.
    if (((v << 25) >> 25) == val) {
      res.push(v);
      return res;
    }
    res.push(v | 0x80);
    val = val >> 7;
  }
  throw new Error(
      'Leb value <' + val + '> exceeds maximum length of ' + max_len);
}

function wasmI32Const(val) {
  return [kExprI32Const, ...wasmSignedLeb(val, 5)];
}

function wasmF32Const(f) {
  // Write in little-endian order at offset 0.
  data_view.setFloat32(0, f, true);
  return [
    kExprF32Const, byte_view[0], byte_view[1], byte_view[2], byte_view[3]
  ];
}

function wasmF64Const(f) {
  // Write in little-endian order at offset 0.
  data_view.setFloat64(0, f, true);
  return [
    kExprF64Const, byte_view[0], byte_view[1], byte_view[2],
    byte_view[3], byte_view[4], byte_view[5], byte_view[6], byte_view[7]
  ];
}

// instant-report.js
(() => {
  let total = 0, passed = 0;

  add_result_callback(r => {
    total++;
    const ok = r.status === 0;
    if (ok) passed++;
    print(`${ok ? "✅" : "❌"} ${r.name}${r.message ? `: ${r.message}` : ""}`);
  });

  add_completion_callback(() => {
    print(`\nTotal ${total}, Passed ${passed}, Failed ${total - passed}`);
  });
})();







// constructor.toString
test(() => {
  assert_own_property(WebAssembly, Symbol.toStringTag);

  const propDesc = Object.getOwnPropertyDescriptor(WebAssembly, Symbol.toStringTag);
  assert_equals(propDesc.value, "WebAssembly", "value");
  assert_equals(propDesc.writable, false, "writable");
  assert_equals(propDesc.enumerable, false, "enumerable");
  assert_equals(propDesc.configurable, true, "configurable");
}, "@@toStringTag exists on the namespace object with the appropriate descriptor");

test(() => {
  assert_equals(WebAssembly.toString(), "[object WebAssembly]");
  assert_equals(Object.prototype.toString.call(WebAssembly), "[object WebAssembly]");
}, "Object.prototype.toString applied to the namespace object");

test(t => {
  assert_own_property(WebAssembly, Symbol.toStringTag, "Precondition: @@toStringTag on the namespace object");
  t.add_cleanup(() => {
    Object.defineProperty(WebAssembly, Symbol.toStringTag, { value: "WebAssembly" });
  });

  Object.defineProperty(WebAssembly, Symbol.toStringTag, { value: "Test" });
  assert_equals(WebAssembly.toString(), "[object Test]");
  assert_equals(Object.prototype.toString.call(WebAssembly), "[object Test]");
}, "Object.prototype.toString applied after modifying the namespace object's @@toStringTag");

test(t => {
  assert_own_property(WebAssembly, Symbol.toStringTag, "Precondition: @@toStringTag on the namespace object");
  t.add_cleanup(() => {
    Object.defineProperty(WebAssembly, Symbol.toStringTag, { value: "WebAssembly" });
  });

  assert_true(delete WebAssembly[Symbol.toStringTag]);
  assert_equals(WebAssembly.toString(), "[object Object]");
  assert_equals(Object.prototype.toString.call(WebAssembly), "[object Object]");
}, "Object.prototype.toString applied after deleting @@toStringTag");


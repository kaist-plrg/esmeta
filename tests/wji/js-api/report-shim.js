// Prints each subtest's pass/fail once the whole file's tests finish. Load
// this LAST -- after shell-shim.js, harness.js, any META scripts, and the
// test file's own content. harness.js's add_completion_callback captures the
// *current* promise_test queue when called, so registering it before every
// promise_test() call has run would miss the later ones.
add_completion_callback((tests, harness_status) => {
  for (const t of tests) {
    print((t.status === 0 ? "PASS" : "FAIL") + " " + t.name + (t.message ? " - " + t.message : ""));
  }
});

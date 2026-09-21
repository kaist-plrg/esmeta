// Prints each subtest's pass/fail once the whole file's tests finish, and
// sets globalThis.__wjiOk to whether every subtest passed -- reusing the same
// convention tests/wji/manual/*.js fixtures use (see EvalSpec/WjiTest), so a
// whole js-api file is judged as one WJI eval test: pass iff every subtest
// in it passed. Also sets globalThis.__wjiFailingSubtests to a "|||"-joined
// list of failed subtest names (empty string when every subtest passed) --
// EvalSpec.expectedFailingSubtests reads this to pin down *which* subtests a
// fixture that's run for real (rather than skipped outright) is known to
// still fail, so a change in exactly what fails becomes a loud test failure
// instead of silently staying green either way. Load this LAST -- after
// shell-shim.js, testharness-lite.js, any META scripts, and the test file's
// own content. testharness-lite.js's add_completion_callback captures the
// *current* promise_test queue when called, so registering it before every
// promise_test() call has run would miss the later ones.
add_completion_callback((tests, harness_status) => {
  for (const t of tests) {
    print((t.status === 0 ? "PASS" : "FAIL") + " " + t.name + (t.message ? " - " + t.message : ""));
  }
  const passed = tests.filter((t) => t.status === 0).length;
  print("SUMMARY " + passed + "/" + tests.length);
  globalThis.__wjiOk = tests.length > 0 && tests.every((t) => t.status === 0);
  globalThis.__wjiFailingSubtests = tests.filter((t) => t.status !== 0).map((t) => t.name).join("|||");
});

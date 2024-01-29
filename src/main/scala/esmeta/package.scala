package esmeta

/** line seperator */
val LINE_SEP = System.getProperty("line.separator")

/** base project directory root */
val BASE_DIR = System.getenv("ESMETA_HOME")

/** base project directory root */
val VERSION = "0.1.0-rc3"

/** log directory */
val LOG_DIR = s"$BASE_DIR/logs"
val EXTRACT_LOG_DIR = s"$LOG_DIR/extract"
val COMPILE_LOG_DIR = s"$LOG_DIR/compile"
val CFG_LOG_DIR = s"$LOG_DIR/cfg"
val ANALYZE_LOG_DIR = s"$LOG_DIR/analyze"
val FUZZ_LOG_DIR = s"$LOG_DIR/fuzz"
val INJECT_LOG_DIR = s"$LOG_DIR/inject"
val GENTEST_LOG_DIR = s"$LOG_DIR/gen-test"
val CONFORMTEST_LOG_DIR = s"$LOG_DIR/conform-test"
val DELTADEBUG_LOG_DIR = s"$LOG_DIR/delta-debug"
val AGGREGATE_LOG_DIR = s"$LOG_DIR/aggregate"
val LOCALIZE_LOG_DIR = s"$LOG_DIR/localize"
val CATEGORIZE_LOG_DIR = s"$LOG_DIR/categorize"
val HANDLE_COVERAGE_LOG_DIR = s"$LOG_DIR/handle-coverage"
val EVAL_LOG_DIR = s"$LOG_DIR/eval"
val TEST262TEST_LOG_DIR = s"$LOG_DIR/test262"

/** tests directory root */
val TEST_DIR = s"$BASE_DIR/tests"

/** specification directory */
val ECMA262_DIR = s"$BASE_DIR/ecma262"
val SPEC_HTML = s"$ECMA262_DIR/spec.html"

/** current directory root */
val CUR_DIR = System.getProperty("user.dir")

/** source code directory */
val SRC_DIR = s"$BASE_DIR/src/main/scala/esmeta"

/** resource directory */
val RESOURCE_DIR = s"$BASE_DIR/src/main/resources"
val UNICODE_DIR = s"$RESOURCE_DIR/unicode"
val MANUALS_DIR = s"$RESOURCE_DIR/manuals"
val RESULT_DIR = s"$RESOURCE_DIR/result"

/** package name */
val PACKAGE_NAME = "esmeta"

/** tests directory */
val IR_TEST_DIR = s"$TEST_DIR/ir"
val ES_TEST_DIR = s"$TEST_DIR/es"
val TEST262_DIR = s"$TEST_DIR/test262"
val TEST262_TEST_DIR = s"$TEST262_DIR/test"

/** error stack trace display mode */
var ERROR_MODE = false

/** exit status return mode */
var STATUS_MODE = false

// -----------------------------------------------------------------------------
// Mutable Global Options
// -----------------------------------------------------------------------------
/** test mode (turn it on only when executing tests) */
var TEST_MODE: Boolean = false

"use strict";
let x ; x = 0 ?. [ await ] || x ; class await { } 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:OptionalChain[1,0].ChainEvaluation) but got transpile-failure */

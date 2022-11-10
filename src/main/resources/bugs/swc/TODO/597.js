"use strict";
0 > { [ Symbol . toPrimitive ] : x => await } ; class await { } 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ExpressionBody[0,0].Evaluation) but got transpile-failure */

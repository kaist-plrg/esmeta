"use strict";
String . prototype . normalize ( { [ Symbol . toPrimitive ] : x => await . x %= 0 } ) ; 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(GetValue ((step 3, 4:57-92))<SYNTAX>:MemberExpression[2,0].Evaluation) but got transpile-failure */

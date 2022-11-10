"use strict";
class x { } x &&= await ; 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(GetValue ((step 3, 4:57-92))<SYNTAX>:AssignmentExpression[6,0].Evaluation) but got transpile-failure */

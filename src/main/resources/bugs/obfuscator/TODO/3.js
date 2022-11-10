"use strict";
let x ; [ x = await ] = `` ; 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(GetValue ((step 3, 4:57-92))<SYNTAX>:AssignmentElement[0,1].IteratorDestructuringAssignmentEvaluation) but got transpile-failure */

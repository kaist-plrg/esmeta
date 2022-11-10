"use strict";
let x ; [ x = await ] = '' ; class await { } 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:AssignmentElement[0,1].IteratorDestructuringAssignmentEvaluation) but got transpile-failure */

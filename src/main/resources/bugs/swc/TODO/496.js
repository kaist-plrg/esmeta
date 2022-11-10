"use strict";
let x ; [ { } = x ] = `` ; 

/* TAG: NEW-SWC-DESTRUCTURING-ASSIGN-EXPR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(RequireObjectCoercible<SYNTAX>:ObjectAssignmentPattern[0,0].DestructuringAssignmentEvaluation) but got normal */

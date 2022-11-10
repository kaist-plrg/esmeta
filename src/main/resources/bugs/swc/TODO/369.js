"use strict";
async function * x ( ) { } [ { } ] = { [ Symbol . iterator ] : x } ; 

/* TAG: NEW-DSG-HIDDEN-GEN-PARAM-ERROR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(RequireObjectCoercible<SYNTAX>:ObjectAssignmentPattern[0,0].DestructuringAssignmentEvaluation) but got normal */

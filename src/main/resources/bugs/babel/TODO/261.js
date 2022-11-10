"use strict";
for ( var x of function * x ( ) { yield * x ; let x ; } ( ) ) ; 

/* TAG: NEW-DSG-HIDDEN-GEN-PARAM-ERROR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:YieldExpression[2,0].Evaluation) but got normal */

"use strict";
for ( let x of { [ Symbol . iterator ] : function * ( ) { yield * x -- ; } } ) ; 

/* TAG: NEW-DSG-HIDDEN-GEN-PARAM-ERROR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:UpdateExpression[2,0].Evaluation) but got normal */

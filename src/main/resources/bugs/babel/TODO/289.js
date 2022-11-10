"use strict";
for ( let x of { [ Symbol . iterator ] : function * ( ) { return yield x ; } } ) ; 

/* TAG: NEW-DSG-HIDDEN-GEN-PARAM-ERROR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:YieldExpression[1,0].Evaluation) but got normal */

"use strict";
for ( let x of { [ Symbol . iterator ] : function * ( ) { yield * `` [ 0 , x ] ; } } ) ; 

/* TAG: NEW-DSG-HIDDEN-GEN-PARAM-ERROR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:Expression[1,0].Evaluation) but got normal */

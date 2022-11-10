"use strict";
for ( let x of { [ Symbol . iterator ] : function * ( ) { yield * 0 ?. x ( x , ) ; } } ) ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ArgumentList[0,0].ArgumentListEvaluation) but got throw-error: TypeError(unnamed:158: TypeError: ref.x is not a function) */

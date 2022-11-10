"use strict";
for ( let x of { [ Symbol . iterator ] : function * ( ) { yield * x -- ; } } ) ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:UpdateExpression[2,0].Evaluation) but got throw-error: TypeError(unnamed:157: TypeError: Object is not iterable.) */

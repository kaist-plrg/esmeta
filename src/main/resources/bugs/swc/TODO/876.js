"use strict";
let x = { get 0 ( ) { x ( ) ; } , } [ 0 ] ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:CallExpression[0,0].Evaluation) but got throw-error: TypeError(unnamed:6: TypeError: x is not a function) */

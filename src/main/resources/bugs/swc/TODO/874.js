"use strict";
let x = 0 % 0 !== x ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:EqualityExpression[4,0].Evaluation) but got normal */

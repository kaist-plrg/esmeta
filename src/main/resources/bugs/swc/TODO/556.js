"use strict";
let x = { [ 0 || 0 ] : x } <= 0 ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:PropertyDefinition[2,0].PropertyDefinitionEvaluation) but got normal */

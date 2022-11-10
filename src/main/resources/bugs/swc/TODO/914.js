"use strict";
let x = { x = { x } } = 0 ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:PropertyDefinition[0,0].PropertyDefinitionEvaluation) but got normal */

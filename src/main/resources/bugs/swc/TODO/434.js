"use strict";
for ( let x of { [ 0 ] : x } ) ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:PropertyDefinition[2,0].PropertyDefinitionEvaluation) but got throw-error: TypeError(unnamed:33: TypeError: _defineProperty(...)[Symbol.iterator] is not a function) */

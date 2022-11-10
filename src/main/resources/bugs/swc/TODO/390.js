"use strict";
new function ( { x } = x , ) { } ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:BindingElement[1,1].IteratorBindingInitialization) but got throw-error: TypeError(unnamed:5: TypeError: Cannot read properties of undefined (reading 'x')) */

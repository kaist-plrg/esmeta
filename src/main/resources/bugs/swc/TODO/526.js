"use strict";
var [ [ ] = x ] = '' ; let x ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:BindingElement[1,1].IteratorBindingInitialization) but got throw-error: TypeError(unnamed:37: TypeError: Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.) */

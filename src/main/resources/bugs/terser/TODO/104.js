"use strict";
let x ; [ function ( ) { return x ; let x ; } ( ) [ 1 ] = 0 ] = `` ; 

/* TAG: NEW-YET-TRS-REMOVE-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ReturnStatement[1,0].Evaluation) but got throw-error: TypeError(unnamed:3: TypeError: Cannot set properties of undefined (setting '1')) */

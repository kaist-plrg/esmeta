"use strict";
let x = { get 0 ( ) { throw x ; } } [ 0 ] ??= 0 ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ThrowStatement[0,0].Evaluation) but got throw-value: "unnamed:8: undefined
        throw x;
        ^
" */

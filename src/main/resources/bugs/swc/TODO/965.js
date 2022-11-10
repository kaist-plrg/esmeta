"use strict";
class x { static { throw x ; let x ; } } 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ThrowStatement[0,0].Evaluation) but got throw-value: "unnamed:15: undefined
        throw x;
        ^
" */

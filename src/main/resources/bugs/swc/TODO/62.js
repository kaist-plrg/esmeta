"use strict";
throw x ; let x ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ThrowStatement[0,0].Evaluation) but got throw-value: "unnamed:4: undefined
throw x;
^
" */

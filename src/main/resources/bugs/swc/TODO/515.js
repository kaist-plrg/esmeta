"use strict";
class x extends await { ; } class await { } 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ClassTail[0,3].ClassDefinitionEvaluation) but got throw-error: TypeError(unnamed:23: TypeError: Super expression must either be null or a function) */

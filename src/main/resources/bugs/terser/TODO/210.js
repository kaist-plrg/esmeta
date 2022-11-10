"use strict";
! class extends x { ; } ; let x ; 

/* TAG: NEW-YET-TRS-REMOVE-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ClassTail[0,3].ClassDefinitionEvaluation) but got normal */

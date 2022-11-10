"use strict";
switch ( 0 ) { default : case x : let x ; } 

/* TAG: NEW-YET-TRS-REMOVE-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:CaseBlock[1,1].CaseBlockEvaluation) but got normal */

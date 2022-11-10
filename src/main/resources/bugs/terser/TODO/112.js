"use strict";
0 ( `${ 0 }` [ 0 ] &&= x ) ; let x ; 

/* TAG: NEW-YET-TRS-REMOVE-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:AssignmentExpression[6,0].Evaluation) but got throw-error: TypeError(unnamed:3: TypeError: Cannot assign to read only property '0' of string '0') */

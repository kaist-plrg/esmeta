"use strict";
for ( let x in delete new x ( 0 ) ) ; 

/* TAG: NEW-GRL-DELETE
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:MemberExpression[6,0].Evaluation) but got normal */

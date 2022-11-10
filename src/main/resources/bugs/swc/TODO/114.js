"use strict";
for ( let x in -- x ) ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:UpdateExpression[4,0].Evaluation) but got normal */

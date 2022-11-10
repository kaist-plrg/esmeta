"use strict";
class x { static 0 = x = 0 ; } 

/* TAG: NEW-SWC-STATIC-REASSIGN-CLASS
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:AssignmentExpression[4,0].Evaluation) but got normal */

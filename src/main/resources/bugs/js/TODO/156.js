"use strict";
class x { static 0 = x &&= [ { } ] [ 0 ] -- ; } 

/* TAG: NEW-GRL-LOGICAL-ASSIGN-CONST
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:AssignmentExpression[6,0].Evaluation) but got normal */

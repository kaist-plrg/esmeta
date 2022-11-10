"use strict";
class x { static 0 = x = 0 ; } 

/* TAG: NEW-BBL-STATIC-REASSIGN-CLASS
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:AssignmentExpression[4,0].Evaluation) but got throw-error: ReferenceError(unnamed:13: ReferenceError: x is not defined - temporal dead zone) */

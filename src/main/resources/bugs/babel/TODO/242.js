"use strict";
class x { static 0 = super [ ++ x ** 0 ] ; } 

/* TAG: NEW-BBL-STATIC-REASSIGN-CLASS
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:UpdateExpression[3,0].Evaluation) but got throw-error: ReferenceError(unnamed:19: ReferenceError: x is not defined - temporal dead zone) */

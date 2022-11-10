"use strict";
class x { static 0 = super [ ++ x ** 0 ] ; } 

/* TAG: NEW-SWC-STATIC-REASSIGN-CLASS
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:UpdateExpression[3,0].Evaluation) but got normal */

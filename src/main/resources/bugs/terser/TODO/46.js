"use strict";
class x { set [ #x in 0 >> x !== 0 ^ 0 <= 0 ?? 0 ] ( x ) { } get #x ( ) { } } 

/* TAG: NEW-PRIVATE-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ShiftExpression[2,0].Evaluation) but got transpile-failure */

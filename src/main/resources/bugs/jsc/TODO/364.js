"use strict";
class x { static 0 = super [ [ x &= x | 0 , ] [ 0 ] ??= 0 ] ; } 

/* TAG: NEW-JSC-STATIC-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:AssignmentExpression[5,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Left hand side of operator '??=' must be a reference.) */

"use strict";
class x { [ `${ x , 0 }` [ 0 ] ??= 0 ] ; } 

/* TAG: NEW-JSC-TMP-LITERAL-LHS
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:Expression[1,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Left hand side of operator '??=' must be a reference.) */

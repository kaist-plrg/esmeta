"use strict";
for ( x in [ ! 0 [ 0 ] . x , ] [ 0 ] &&= 0 ) ; 

/* TAG: NEW-JSC-ARR-LITERAL-LHS
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:UnaryExpression[7,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Left hand side of operator '&&=' must be a reference.) */

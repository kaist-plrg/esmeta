"use strict";
do ; while ( [ ~ { [ Symbol . toPrimitive ] : { 1 : '' } } , ] [ 0 ] &&= 0 ) ; 

/* TAG: NEW-JSC-ARR-LITERAL-LHS
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(GetMethod ((step 3, 4:46-76))<SYNTAX>:UnaryExpression[6,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Left hand side of operator '&&=' must be a reference.) */

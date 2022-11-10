"use strict";
do ; while ( [ ~ { [ Symbol . toPrimitive ] : async function ( x ) { } } , ] [ 0 ] &&= 0 ) ; 

/* TAG: NEW-JSC-ARR-LITERAL-LHS
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToPrimitive ((step 1.b.vi, 12:16-46))<SYNTAX>:UnaryExpression[6,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Left hand side of operator '&&=' must be a reference.) */

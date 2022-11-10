"use strict";
let x = `${ [ 0 ] [ 0 ] %= 1 }` [ 0 ] &&= x => 0 ; 

/* TAG: NEW-JSC-TMP-LITERAL-LHS
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(PutValue ((step 5.d, 15:72-102))<SYNTAX>:AssignmentExpression[6,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Left hand side of operator '&&=' must be a reference.) */

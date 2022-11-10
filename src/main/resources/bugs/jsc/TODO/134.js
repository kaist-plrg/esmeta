"use strict";
for ( let x of [ - + 0 , ] [ 0 ] -= 0 ) ; 

/* TAG: NEW-JSC-ARR-LITERAL-LHS
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Call ((step 2, 3:43-73))<SYNTAX>:ForInOfStatement[5,0].ForInOfLoopEvaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Left hand side of operator '-=' must be a reference.) */

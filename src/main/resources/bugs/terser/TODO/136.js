"use strict";
( x => 0 ( ) ?. x ?. x ) ( ) . x -- ; 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(EvaluateCall ((step 4, 12:45-75))<SYNTAX>:CallExpression[0,0].Evaluation) but got throw-error: SyntaxError(unnamed:3: SyntaxError: Invalid left-hand side expression in postfix operation) */

"use strict";
async function x ( ) { ; for await ( [ , 0 , ] [ 1 ] of [ 0 ] ) ; } x ( ) ; 

/* TAG: NEW-TRS-LHS-TO-LITERAL
[Exit Tag Mismatch]
 > Expected normal but got throw-error: SyntaxError(unnamed:2: SyntaxError: Invalid left-hand side in for-loop) */

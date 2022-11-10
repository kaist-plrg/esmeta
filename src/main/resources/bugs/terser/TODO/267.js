"use strict";
async function x ( ) { ; for await ( [ , ] [ 0 ] of [ 0 ] ) ; } x ( ) ; 

/* TAG: NEW-TRS-LHS-TO-LITERAL
[Exit Tag Mismatch]
 > Expected normal but got throw-error: SyntaxError(unnamed:2: SyntaxError: Unexpected token 'void') */

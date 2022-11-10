"use strict";
for ( let x of { [ Symbol . iterator ] : async function * ( ) { for await ( [ , ] [ 0 ] of [ 0 ] ) ; } } ) break ; 

/* TAG: NEW-TRS-LHS-TO-LITERAL
[Exit Tag Mismatch]
 > Expected normal but got throw-error: SyntaxError(unnamed:2: SyntaxError: Unexpected token 'void') */

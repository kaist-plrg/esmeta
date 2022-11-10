"use strict";
for ( [ , ] of [ { [ Symbol . iterator ] : async function * ( ) { return yield * `${ 0 }${ yield * ! 0 }` [ 0 ] %= 0 ; } } , ] ) ; 

/* TAG: NEW-JSC-TMP-LITERAL-LHS
[Exit Tag Mismatch]
 > Expected normal but got throw-error: SyntaxError(Exception: SyntaxError: Unexpected token '*'. Expected a ';' following a return statement.) */

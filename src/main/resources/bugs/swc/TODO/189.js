"use strict";
var [ , ] = { [ Symbol . iterator ] : async function * ( ) { for await ( 0 [ yield ] of [ 0 ] ) ; } } ; 

/* TAG: NEW-JSC-IN-INSIDE-FOR-DESTR?
[Exit Tag Mismatch]
 > Expected normal but got throw-error: SyntaxError(unnamed:319: SyntaxError: Unexpected strict mode reserved word) */

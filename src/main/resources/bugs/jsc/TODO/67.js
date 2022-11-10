"use strict";
for ( [ , ] of [ { [ Symbol . iterator ] : async function * x ( ) { yield * x = yield * { [ Symbol . iterator ] : async function * x ( { ... x } ) { } } ; } } , ] ) ; 

/* TAG: NEW-JSC-FUNC-PARAM-SAME-NAME
[Exit Tag Mismatch]
 > Expected normal but got throw-error: SyntaxError(Exception: SyntaxError: Unexpected token '*') */

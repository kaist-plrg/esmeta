"use strict";
[ , ] = { [ Symbol . iterator ] : async function * ( ) { return { then : x => await } ; } } ; 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected normal but got transpile-failure */

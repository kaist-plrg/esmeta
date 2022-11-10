"use strict";
[ , ] = { [ Symbol . iterator ] : async function * ( ) { return { then : yield } ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

"use strict";
[ , ] = { [ Symbol . iterator ] : async function * ( ) { yield * 0n ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

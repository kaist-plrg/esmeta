"use strict";
[ , ] = { [ Symbol . iterator ] : async function * ( ) { yield * 1n . x ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

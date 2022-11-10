"use strict";
[ , ] = { [ Symbol . iterator ] : async function * ( ) { yield * 0 . x . x ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

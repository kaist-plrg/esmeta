"use strict";
[ , ] = { [ Symbol . iterator ] : async function * ( ) { yield * null . x ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

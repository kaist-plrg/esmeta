"use strict";
[ , ] = { [ Symbol . iterator ] : async function * ( ) { yield * true . x ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

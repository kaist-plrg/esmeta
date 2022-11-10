"use strict";
[ , ] = { [ Symbol . iterator ] : async function * ( ) { yield * x ( ) ; } } ; async function * x ( ) { } 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

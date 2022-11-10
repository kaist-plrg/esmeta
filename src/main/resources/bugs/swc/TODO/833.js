"use strict";
[ , ] = { [ Symbol . iterator ] : async function * ( ) { yield * `` ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

"use strict";
[ , ] = { [ Symbol . iterator ] : async function * ( ) { yield * { [ Symbol . iterator ] : [ ] } ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

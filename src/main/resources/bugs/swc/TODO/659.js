"use strict";
[ , , ] = { [ Symbol . iterator ] : async function * ( ) { return 0 ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

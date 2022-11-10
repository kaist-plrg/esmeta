"use strict";
[ , ] = { [ Symbol . iterator ] : async function * ( ) { return [ ] ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

"use strict";
[ , ] = { [ Symbol . iterator ] : async function * ( ) { yield * [ { then : ( ) => { throw 0 ; } } ] ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

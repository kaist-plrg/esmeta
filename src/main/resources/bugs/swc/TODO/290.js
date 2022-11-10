"use strict";
var [ , ] = { [ Symbol . iterator ] : async function * ( ) { yield { then : x => 0 } ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

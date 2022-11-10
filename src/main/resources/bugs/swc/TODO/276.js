"use strict";
var [ , ] = { [ Symbol . iterator ] : async function * ( ) { yield { then : class { } } ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

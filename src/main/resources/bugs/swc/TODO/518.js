"use strict";
[ , ] = { [ Symbol . iterator ] : async function * ( ) { return async function ( ) { } ( ) ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

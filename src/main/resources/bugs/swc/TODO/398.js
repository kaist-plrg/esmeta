"use strict";
var [ , ] = { [ Symbol . iterator ] : async function * ( ) { for await ( x of { [ Symbol . iterator ] : async function * ( ) { } } ) ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

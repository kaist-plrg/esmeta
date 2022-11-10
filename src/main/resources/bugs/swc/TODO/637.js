"use strict";
var [ , ] = { [ Symbol . iterator ] : async function * ( ) { for await ( { x = x } of [ 0 ] ) var x ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

"use strict";
var [ , ] = { [ Symbol . iterator ] : async function * ( ) { for await ( { x = 0 } of [ 0 ] ) ; let x ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

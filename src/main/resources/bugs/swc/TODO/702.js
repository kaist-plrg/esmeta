"use strict";
var [ , ] = { [ Symbol . iterator ] : async function * ( ) { for await ( x of [ async function ( ) { } ( ) ] ) ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

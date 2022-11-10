"use strict";
let x ; var [ , ] = { [ Symbol . iterator ] : async function * ( ) { for await ( x of x ) ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

"use strict";
var [ , ] = { [ Symbol . iterator ] : async function * ( ) { for await ( var [ x = 0 ] of [ '' ] ) ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

"use strict";
var [ , ] = { [ Symbol . iterator ] : async function * ( ) { for await ( var [ x = yield ] of [ '' ] ) ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

"use strict";
var [ , ] = { [ Symbol . iterator ] : async function * ( ) { yield * { [ Symbol . asyncIterator ] : function * ( ) { yield ; } } ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

"use strict";
var [ , ] = { [ Symbol . iterator ] : async function * ( ) { for await ( x of { [ Symbol . asyncIterator ] : function * ( ) { yield ; } } ) ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

"use strict";
var [ , ] = { [ Symbol . iterator ] : async function * ( ) { for await ( x of { [ Symbol . asyncIterator ] : function * ( ) { yield ; } } ) for ( var x of 0 ) ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */

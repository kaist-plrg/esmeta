var [ x ] = { [ Symbol . iterator ] : function * ( ) { yield * { [ Symbol . iterator ] : x => [ ] } ; } } ; 
var [ x ] = { [ Symbol . iterator ] : function * ( ) { yield * { [ Symbol . iterator ] : x => 0 } ; } } ; 
var [ , ] = { [ Symbol . iterator ] : function * ( ) { yield * { [ Symbol . iterator ] : async x => 0 } ; } } ;

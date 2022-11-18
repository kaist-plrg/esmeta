class x { [ { [ Symbol . toPrimitive ] : '' } ] = 0 ; } 
class x { [ { [ Symbol . toPrimitive ] : ( ) => { throw 0 ; } } ] ; } 
class x { [ { [ Symbol . toPrimitive ] : 0 } ] ; } 
class x { [ { [ Symbol . toPrimitive ] : async function * ( ) { } } ] ; } 
class x { [ { [ Symbol . toPrimitive ] : async x => 0 } ] ; } 
class x { [ { [ Symbol . toPrimitive ] : async x => { } } ] ; } 
class x { [ { [ Symbol . toPrimitive ] : class { } } ] ; } 
class x { [ { [ Symbol . toPrimitive ] : function * ( ) { } } ] ; } 
class x { [ { [ Symbol . toPrimitive ] : this } ] ; } 
class x { [ { [ Symbol . toPrimitive ] : x => [ ] } ] ; } 
class x { [ { [ Symbol . toPrimitive ] : x => await } ] ; } 
class x { [ { [ Symbol . toPrimitive ] : x => this } ] ; } 
class x { [ { [ Symbol . toPrimitive ] : x => { new 0 ; } } ] ; } 
class x { [ { [ Symbol . toPrimitive ] : { } } ] ; } 

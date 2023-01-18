 // class ID { static ID = ... delete super [ THROW ] ... ; } ;
 // new class ID? { ID = ... delete super [ THROW ] ... ; } ;
 ///
class x extends null { static 0 = delete super [ 0 . x . x ] ; }
class x { static 0 = delete super [ new 0 ] ; }
class x { static 0 = delete super [ this [ 0 ] [ 0 ] ] ; }
new class x { 0 = delete super [ 0 [ 0 ] . x ] ; } ; 
new class x { 0 = delete super [ new 0 ] ; } ; 
new class x { 0 = delete super [ null [ 0 ] ] ; } ; 
new class x { 0 = delete super [ { [ Symbol . toPrimitive ] : ( ) => { throw 0 ; } } ] ; } ; 
new class x { 0 = delete super [ { [ Symbol . toPrimitive ] : 0 } ] ; } ; 
new class x { 0 = delete super [ { [ Symbol . toPrimitive ] : async x => 0 } ] ; } ; 
new class x { 0 = delete super [ { [ Symbol . toPrimitive ] : x } ] ; } ; 
new class x { 0 = delete super [ { [ Symbol . toPrimitive ] : { } } ] ; } ; 
new class { 0 = super [ delete super [ 0 [ 0 ] [ 0 ] ] ] ; } ;
new class { 0 = super [ delete super [ new 0 ] ] ; } ;
new class { 0 = super [ delete super [ { [ Symbol . toPrimitive ] : '' } ] ] ; } ;
new class { 0 = super [ delete super [ { [ Symbol . toPrimitive ] : async function * ( ) { } } ] ] ; } ;
new class { 0 = super [ delete super [ { [ Symbol . toPrimitive ] : { } } ] ] ; } ;

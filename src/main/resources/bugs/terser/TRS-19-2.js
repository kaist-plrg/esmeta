// x ? <Number1> : <Number2>
// Number 1 and 2 should be arithmetically equivalent within scope
// -- result should <THROW>
//
// expands to:
// for (let x in x ? 0 : 0)
// for (let x of x ? 0 : 0)
// let x = x ? 0 : 0
for ( let x in ! x ? 0 : 0 ) ;
for ( let x in 0 !== { x } ? 0 : 0 ) ;
for ( let x of x ? 0 : 0 ) ;
for ( let x of { x } ? 0 : 0 ) ;
let [ x = x ? 0 : 0 ] = '' ;
let x = x ? 0 : 0 ;

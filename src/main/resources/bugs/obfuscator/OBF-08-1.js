// let [ x ] = function * ( ) { yield class x { } ; } ( ) ;
let [ x , , ... [ ] ] = function * ( ) { yield async function * x ( ) { } ; } ( ) ;
let [ x , , ... [ ] ] = function * ( ) { yield x || 0 ; function x ( ) { } } ( ) ;
let [ x ] = function * ( ) { yield async function x ( ) { } ; } ( ) ;
let [ x ] = function * ( ) { yield class x { } ; } ( ) ;
let [ x ] = function * ( ) { yield function * x ( ) { } ; } ( ) ;

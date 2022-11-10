"use strict";
let x = async x => { for await ( { x , } of [ 0 ] ) for ( 0 [ 0 , 0 ] in 0 ? 0 : 0 ) return 0 ; } ; x ( ) ; 

/* TAG: NEW-ASYNC-GENERATOR?
[Assertion Fail]
 > Expected the async function prototype but got undefined.
Expected true but got false.
Expected undefined has [[Call]] but does not.
$assert.compareArray requires an array as the first argument but undefined given.
$verifyProperty requires an object but undefined given.
$verifyProperty requires an object but undefined given. */

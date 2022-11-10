"use strict";
function * x ( ) { } var [ , ] = { [ Symbol . iterator ] : async function * ( ) { for await ( { x = 0 , } of [ { x } ] ) ; } } ; 

/* TAG: NEW-SM-PROP-ORDER
[Assertion Fail]
 > Expected ["length", "name", "prototype"] but got ["prototype", "length", "name"] for GeneratorFunction. */

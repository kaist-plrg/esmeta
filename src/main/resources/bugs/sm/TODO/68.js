"use strict";
function * x ( ) { var x ; } x ( ) ; 

/* TAG: NEW-SM-PROP-ORDER
[Assertion Fail]
 > Expected ["length", "name", "prototype"] but got ["prototype", "length", "name"] for GeneratorFunction. */

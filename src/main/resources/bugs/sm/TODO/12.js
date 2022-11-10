"use strict";
for ( var x of function * x ( ) { yield * [ x ] ; } ( ) ) ; 

/* TAG: NEW-SM-PROP-ORDER
[Assertion Fail]
 > Expected ["length", "name", "prototype"] but got ["prototype", "length", "name"] for GeneratorFunction. */

"use strict";
class await { } for ( var x of function * ( ) { yield await ; } ( ) ) ; 

/* TAG: NEW-SM-PROP-ORDER
[Assertion Fail]
 > Expected ["length", "name", "prototype"] but got ["prototype", "length", "name"] for Function. */

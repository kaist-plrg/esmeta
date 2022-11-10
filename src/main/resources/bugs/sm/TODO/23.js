"use strict";
class x { static 0 = super [ ++ x . x ** super . x ] ; } 

/* TAG: NEW-SM-PROP-ORDER
[Assertion Fail]
 > Expected ["0", "length", "name", "prototype", "x"] but got ["0", "prototype", "x", "length", "name"] for Function. */

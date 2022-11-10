"use strict";
class x { static 0 = super [ ++ super . x ** 0 ] ; } 

/* TAG: NEW-SM-PROP-ORDER
[Assertion Fail]
 > Expected ["0", "length", "name", "prototype", "x"] but got ["0", "prototype", "x", "length", "name"] for Function. */

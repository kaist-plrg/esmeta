"use strict";
class x { [ { [ Symbol . toPrimitive ] : x => 0 } ] ; } 

/* TAG: NEW-SM-PROP-ORDER
[Assertion Fail]
 > Expected ["length", "name", "prototype"] but got ["prototype", "length", "name"] for Function. */

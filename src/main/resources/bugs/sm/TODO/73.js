"use strict";
class x { static 0 = super [ { [ 0 ?? 0 ] : x && 0 } > x ] ; } 

/* TAG: NEW-SM-PROP-ORDER
[Assertion Fail]
 > Expected ["0", "length", "name", "prototype"] but got ["0", "prototype", "length", "name"] for Function. */

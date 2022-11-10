"use strict";
class x { static 0 = super [ { [ Symbol . toPrimitive ] : x => 0 } > x ] ; } 

/* TAG: NEW-SWC-CHAIN-COMP-ASSIGN-SUPER-IN-STATIC
[Exit Tag Mismatch]
 > Expected normal but got throw-error: ReferenceError(unnamed:22: ReferenceError: x is not defined - temporal dead zone) */

"use strict";
class x { static 0 = x . x ; } 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected normal but got throw-error: ReferenceError(unnamed:12: ReferenceError: x is not defined - temporal dead zone) */

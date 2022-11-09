"use strict";
class x { static #x = #x in x ; } 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected normal but got throw-error: ReferenceError(unnamed:10: ReferenceError: x is not defined - temporal dead zone) */

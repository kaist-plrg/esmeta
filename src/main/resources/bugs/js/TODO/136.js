"use strict";
let { x } = typeof '' [ 0 ] [ { [ Symbol . toPrimitive ] : ( ) => { throw 0 ; } } ] ; 

/* TAG: NEW-ALL-TO-PRIMITIVE
[Exit Tag Mismatch]
 > Expected throw-value: 0.0f but got throw-error: TypeError(TypeError: Cannot convert undefined or null to object: undefined) */

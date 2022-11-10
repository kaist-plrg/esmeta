"use strict";
let { x } = typeof '' [ 0 ] [ { [ Symbol . toPrimitive ] : ( ) => { throw 0 ; } } ] ; 

/* TAG: NEW-ALL-TO-PRIMITIVE
[Exit Tag Mismatch]
 > Expected throw-value: 0.0f but got throw-error: TypeError(Exception: TypeError: undefined is not an object (evaluating ''' [ 0 ] [ { [ Symbol . toPrimitive ] : ( ) => { throw 0 ; } } ]')) */

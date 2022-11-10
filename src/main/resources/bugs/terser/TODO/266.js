"use strict";
[ { [ Symbol . toPrimitive ] : ( ) => { throw 0 ; } } ] >= { } ; 

/* TAG: NEW-YET-TRS-REMOVE-THROW
[Exit Tag Mismatch]
 > Expected throw-value: 0.0f but got normal */

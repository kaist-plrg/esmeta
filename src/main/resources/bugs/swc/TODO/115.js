"use strict";
class x { [ { [ Symbol . toPrimitive ] : x => { } } ] ; } 

/* TAG: NEW-SWC-WRITABLE-CLASS-PROTOTYPE
[Assertion Fail]
 > descriptor should not be writable */

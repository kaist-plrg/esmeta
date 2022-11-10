"use strict";
class x { [ { [ Symbol . toPrimitive ] : function ( ) { } } ] ; } 

/* TAG: NEW-SWC-WRITABLE-CLASS-PROTOTYPE
[Assertion Fail]
 > descriptor should not be writable */

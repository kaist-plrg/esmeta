"use strict";
class x { static { let x ; } } 

/* TAG: NEW-SWC-WRITABLE-CLASS-PROTOTYPE
[Assertion Fail]
 > descriptor should not be writable */

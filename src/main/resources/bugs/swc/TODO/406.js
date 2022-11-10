"use strict";
var { x = class x { } } = 0 ; 

/* TAG: NEW-SWC-WRITABLE-CLASS-PROTOTYPE
[Assertion Fail]
 > descriptor should not be writable */

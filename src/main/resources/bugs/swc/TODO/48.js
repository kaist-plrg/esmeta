"use strict";
class x { set #x ( x ) { } } 

/* TAG: NEW-SWC-WRITABLE-CLASS-PROTOTYPE
[Assertion Fail]
 > descriptor should not be writable */

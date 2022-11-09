"use strict";
class x { } x ||= 0 ; 

/* TAG: NEW-SWC-WRITABLE-CLASS-PROTOTYPE
[Assertion Fail]
 > descriptor should not be writable */

"use strict";
class x { } switch ( 0 ) { case x : } 

/* TAG: NEW-SWC-WRITABLE-CLASS-PROTOTYPE
[Assertion Fail]
 > descriptor should not be writable */

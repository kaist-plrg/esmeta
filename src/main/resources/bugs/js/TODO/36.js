"use strict";
class x { static 0 = [ this , ] [ 0 ] += null ; } 

/* TAG: NEW-NATIVE-CODE
[Assertion Fail]
 > descriptor value of "0" should be "function x() { [native code] }null" but "class x { static 0 = [ this , ] [ 0 ] += null ; }null" */

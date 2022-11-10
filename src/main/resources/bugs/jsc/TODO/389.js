"use strict";
class x { static 0 = x + 1n ; } 

/* TAG: NEW-NATIVE-CODE
[Assertion Fail]
 > descriptor value of "0" should be "function x() { [native code] }1" but "class x { static 0 = x + 1n ; }1" */

"use strict";
class x { static 0 = x . x + x ; } 

/* TAG: NEW-NATIVE-CODE
[Assertion Fail]
 > descriptor value of "0" should be "undefinedfunction x() { [native code] }" but "undefinedclass x { static 0 = x . x + x ; }" */

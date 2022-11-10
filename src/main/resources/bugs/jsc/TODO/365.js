"use strict";
class x { static 0 = super [ 0 < 0 ] + x ; } 

/* TAG: NEW-NATIVE-CODE
[Assertion Fail]
 > descriptor value of "0" should be "undefinedfunction x() { [native code] }" but "undefinedclass x { static 0 = super [ 0 < 0 ] + x ; }" */

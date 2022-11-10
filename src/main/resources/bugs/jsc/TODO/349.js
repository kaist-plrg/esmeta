"use strict";
class x { static 0 = null + x ; } 

/* TAG: NEW-NATIVE-CODE
[Assertion Fail]
 > descriptor value of "0" should be "nullfunction x() { [native code] }" but "nullclass x { static 0 = null + x ; }" */

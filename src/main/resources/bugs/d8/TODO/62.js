"use strict";
class x { static 0 = ! 0 + x ; } 

/* TAG: NEW-NATIVE-CODE
[Assertion Fail]
 > descriptor value of "0" should be "truefunction x() { [native code] }" but "trueclass x { static 0 = ! 0 + x ; }" */

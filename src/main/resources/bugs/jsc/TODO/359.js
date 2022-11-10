"use strict";
class x { static 0 = 0 + x ; } 

/* TAG: NEW-NATIVE-CODE
[Assertion Fail]
 > descriptor value of "0" should be "0function x() { [native code] }" but "0class x { static 0 = 0 + x ; }" */

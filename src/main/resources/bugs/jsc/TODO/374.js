"use strict";
class x { static 0 = { x } . x + x ; } 

/* TAG: NEW-NATIVE-CODE
[Assertion Fail]
 > descriptor value of "0" should be "function x() { [native code] }function x() { [native code] }" but "class x { static 0 = { x } . x + x ; }class x { static 0 = { x } . x + x ; }" */

"use strict";
class x { static 0 = ! typeof ~ x + x ; } 

/* TAG: NEW-NATIVE-CODE
[Assertion Fail]
 > descriptor value of "0" should be "falsefunction x() { [native code] }" but "falseclass x { static 0 = ! typeof ~ x + x ; }" */

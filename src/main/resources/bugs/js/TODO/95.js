"use strict";
class x { static 0 = `${ x }` || 0 ; } 

/* TAG: NEW-NATIVE-CODE
[Assertion Fail]
 > descriptor value of "0" should be "function x() { [native code] }" but "class x { static 0 = `${ x }` || 0 ; }" */

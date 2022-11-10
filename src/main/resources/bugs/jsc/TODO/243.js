"use strict";
class x { static 0 = { [ Symbol ] : 0 } ; } 

/* TAG: NEW-NATIVE-CODE
[Assertion Fail]
 > Expected ["function Symbol() { [native code] }"] but got ["function Symbol() {
    [native code]
}"] for Object.
[object Object] does not have own property named "function Symbol() { [native code] }" */

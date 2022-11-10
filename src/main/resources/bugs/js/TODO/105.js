"use strict";
class x { static 0 = super [ { x } . x ] -- ; } 

/* TAG: NEW-NATIVE-CODE
[Assertion Fail]
 > Expected ["0", "length", "name", "prototype", "function x() { [native code] }"] but got ["0", "length", "name", "prototype", "class x { static 0 = super [ { x } . x ] -- ; }"] for Function.
[object Function] does not have own property named "function x() { [native code] }" */

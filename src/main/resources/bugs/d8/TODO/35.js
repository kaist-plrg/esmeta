"use strict";
class x { static { if ( ++ super [ { x } ?. x ] - x ) ; else ; } } 

/* TAG: NEW-NATIVE-CODE
[Assertion Fail]
 > Expected ["length", "name", "prototype", "function x() { [native code] }"] but got ["length", "name", "prototype", "class x { static { if ( ++ super [ { x } ?. x ] - x ) ; else ; } }"] for Function.
[object Function] does not have own property named "function x() { [native code] }" */

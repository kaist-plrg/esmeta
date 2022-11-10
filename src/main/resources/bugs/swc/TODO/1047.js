"use strict";
class x { static x = x => 0 ; } 

/* TAG: NEW-NAME
[Assertion Fail]
 > descriptor should not be writable
Expected [object Function] does not have [[Construct]] but does.
Expected ["length", "name"] but got ["length", "name", "prototype"] for Function.
descriptor value of "name" should be "x" but "" */

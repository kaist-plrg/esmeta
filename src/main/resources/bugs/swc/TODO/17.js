"use strict";
class x { } x &&= x => 0 ; 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Assertion Fail]
 > Expected [object Function] does not have [[Construct]] but does.
Expected ["length", "name"] but got ["length", "name", "prototype"] for Function. */

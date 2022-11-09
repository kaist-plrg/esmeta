"use strict";
class x { static 0 = x => 0 ; } 

/* TAG: NEW-NAME
[Assertion Fail]
 > Expected [object Function] does not have [[Construct]] but does.
descriptor value of "name" should be "0" but "bound " */

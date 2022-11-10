"use strict";
class x { static #x = #x in [ super . x ] ; } 

/* TAG: NEW-PRIVATE-FAIL
[Exit Tag Mismatch]
 > Expected normal but got transpile-failure */

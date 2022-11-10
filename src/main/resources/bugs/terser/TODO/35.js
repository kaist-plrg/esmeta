"use strict";
class x { static #x = #x in [ 0 . #x ] ; } 

/* TAG: NEW-PRIVATE-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(PrivateGet ((step 2, 3:35-65))<SYNTAX>:ElementList[0,0].ArrayAccumulation) but got transpile-failure */

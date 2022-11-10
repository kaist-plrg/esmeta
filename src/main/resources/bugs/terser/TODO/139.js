"use strict";
class x { static #x = #x in [ function ( ) { return `` ; } ( ) [ 0 ] . #x ] ; } 

/* TAG: NEW-PRIVATE-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:ElementList[0,0].ArrayAccumulation) but got transpile-failure */

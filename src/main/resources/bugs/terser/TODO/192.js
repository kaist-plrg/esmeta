"use strict";
class x { static #x = #x in [ function ( ) { } ( ) [ x ] . #x ] ; } 

/* TAG: NEW-PRIVATE-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:CallExpression[7,0].Evaluation) but got transpile-failure */

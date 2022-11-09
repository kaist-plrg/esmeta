"use strict";
var x = ( ... [ x , , ... [ ] ] ) => 0 ; x ( ) ; 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Assertion Fail]
 > Expected [object Function] does not have [[Construct]] but does.
Expected ["length", "name"] but got ["length", "name", "prototype"] for Function. */

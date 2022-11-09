"use strict";
class x { x ( ) { } } 

/* TAG: NEW-SWC-WRITABLE-CLASS-PROTOTYPE
[Assertion Fail]
 > Expected [object Function] does not have [[Construct]] but does.
Expected ["length", "name"] but got ["length", "name", "prototype"] for Function.
descriptor should not be writable */

"use strict";
class x { get 0 ( ) { } } 

/* TAG: NEW-SWC-WRITABLE-CLASS-PROTOTYPE
[Assertion Fail]
 > Expected [object Function] does not have [[Construct]] but does.
Expected ["length", "name"] but got ["length", "name", "prototype"] for Function.
descriptor value of "name" should be "get 0" but "get"
descriptor should not be writable */

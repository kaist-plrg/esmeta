"use strict";
class x { set 0 ( x = 0 ) { } get 0 ( ) { } } 

/* TAG: NEW-NAME
[Assertion Fail]
 > Expected [object Function] does not have [[Construct]] but does.
Expected ["length", "name"] but got ["length", "name", "prototype"] for Function.
descriptor value of "name" should be "get 0" but "get"
Expected [object Function] does not have [[Construct]] but does.
Expected ["length", "name"] but got ["length", "name", "prototype"] for Function.
descriptor value of "name" should be "set 0" but "set"
descriptor should not be writable */

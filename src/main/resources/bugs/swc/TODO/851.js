"use strict";
class x { get 0 ( ) { } async 0 ( ) { } } 

/* TAG: NEW-NAME
[Assertion Fail]
 > Expected the async function prototype but got [object Function].
Expected [object Function] does not have [[Construct]] but does.
Expected ["length", "name"] but got ["length", "name", "prototype"] for Function.
descriptor value of "name" should be "0" but "value"
descriptor should not be writable */

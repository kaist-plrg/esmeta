"use strict";
class x { static get 0 ( ) { } static set 0 ( x ) { } } 

/* TAG: NEW-JSC-GETSET-0
[Assertion Fail]
 > descriptor value of "name" should be "get 0" but "get "
descriptor value of "name" should be "set 0" but "set " */

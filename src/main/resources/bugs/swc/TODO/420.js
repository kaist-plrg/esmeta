"use strict";
class x { static 0 = class extends x { } ; } 

/* TAG: NEW-NAME
[Assertion Fail]
 > descriptor should not be writable
descriptor should not be writable
descriptor value of "name" should be "0" but "_class" */

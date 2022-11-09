"use strict";
class x extends class { } { } 

/* TAG: NEW-SWC-WRITABLE-CLASS-PROTOTYPE
[Assertion Fail]
 > descriptor should not be writable
descriptor value of "name" should be "" but "_class"
descriptor should not be writable */

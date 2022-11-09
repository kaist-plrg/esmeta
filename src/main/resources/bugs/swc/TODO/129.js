"use strict";
class x extends class { } { } new x ( ) ; 

/* TAG: NEW-NAME
[Assertion Fail]
 > descriptor should not be writable
descriptor value of "name" should be "" but "_class"
descriptor should not be writable */

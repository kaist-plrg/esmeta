"use strict";
var x = { x } = { set x ( x ) { } } ; 

/* TAG: NEW-SWC-SETTER-RENAME
[Assertion Fail]
 > Expected the object prototype but got undefined.
Expected true but got false.
$assert.compareArray requires an array as the first argument but undefined given.
An exception occured while checking assertions */

"use strict";
switch ( 1 ) { case { x } = 0 : default : var x ; } 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected normal but got throw-error: ReferenceError(unnamed:2: ReferenceError: x is not defined) */

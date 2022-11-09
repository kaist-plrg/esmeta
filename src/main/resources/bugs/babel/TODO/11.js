"use strict";
eval ( `${ x => async function ( ) { } }` ) ; 

/* TAG: NEW-F2STRING
[Exit Tag Mismatch]
 > Expected normal but got throw-error: SyntaxError(undefined:1: SyntaxError: Function statements require a function name) */

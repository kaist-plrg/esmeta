"use strict";
function x ( ) { } [ ... x ] ; 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Call ((step 2, 3:43-73))<SYNTAX>:SpreadElement[0,0].ArrayAccumulation) but got normal */

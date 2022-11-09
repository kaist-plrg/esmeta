"use strict";
class x { static { super . x = 0 ; } } 

/* TAG: NEW-SWC-STATIC-SUPER
[Exit Tag Mismatch]
 > Expected normal but got throw-error: SyntaxError(unnamed:8: SyntaxError: 'super' keyword unexpected here) */

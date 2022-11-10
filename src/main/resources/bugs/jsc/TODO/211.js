"use strict";
class x { static 0 = `${ 0 , 0 }` [ 0 ] ??= x => 0 ; } 

/* TAG: NEW-JSC-STATIC-SAME-NAME
[Exit Tag Mismatch]
 > Expected normal but got throw-error: SyntaxError(Exception: SyntaxError: Left hand side of operator '??=' must be a reference.) */

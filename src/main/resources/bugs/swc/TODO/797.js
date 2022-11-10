"use strict";
async function x ( ) { for await ( x of [ , ] ) break ; } x ( ) ; 

/* TAG: NEW-CASE-SCOPE
[Assertion Fail]
 > Expected undefined but got [object Function]. */

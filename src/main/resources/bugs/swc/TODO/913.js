"use strict";
async function x ( ) { for await ( x of [ , ] ) return ; } x ( ) ; 

/* TAG: NEW-CASE-SCOPE
[Assertion Fail]
 > Expected undefined but got [object Function]. */

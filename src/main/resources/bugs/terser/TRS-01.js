class x { static #x = #x in 0 ; } 
class x { static #x = #x in 0 ?. #x ; } 
class x { static #x = #x in 0 ?. x . #x ; } 
class x { static #x = #x in x ++ ; } 
class x { static #x = #x in x ; } 
class x { static #x = #x in x ?. #x . #x ; } 
class x { static #x = #x in { x } ?. x ?. x ( ) ; } 
class x { static #x = #x in { x } ?. x ?. x ; } 
class x { static #x = 0 || 0 | 0 !== #x in 0 ?. ( ) . #x ; } 

// <context:SWITCH>{ case new <NON-CONSTRUCTOR> , 0 : }
// <context:SWITCH>{ case <FREE-ID>, 0 : }
// <context:SWITCH>{ case { <FREE-ID> }, 0 : }
// <context:SWITCH>{ case null [<ANY>], 0 : }
//
// <context:SWITCH>{ case <THROW>, 0 : }
//
switch ( 0 ) { case new 0 , 0 : default : }
switch ( 0 ) { case new 0 , 0 : }
switch ( 0 ) { case null [ 0 ] , 0 : default : }
switch ( 0 ) { case x -- , 0 : default : case 0 : }
switch ( 0 ) { default : case x , 0 : }
switch ( 0 ) { default : case { x } , 0 : }

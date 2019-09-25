(* The following code demonstrates that "list" is actually a datatype. *)
datatype 'a mylist = empty | cons of 'a * 'a mylist
fun sum xs = 
    case xs of
        empty => 0 |
        cons(x,xs') => x + sum xs'
fun my_null xs = 
    case xs of
        empty => true |
        _ => false
fun my_hd xs = 
    case xs of
        empty => raise Empty |
        cons(x, _) => x
fun my_tl xs = 
    case xs of
        empty => raise Empty |
        cons(_, xs') => xs'

(* The following code demonstrates that "option" is actually a datatype. *)
datatype 'a myoption = mynone | mysome of 'a
fun my_issome myoption = 
    case myoption of
        mynone => false |
        _ => true
fun my_valof myoption = 
    case myoption of
        mynone => raise Option |
        mysome x => x

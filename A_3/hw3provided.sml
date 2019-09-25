(* Coursera Programming Languages, Homework 3, Provided Code *)
(* 1 *)
val only_capitals = List.filter (fn str => Char.isUpper(String.sub(str, 0)))
(* 2 *)
val longest_string1 = List.foldl (fn (str, acc) => if String.size str > String.size acc then str else acc) ""
(* 3 *)
val longest_string2 = List.foldl (fn (str, acc) => if String.size str >= String.size acc then str else acc) ""
(* 4 *)
fun longest_string_helper compare = List.foldl (fn (str, acc) => if compare(String.size str, String.size acc) then str else acc) ""
val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)
(* 5 *)
val longest_capitalized = longest_string1 o only_capitals
(* 6 *)
val rev_string = String.implode o List.rev o String.explode

exception NoAnswer
(* 7 *)
fun first_answer f xs = 
    case xs of
        [] => raise NoAnswer |
        x::xs' => case f x of
                      NONE => first_answer f xs' |
                      SOME v => v
(* 8 *)
fun all_answers f xs = 
    let 
        fun answers (xs, acc) = 
            case xs of
                [] => acc |
                x::xs' => case f x of
                              NONE => answers([], NONE) |
                              SOME lst => answers(xs', SOME(lst @ (case acc of
                                                                      SOME lst => lst |
                                                                      NONE => [])))
    in
        answers (xs, SOME [])
    end

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end
(* 9a *)
val count_wildcards = g (fn () => 1) (fn x => 0)
(* 9b *)
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)
(* 9c *)
fun count_some_var (varname, p) = g (fn () => 0) (fn x => if x = varname then 1 else 0) p
(* 10 *)
val check_pat = 
    let
        fun get_varname_list p = 
            case p of
                Variable varname => varname::[] |
                TupleP ps => List.foldl (fn (p, acc) => get_varname_list p @ acc) [] ps |
                ConstructorP (_, p) => get_varname_list p |
                _ => []
        fun has_repeats xs = 
            case xs of
                [] => false |
                x::xs' => List.exists (fn y => y = x) xs' orelse has_repeats xs'
    in
        not o has_repeats o get_varname_list
    end
(* 11 *) 
fun match value_pattern = 
    case value_pattern of
        (_, Wildcard) => SOME [] |
        (value, Variable varname) => SOME((varname, value)::[]) |
        (Unit, UnitP) => SOME [] |
        (Const value, ConstP valuep) => if value = valuep then SOME [] else NONE |
        (Tuple values, TupleP patterns) => if List.length values = List.length patterns
                                           then all_answers match (ListPair.zip(values, patterns))
                                           else NONE |
        (Constructor(s1, value), ConstructorP(s2, pattern)) => if s1 = s2
                                                               then match (value, pattern)
                                                               else NONE |
        _ => NONE
(* 12 *)
fun first_match value patterns = 
    SOME (first_answer (fn p => match(value, p)) patterns) handle NoAnswer => NONE

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

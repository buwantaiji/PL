type student_id = int
type grade = int
type final_grade = {id: student_id, grade: grade option}
datatype pass_fail = pass | fail
(* 1 *)
fun pass_or_fail {id=id, grade=grade} = 
    case grade of
        SOME points => if points >= 75 then pass else fail |
        NONE => fail
(* 2 *)
fun has_passed final_grade = 
    pass_or_fail final_grade = pass 
(* 3 *)
fun number_passed final_grades = 
    case final_grades of
        [] => 0 |
        final_grade::final_grades' => (if has_passed final_grade then 1 else 0) + number_passed final_grades'
(* 4 *)
fun number_misgraded pairs = 
    case pairs of
        [] => 0 |
        (pass, final_grade)::pairs' => (if has_passed final_grade then 0 else 1) + number_misgraded pairs' |
        (fail, final_grade)::pairs' => (if has_passed final_grade then 1 else 0) + number_misgraded pairs'

datatype 'a tree = leaf | node of {value: 'a, left: 'a tree, right: 'a tree}
datatype flag = leave_me_alone | prune_me
(* 5 *)
fun tree_height tr = 
    case tr of
        leaf => 0 |
        node {value = _, left = ltr, right = rtr} => 1 + Int.max(tree_height ltr, tree_height rtr)
(* 6 *)
fun sum_tree tr = 
    case tr of
        leaf => 0 |
        node {value = v, left = ltr, right = rtr} => v + sum_tree ltr + sum_tree rtr
(* 7 *)
fun gardener tr = 
    case tr of
        leaf => leaf |
        node {value = prune_me, left = _, right = _} => leaf |
        node {value = leave_me_alone, left = ltr, right = rtr} => node {value = leave_me_alone, left = gardener ltr, right = gardener rtr}

datatype nat = ZERO | SUCC of nat
(* 9 *)
fun is_positive n = 
    case n of
        ZERO => false |
        _ => true
(* 10 *)
exception Negative
fun pred n = 
    case n of
        ZERO => raise Negative |
        SUCC num => num
(* 11 *)
fun nat_to_int n = 
    case n of
        ZERO => 0 |
        SUCC num => 1 + nat_to_int num
(* 12 *)
fun int_to_nat n = 
    if n < 0
    then raise Negative
    else if n = 0
    then ZERO
    else SUCC(int_to_nat(n - 1))
(* 13 *)
fun add (n1, n2) = 
    case n1 of
        ZERO => n2 |
        SUCC num => SUCC(add(num, n2))
(* 14 *)
fun sub (n1, n2) = 
    case n2 of
        ZERO => n1 |
        SUCC num => sub(pred n1, num)
(* 15 *)
fun mult (n1, n2) = 
    case n1 of
        ZERO => ZERO |
        SUCC num => add(mult(num, n2), n2)
(* 16 *)
fun less_than (n1, n2) = 
    case (n1, n2) of
        (ZERO, ZERO) => false |
        (SUCC _, ZERO) => false |
        (ZERO, SUCC _) => true |
        (SUCC num1, SUCC num2) => less_than(num1, num2)
(*
datatype intSet = Elems of int list |
                  Range of {from: int, to: int} |
                  Union of intSet * intSet |
                  Intersection of intSet * intSet
*)
(* 17 *)
(*
fun noCommonElem (set1, set2) = 
    case (set1, set2) of
        (Union (set11, set12), set2) => noCommonElem(set11, set2) andalso noCommonElem(set12, set2) |
        (set1, Union (set21, set22)) => noCommonElem(set1, set21) andalso noCommonElem(set1, set22) |
        (Intersection (set11, set12), set2) => 
fun isEmpty set = 
    case set of
        Elems xs => null xs |
        Range {from = from, to = to} => from > to |
        Union (set1, set2) => isEmpty set1 andalso isEmpty set2 |
        Intersection (set1, set2) => isEmpty set1 orelse isEmpty set2 orelse noCommonElem(set1, set2)
*)

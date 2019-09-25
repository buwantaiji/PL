(* 1 *)
fun compose_opt (f, g) = 
    fn x => case g x of
                NONE => NONE |
                SOME y => f y
(* 2 *)
fun do_until f p x =  (* fun do_until f = fn p => fn x => ... *)
    if not (p x)
    then x
    else do_until f p (f x)
(* 4 *)
fun fixed_point f = do_until f (fn x => f x <> x)
(* 5 *)
fun map2 f (x1, x2) = (f x1, f x2)
(* 6 *)
fun app_all f g x = 
    let
        fun app_f_contatenate xs = 
            case xs of
                [] => [] |
                x::xs' => f x @ app_f_contatenate xs'
    in
        app_f_contatenate(g x)
    end
(* 7 *)
fun foldl f acc xs = 
    case xs of
        [] => acc |
        x::xs' => foldl f (f(acc, x)) xs'
fun foldr f acc xs = 
    case xs of
        [] => acc |
        x::xs' => f(foldr f acc xs', x)
(* 8 *)
fun partition f xs = 
    case xs of
        [] => ([], []) |
        x::xs' => let val tail_result = partition f xs'
                  in if f x
                     then (x::(#1 tail_result), #2 tail_result)
                     else (#1 tail_result, x::(#2 tail_result))
                  end
(* 9 *)
fun unfold f seed = 
    case f seed of
        NONE => [] |
        SOME (x, new_seed) => x::(unfold f new_seed)
(* 10 *)
fun factorial n = 
    foldl (fn (x, y) => x * y) 1 (unfold (fn n => if n = 0 then NONE else SOME (n, n - 1)) n)
(* 11 *)
fun map f = foldr (fn (acc, x) => f x::acc) []
(* 12 *)
fun filter f = foldr (fn (acc, x) => if f x then x::acc else acc) []
(* 14 *)
datatype 'a tree = leaf | node of {lchild: 'a tree, rchild: 'a tree, data: 'a}
fun map_tree f tr = 
    case tr of
        leaf => leaf |
        node {lchild = ltr, rchild = rtr, data = x} => node {lchild = map_tree f ltr, rchild = map_tree f rtr, data = f x}
fun fold_tree f acc tr = 
    case tr of
        leaf => acc |
        node {lchild = ltr, rchild = rtr, data = x} => fold_tree f (fold_tree f (f(acc, x)) ltr) rtr
fun filter_tree f tr = 
    case tr of
        leaf => leaf |
        node {lchild = ltr, rchild = rtr, data = x} => if f x then node {lchild = filter_tree f ltr, rchild = filter_tree f rtr, data = x} else leaf

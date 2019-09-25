(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* a *)
fun all_except_option (str, strlist) = 
    let
        fun all_except strlist = 
            case strlist of
                [] => [] |
                hdstr::strlist' => if same_string(str, hdstr) then strlist' else hdstr::all_except strlist'
        val result = all_except strlist
    in
        if result = strlist then NONE else SOME result
    end
(* b *)
fun get_substitutions1 (substitutions, str) = 
    case substitutions of
        [] => [] |
        substitution::substitutions' => case all_except_option(str, substitution) of
                                            NONE => get_substitutions1(substitutions', str) |
                                            SOME lst => lst @ get_substitutions1(substitutions', str)
(* c *)
fun get_substitutions2 (substitutions, str) = 
    let
        fun aux (substitutions, acc) = 
            case substitutions of
                [] => acc |
                substitution::substitutions' => case all_except_option(str, substitution) of
                                                    NONE => aux(substitutions', acc) |
                                                    SOME lst => aux(substitutions', acc @ lst)
    in
        aux(substitutions, [])
    end
(* d *)
fun similar_names (substitutions, {first = fname, middle = mname, last = lname}) = 
    let
        fun substitute_fullnames (fnames) = 
            case fnames of
                [] => [] |
                substitute_fname::fnames' => {first = substitute_fname, middle = mname, last = lname}
                                             ::substitute_fullnames(fnames')
    in
        {first = fname, middle = mname, last = lname}::substitute_fullnames(get_substitutions2(substitutions, fname))
    end
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* a *)
fun card_color (st, rk) = 
    case st of
        Clubs => Black |
        Spades => Black |
        Diamonds => Red |
        Hearts => Red
(* b *)
fun card_value (st, rk) = 
    case rk of
        Num points => points |
        Ace => 11 |
        _ => 10
(* c *)
fun remove_card (cds, cd, e) = 
    case cds of
        [] => raise e |
        hd_cd::cds' => if cd = hd_cd then cds' else hd_cd::remove_card(cds', cd, e)
(* d *)
fun all_same_color cds = 
    case cds of
        [] => true |
        cd::[] => true |
        hd_cd::nk_cd::cds' => card_color hd_cd = card_color nk_cd andalso all_same_color(nk_cd::cds')
(* e *)
fun sum_cards cds = 
    let
        fun aux (cds, acc) = 
            case cds of
                [] => acc |
                cd::cds' => aux(cds', acc + card_value cd)
    in
        aux(cds, 0)
    end
(* f *)
fun score (held_cds, goal) = 
    let
        val sum = sum_cards held_cds
        val preliminary_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in
        if all_same_color held_cds then preliminary_score div 2 else preliminary_score
    end
(* g *)
fun officiate (cd_list, move_list, goal) = 
    let
        fun aux (cd_list, held_cds, move_list, sum) = 
            case move_list of
                [] => held_cds |
                Discard cd::move_list' => aux(cd_list, remove_card(held_cds, cd, IllegalMove), move_list', sum - card_value cd) |
                Draw::move_list' => case cd_list of
                                        [] => held_cds |
                                        cd::cd_list' => if sum + card_value cd > goal then cd::held_cds else aux(cd_list', cd::held_cds, move_list', sum + card_value cd)
        (* A more elegant approach, using nested patterns. *)
        fun aux2 (cd_list, held_cds, move_list, sum) = 
            case (move_list, cd_list) of
                ([], _) => held_cds |
                (Discard cd::move_list', _) => aux2(cd_list, remove_card(held_cds, cd, IllegalMove), move_list', sum - card_value cd) |
                (Draw::move_list', []) => held_cds |
                (Draw::move_list', cd::cd_list') => if sum + card_value cd > goal then cd::held_cds else aux2(cd_list', cd::held_cds, move_list', sum + card_value cd)
    in
        score(aux2(cd_list, [], move_list, 0), goal)
    end

(* 3a *)
fun ace_num cds = 
    case cds of
        [] => 0 |
        (_, Ace)::cds' => 1 + ace_num cds' |
        _::cds' => ace_num cds'
fun score_challenge (held_cds, goal) = 
    let
        val sum = sum_cards held_cds
        fun score_from_sum sum = 
            let
                val preliminary_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
            in
                if all_same_color held_cds then preliminary_score div 2 else preliminary_score
            end
        fun scores (sum, aces) = 
            if aces = 0
            then score_from_sum sum::[]
            else score_from_sum sum::scores(sum - 10, aces - 1)
        val possible_scores = scores(sum, ace_num(held_cds))
        fun min xs = 
            case xs of
                [] => raise Empty |
                x::[] => x |
                x::xs' => let val tail_min = min xs' in if x < tail_min then x else tail_min end
        val final_score = min possible_scores
    in
        final_score
    end
fun officiate_challenge (cd_list, move_list, goal) = 
    let
        fun aux (cd_list, held_cds, move_list, sum) = 
            case move_list of
                [] => held_cds |
                Discard cd::move_list' => aux(cd_list, remove_card(held_cds, cd, IllegalMove), move_list', sum - card_value cd) |
                Draw::move_list' => case cd_list of
                                        [] => held_cds |
                                        cd::cd_list' => if sum_cards(cd::held_cds) - 10 * ace_num(cd::held_cds) > goal
                                                        then cd::held_cds
                                                        else aux(cd_list', cd::held_cds, move_list', sum + card_value cd)
        (* A more elegant approach, using nested patterns. *)
        fun aux2 (cd_list, held_cds, move_list, sum) = 
            case (move_list, cd_list) of
                ([], _) => held_cds |
                (Discard cd::move_list', _) => aux2(cd_list, remove_card(held_cds, cd, IllegalMove), move_list', sum - card_value cd) |
                (Draw::move_list', []) => held_cds |
                (Draw::move_list', cd::cd_list') => if sum_cards(cd::held_cds) - 10 * ace_num(cd::held_cds) > goal
                                                    then cd::held_cds
                                                    else aux2(cd_list', cd::held_cds, move_list', sum + card_value cd)
    in
        score_challenge(aux2(cd_list, [], move_list, 0), goal)
    end

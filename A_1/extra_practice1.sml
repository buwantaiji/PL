fun append (xs : int list, ys : int list) = 
    if null xs
    then ys
    else hd xs::append(tl xs, ys)
(* 1 *)
fun alternate (xs : int list) = 
    if null(tl xs)
    then hd xs
    else hd xs - alternate(tl xs)
(* 2 *)
fun min_max (xs : int list) = 
    if null(tl xs)
    then (hd xs, hd xs)
    else
        let val tail_minmax = min_max(tl xs)
        in
            (if hd xs < #1 tail_minmax then hd xs else #1 tail_minmax, 
             if hd xs > #2 tail_minmax then hd xs else #2 tail_minmax)
        end
(* 3 *)
fun cumsum (xs : int list) = 
    let
        fun add (xs : int list, addnum : int) = 
            if null(tl xs)
            then [hd xs + addnum]
            else (hd xs + addnum)::add(tl xs, addnum)
    in
        if null(tl xs)
        then [hd xs]
        else hd xs::add(cumsum(tl xs), hd xs)
    end
(* 5 *)
fun repeat (xs : int list, ys : int list) = 
    let
        fun duplicate (element : int, n : int) = 
            if n = 0
            then []
            else element::duplicate(element, n - 1)
        fun append (xs : int list, ys : int list) = 
            if null xs
            then ys
            else hd xs::append(tl xs, ys)
    in
        if null(tl xs)
        then duplicate(hd xs, hd ys)
        else append(duplicate(hd xs, hd ys), repeat(tl xs, tl ys))
    end
(* 8 *)
fun any (xs : bool list) = 
    if null xs
    then false
    else hd xs orelse any(tl xs)
(* 9 *)
fun all (xs : bool list) = 
    if null xs
    then true
    else hd xs andalso all(tl xs)
(* 10 *)
fun zip (xs : int list, ys : int list) = 
    if null xs orelse null ys
    then []
    else (hd xs, hd ys)::zip(tl xs, tl ys)
(* 14 *)
fun splitup (xs : int list) = 
    if null xs
    then ([], [])
    else
        let 
            val tail_split = splitup(tl xs)
        in
            if hd xs >= 0
            then (hd xs::(#1 tail_split), #2 tail_split)
            else (#1 tail_split, hd xs::(#2 tail_split))
        end
(* 15 *)
fun splitAt (xs : int list, threshold : int) = 
    if null xs
    then ([], [])
    else
        let 
            val tail_split = splitAt(tl xs, threshold)
        in
            if hd xs >= threshold
            then (hd xs::(#1 tail_split), #2 tail_split)
            else (#1 tail_split, hd xs::(#2 tail_split))
        end
(* 16 *)
fun isSorted (xs : int list) = 
    if null(tl xs)
    then true
    else (hd xs <= hd(tl xs)) andalso isSorted(tl xs)
(* 17 *)
fun isAnySorted (xs : int list) = 
    let
        fun isUpSorted (xs : int list) = 
            if null(tl xs)
            then true
            else (hd xs <= hd(tl xs)) andalso isUpSorted(tl xs)
        fun isDownSorted (xs : int list) = 
            if null(tl xs)
            then true
            else (hd xs >= hd(tl xs)) andalso isDownSorted(tl xs)
    in
        isUpSorted(xs) orelse isDownSorted(xs)
    end
(* 18 *)
fun sortedMerge (xs : int list, ys : int list) = 
    if null xs then ys
    else if null ys then xs
    else if hd xs <= hd ys
    then hd xs::sortedMerge(tl xs, ys)
    else hd ys::sortedMerge(xs, tl ys)
(* 19 *)
fun qsort (xs: int list) = 
    if null xs
    then []
    else
        let
            val (rightlist, leftlist) = splitAt(xs, hd xs)
            val rightsortedlist = qsort(tl rightlist)
            val leftsortedlist = qsort leftlist
        in 
            append(leftsortedlist, hd xs::rightsortedlist)
        end
(* 20 *)
fun divide (xs : int list) = 
    if null(tl xs)
    then (xs, [])
    else
        let
            val tail_divide = divide(tl xs)
        in
            (hd xs::(#2 tail_divide), #1 tail_divide)
        end
(* 21 *)
fun not_so_quick_sort (xs : int list) = 
    if null xs then []
    else if null(tl xs) then xs
    else
        let
            val (div1, div2) = divide xs
            val div1sorted = not_so_quick_sort div1
            val div2sorted = not_so_quick_sort div2
        in
            sortedMerge(div1sorted, div2sorted)
        end
(* 22 *)
fun fullDivide (k : int, n : int) = 
    if n mod k <> 0
    then (0, n)
    else
        let
            val reduced_result = fullDivide(k, n div k)
        in
            (#1 reduced_result + 1, #2 reduced_result)
        end
(* 23 *)
fun factorize (n : int) = 
    let
        fun search (start : int, n : int) = 
            if n = 1 then []
            else if start * start > n then [(n, 1)]
            else if n mod start <> 0
            then search(start + 1, n)
            else
                let
                    val (power, residue) = fullDivide(start, n)
                in
                    (start, power)::search(start + 1, residue)
                end
    in
        search(2, n)
    end
(* 24 *)
fun multiply (factors : (int * int) list) = 
    let
        fun pow (base : int, index : int) = 
            if index = 0
            then 1
            else base * pow(base, index - 1)
    in
        if null factors
        then 1
        else pow(hd factors) * multiply(tl factors)
    end

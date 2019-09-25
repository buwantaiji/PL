fun sumall (xs : int list) = 
    if null(tl xs)
    then hd xs
    else hd xs + sumall(tl xs)
fun countdownto1 (n : int) = 
    if n = 1
    then [1]
    else n::countdownto1(n - 1)
fun append (xs : int list, ys : int list) = 
    if null xs
    then ys
    else hd xs::append(tl xs, ys)

fun countupfrom1 (n : int) = 
    let
        fun countupfrom (from : int) = 
            if from = n
            then [n]
            else from::countupfrom(from + 1)
    in
        countupfrom 1
    end

fun badmax (xs : int list) = (* Assume that int list xs is nonempty. *)
    if null(tl xs)
    then hd xs
    else if hd xs > badmax(tl xs)
    then hd xs
    else badmax(tl xs)

fun goodmax (xs : int list) = 
    if null(tl xs)
    then hd xs
    else
        let
            val tailmax = goodmax(tl xs)
        in 
            if hd xs > tailmax
            then hd xs
            else tailmax
        end

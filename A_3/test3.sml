fun double x = 2 * x
fun increment x = x + 1
fun n_times (f, n, x) = 
    if n = 0
    then x
    else f(n_times(f, n - 1, x))

fun triple_ntimes1 (n, x) = 
    let
        fun triple x = 3 * x
    in
        n_times(triple, n, x)
    end
fun triple_ntimes2 (n, x) = 
    n_times(let fun triple x = 3 * x in triple end, n, x)
fun triple_ntimes3 (n, x) = 
    n_times(fn x => 3 * x, n, x)

fun testreturn f = 
    if f 7
    then fn x => 2 * x
    else fn x => 3 * x

fun map (f, xs) = 
    case xs of
        [] => [] |
        x::xs' => f x::map(f, xs')
fun filter (f, xs) = 
    case xs of
        [] => [] |
        x::xs' => if f x then x::filter(f, xs') else filter(f, xs')
fun fold (f, acc, xs) = 
    case xs of
        [] => acc |
        x::xs' => fold(f, f(acc, x), xs')
fun sum xs = fold(fn (x, y) => x + y, 0, xs)
fun predicate (f, xs) = fold(fn (x, y) => x andalso f y, true, xs)

fun allgreaterthann (n, xs) = 
    filter(fn x => x > n, xs)
fun isallgreaterthann (n, xs) = 
    predicate(fn x => x > n, xs)


fun addn n = fn x => x + n
fun add x = fn y => fn z => x + y + z
fun add_complex x = 
    let
        fun add1 y = 
            let
                fun add2 z = x + y + z
            in
                add2
            end
    in
        add1
    end

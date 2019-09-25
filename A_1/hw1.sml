(* 1 *)
fun is_older (date1 : int * int * int, date2 : int * int * int) = 
    if #1 date1 < #1 date2 then true
    else if #1 date1 > #1 date2 then false
    else if #2 date1 < #2 date2 then true
    else if #2 date1 > #2 date2 then false
    else if #3 date1 < #3 date2 then true
    else false
(* 2 *)
fun number_in_month (dates : (int * int * int) list, month : int) = 
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)
(* 3 *)
fun number_in_months (dates : (int * int * int) list, months : int list) = 
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)
(* 4 *)
fun dates_in_month (dates : (int * int * int) list, month : int) = 
    if null dates
    then []
    else if #2 (hd dates) = month
    then hd dates::dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)
(* 5 *)
fun dates_in_months (dates : (int * int * int) list, months : int list) = 
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
(* 6 *)
fun get_nth (strs : string list, n : int) = 
    if n = 1
    then hd strs
    else get_nth(tl strs, n - 1)
(* 7 *)
fun date_to_string (date : int * int * int) = 
    let
        val month_strings = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(month_strings, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end
(* 8 *)
fun number_before_reaching_sum (sum : int, xs : int list) = 
    if hd xs >= sum
    then 0
    else number_before_reaching_sum(sum - hd xs, tl xs) + 1
(* 9 *)
fun what_month (day_of_year : int) = 
    let
        val days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day_of_year, days_per_month) + 1
    end
(* 10 *)
fun month_range (day_of_year1 : int, day_of_year2 : int) = 
    if day_of_year1 > day_of_year2
    then []
    else if day_of_year1 = day_of_year2
    then [what_month day_of_year1]
    else what_month day_of_year1::month_range(day_of_year1 + 1, day_of_year2)
(* 11 *)
fun oldest (dates : (int * int * int) list) = 
    if null dates
    then NONE
    else
        let
            fun oldest_date (dates: (int * int * int) list) = 
                if null(tl dates)
                then hd dates
                else
                    let
                        val tail_result = oldest_date(tl dates)
                    in
                        if is_older(hd dates, tail_result)
                        then hd dates
                        else tail_result
                    end
        in
            SOME(oldest_date dates)
        end
(* 12 *)
fun delete_int (k : int, xs : int list) = 
    if null xs
    then []
    else if hd xs = k
    then delete_int(k, tl xs)
    else hd xs::delete_int(k, tl xs)
fun remove_duplicates (xs : int list) = 
    if null xs
    then []
    else hd xs::remove_duplicates(delete_int(hd xs, xs))
fun number_in_months_challenge (dates : (int * int * int) list, months : int list) = 
    number_in_months(dates, remove_duplicates months)
fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) = 
    dates_in_months(dates, remove_duplicates months)
(* 13 *)
fun reasonable_date (date : int * int * int) = 
    (#1 date > 0) andalso (#2 date >=1 andalso #2 date <= 12) andalso
    let
        fun isLeapYear (year : int) = 
            year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
        val days_per_month = [31, if isLeapYear(#1 date) then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        fun get_nth_int (xs : int list, n : int) = 
            if n = 1
            then hd xs
            else get_nth_int(tl xs, n - 1)
        val dayrange = get_nth_int(days_per_month, #2 date)
    in
        (#3 date >=1 andalso #3 date <= dayrange)
    end

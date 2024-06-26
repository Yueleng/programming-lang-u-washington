(* Q1 *)
fun is_older(date1: int * int * int, date2: int*int*int) = 
    if #1 date1 < #1 date2
    then true
    else if #1 date1 > #1 date2
    then false
    else if #2 date1 < #2 date2
    then true
    else if #2 date1 > #2 date2
    then false
    else if #3 date1 < #3 date2
    then true 
    else if #3 date1 > #3 date2
    then false
    else false

(* Q2 *)
fun number_in_month(dates: (int * int * int) list, month: int) = 
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

(* Q3 *)
fun number_in_months(dates: (int * int * int) list, months: int list) = 
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* Q4 *)
fun dates_in_month(dates: (int * int * int) list, month: int) = 
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

(* Q5 *)
fun dates_in_months(dates: (int * int * int) list, months: int list) = 
    let 
        fun append(xs : (int * int * int) list, ys : (int * int * int) list) =
        if null xs
        then ys
        else (hd xs) :: append((tl xs), ys);
    in 
        if null months
        then []
        else 
            append(
                dates_in_month(dates, hd months), 
                dates_in_months(dates, tl months)
            )
    end
    
    
(* Q6 *)
fun get_nth(elements: string list, n: int) = 
    if n = 1 
    then hd elements
    else get_nth(tl elements, n - 1)

(* Q7 *)
fun date_to_string(date: (int * int * int)) = 
    let 
        val months = ["January", "February", "March", "April",
                  "May", "June", "July", "August", "September", 
                  "October", "November", "December"]
        fun date_to_string_month(date: (int * int * int), months: string list) = 
            if #2 (date) = 1
            then hd months ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
            else date_to_string_month((#1 date, (#2 date) - 1, #3 date), tl months)
    in  
        date_to_string_month(date, months)
    end 

(* Q8 *)
fun number_before_reaching_sum(sum: int, ints: int list) = 
    if hd ints >= sum 
    then 0
    else 1 + number_before_reaching_sum(sum - hd ints, tl ints)

(* Q9 *)
fun what_month(day_of_year: int) = 
    let
        val ints = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        (* val months = ["January", "February", "March", "April",
                  "May", "June", "July", "August", "September", 
                  "October", "November", "December"]*)
        val n = number_before_reaching_sum(day_of_year, ints)
    in
        n + 1
    end

(* Q10 *)
fun month_range(day1: int, day2: int) = 
    if day1 > day2 
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

(* Q11 *)
fun oldest(dates: (int*int*int) list) = 
    if null dates 
    then NONE
    else let 
       val tl_oldest = oldest(tl dates)
    in 
        if isSome tl_oldest andalso is_older(valOf tl_oldest, hd dates)
        then tl_oldest
        else SOME(hd dates)
    end


(* Q12 *)

fun number_in_months_challenge(dates: (int * int * int) list, months: int list) = 
    let 
        fun is_in(month: int, months: int list) = 
            if null months 
            then false 
            else if month = hd months
            then true
            else is_in(month, tl months)

        fun get_distint_months(months: int list, accum: int list) = 
            if null months 
            then accum
            else if is_in(hd months, accum)
            then get_distint_months(tl months, accum)
            else get_distint_months(tl months, (hd months) :: accum)
    in
        number_in_months(dates,get_distint_months(months, []))
    end

fun dates_in_months_challenge(dates: (int * int * int) list, months: int list) = 
    let 
        fun is_in(month: int, months: int list) = 
            if null months 
            then false 
            else if month = hd months
            then true
            else is_in(month, tl months)

        fun get_distint_months(months: int list, accum: int list) = 
            if null months 
            then accum
            else if is_in(hd months, accum)
            then get_distint_months(tl months, accum)
            else get_distint_months(tl months, (hd months) :: accum)
    in
        dates_in_months(dates,get_distint_months(months, []))
    end

(* Q13 *)
fun reasonable_date(date: int * int * int) = 
    if (#1 date) <= 0 
    then false
    else let 
        val is_leap_year = (#1 date) mod 400 = 0
			   orelse 
                          ((#1 date) mod 100 <> 0 andalso (#1 date) mod 4 = 0)
        val big_months = [1, 3, 5, 7, 8, 10, 12]
        val small_month = [4, 6, 9, 11]
        fun is_in(month: int, months: int list) = 
            if null months 
            then false 
            else if month = hd months
            then true
            else is_in(month, tl months)
        val month = #2 date
        val day_in_month = #3 date
    in 
        if (month < 1 orelse month > 12) 
        then false
        else if is_in(month, big_months) (* big_months *)
        then day_in_month >= 1 andalso day_in_month <= 31
        else if is_in(month, small_month) (* small_months *)
        then day_in_month >= 1 andalso day_in_month <= 30
        else if is_leap_year 
        then day_in_month >= 1 andalso day_in_month <= 29
        else day_in_month >= 1 andalso day_in_month <= 28
    end

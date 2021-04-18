fun is_older (date_1 : int*int*int, date_2 : int*int*int) =
    let val year_older = #1 date_1 < #1 date_2
    val year_same = #1 date_1 = #1 date_2
    val month_older = #2 date_1 < #2 date_2
    val month_same = #2 date_1 = #2 date_2
    val day_older = #3 date_1 < #3 date_2
    in 
    if year_older orelse (year_same andalso (month_older orelse (month_same andalso day_older)))
    then true
    else false
    end

fun number_in_month (date_list : (int*int*int) list, month : int) =
    if null date_list
    then 0
    else (if #2 (hd(date_list)) = month then 1 else 0) + number_in_month(tl(date_list), month)

fun number_in_months (date_list : (int*int*int) list, month_list : int list) =
    if null month_list
    then 0
    else number_in_month(date_list, hd(month_list)) + number_in_months(date_list, tl(month_list))

fun dates_in_month (date_list : (int*int*int) list, month : int) =
    if null date_list
    then []
    else (if #2 (hd(date_list)) = month then hd(date_list)::dates_in_month(tl(date_list), month) else dates_in_month(tl(date_list), month))

fun dates_in_months (date_list : (int*int*int) list, month_list : int list) =
    if null month_list
    then []
    else dates_in_month(date_list, hd(month_list)) @ dates_in_months(date_list, tl(month_list))

fun get_nth (str_list : string list, n : int) = 
    if n = 1
    then hd(str_list)
    else get_nth(tl(str_list), n - 1)

fun date_to_string (date : int*int*int) =
    let val month_lookup = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
    get_nth(month_lookup, (#2 date)) ^ " " ^ Int.toString((#3 date)) ^ ", " ^ Int.toString((#1 date))
    end

fun number_before_reaching_sum (sum : int, num_list : int list) = 
    let fun helper (curr_total : int, curr_list : int list, n : int) =
        if curr_total >= sum
        then n - 1
        else helper(curr_total + hd(curr_list), tl(curr_list), n + 1)
    in
    helper(0, num_list, 0)
    end

fun what_month (day : int) =
    let val month_lookup = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
    number_before_reaching_sum(day, month_lookup) + 1
    end

fun month_range (day1 : int, day2 : int) =
    let fun month_sequence (curr_day : int) =
        if curr_day > day2
        then []
        else what_month(curr_day)::month_sequence(curr_day + 1)
    in
    if day1 > day2
    then []
    else month_sequence day1
    end

fun oldest (date_list : (int*int*int) list) =
    let fun get_oldest (dates : (int*int*int) list, curr_oldest : (int*int*int)) =
        if null dates
        then curr_oldest
        else (if is_older(hd(dates), curr_oldest) then get_oldest(tl(dates), hd(dates)) else get_oldest(tl(dates), curr_oldest))
    in
    if null date_list
    then NONE
    else SOME (get_oldest(date_list, hd(date_list)))
    end

fun reasonable_date (date : int*int*int) =
    let val year_ok = #1 date > 0
        val month_ok = #2 date >= 1 andalso #2 date <= 12
        val is_leapyear = #1 date mod 400 = 0 orelse (#1 date mod 4 = 0 andalso not(#1 date mod 100 = 0))
        val days_feb = if is_leapyear then 29 else 28
        val days_lookup = [31, days_feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        fun get_nth_int (int_list : int list, n : int) = 
            if n = 1
            then hd(int_list)
            else get_nth_int(tl(int_list), n - 1)
    in
    year_ok andalso month_ok andalso #3 date >= 1 andalso #3 date <= get_nth_int(days_lookup, #2 date)
    end

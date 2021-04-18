(* HW1 *)
(* In all problems, a “date”
is an SML value of type int*int*int, where the first part is the year, the second part is the month, and the third part is the day *)

(* 
Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same, the result is false.) 
*)
fun is_older (date1: int*int*int, date2: int*int*int) = 
	#1 date1 < #1 date2 orelse 
	#1 date1 <= #1 date2 andalso #2 date1 < #2 date2 orelse
	#1 date1 <= #1 date2 andalso #2 date1 <= #2 date2 andalso #3 date1 < #3 date2
	

(* 
Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns how many dates in the list are in the given month.
*)
fun number_in_month (dates: (int*int*int) list, month: int)=
	if null dates
	then 0
	else 
		let fun date_in_month(date: int*int*int)=
			if #2 date = month then 1 else 0
		in
			date_in_month(hd dates) + number_in_month(tl dates, month)
		end

(*
Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated.
*)
fun number_in_months(dates: (int*int*int) list, months: int list)=
	if null months
	then 0
	else
		number_in_month(dates, hd months) + number_in_months(dates, tl months)

(*
Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a list holding the dates from the argument list of dates that are in the month. The returned list should contain dates in the order they were originally given.
*)
fun dates_in_month(dates: (int*int*int) list, month: int)=
	if null dates
	then []
	else 
		let val tl_ans = dates_in_month(tl dates, month)
		in
			if (#2(hd dates)) = month 
			then hd(dates)::tl_ans
			else tl_ans
		end

(*
Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list) and returns a list holding the dates from the argument list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the previous problem and SML’s list-append operator (@).
*)
fun dates_in_months(dates: (int*int*int) list, months: int list)=
	if null months
	then []
	else
		dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
		

(*
Write a function get_nth that takes a list of strings and an int n and returns the n
th element of the list where the head of the list is 1st
*)
fun get_nth(strings: string list, n: int)=
	if n=1
	then hd strings
	else
		get_nth(tl strings, n-1)

(*
 Write a function date_to_string that takes a date and returns a string of the form January 20, 2013 (for example). Use the operator ^ for concatenating strings and the library function Int.toString for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
*)
fun date_to_string(date: int*int*int)=
	let 
		val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
	in
		get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end

(*
Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more.
*)
fun number_before_reaching_sum(sum: int, numbers: int list)=
	if hd numbers >= sum
	then 0
	else
		1 + number_before_reaching_sum(sum - hd numbers, tl numbers)


(*
Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.)
*)
fun what_month(day: int)=
	let 
		val days_months = [31,28,31,30,31,30,31,31,30,31,30,31];
	in
		number_before_reaching_sum(day, days_months) + 1
	end
	

(*
Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.
*)	
fun month_range(day1: int, day2: int)=
	if day1 > day2
	then []
	else
		what_month(day1)::month_range(day1+1, day2)

(*
Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.
*)		
fun oldest(dates: (int*int*int) list)=
	if null dates
	then NONE
	else
		let 
			fun find_oldest(date1: int*int*int, dates: (int*int*int) list)=
				if null dates then date1
				else if is_older(date1, hd dates) 
				then find_oldest(date1, tl dates)
				else find_oldest(hd dates, tl dates)
		in
			SOME (find_oldest(hd dates, tl dates))
		end
	
		
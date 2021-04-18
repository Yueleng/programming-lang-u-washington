type date = int * int * int
val months = ["January", "February", "March", "April", "May", "June",
	      "July", "August", "September", "October", "November", "December"]
val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]  		 

(*
   Takes two dates and evaluates to true or false. 
   Evaluates to true if the first argument is a date that comes before the second argument.
   If the two dates are the same, the result is false. 
*)	       
fun is_older((y1, m1, d1), (y2, m2, d2)) =
    if y1 <> y2 then y1 < y2
    else if m1 <> m2 then m1 < m2
    else if d1 <> d2 then d1 < d2
    else false

(*
   Takes a list of dates and a month (int) and returns how many dates in the list are 
   in the given month.
*)    	     
fun number_in_month([], m) = 0
  | number_in_month(d :: ds: date list, m) =
    let val acc = number_in_month(ds, m)
    in
	if #2d = m then 1 + acc
	else acc
    end

(*
   Takes a list of dates and a list of months (an int list) and returns the number 
   of dates in the list of dates that are in any of the months in the list of months.
*)	
fun number_in_months(ds, []) = 0
  | number_in_months(ds, m :: ms) =
    number_in_month(ds, m) + number_in_months(ds, ms)

(*
   Takes a list of dates and a month (an int) and returns a list holding the dates 
   from the argument list of dates that are in the month. 
*)					     
fun dates_in_month([], m) = []
  | dates_in_month(d :: ds: date list, m) = 
    let val acc = dates_in_month(ds, m)
    in
	if #2d = m then d :: acc
	else acc
    end

(*
   Takes a list of dates and a list of months (an int list)and returns a list 
   holding the dates from the argument list of dates that are in any of the months 
   in the list of months. Assumes no repeated months.
*)	
fun dates_in_months(ds, []) = []
  | dates_in_months(ds, m :: ms) =
    dates_in_month(ds, m) @ dates_in_months(ds, ms)

(*
   Takes a list of strings and an int n and returns the nth element of the list 
   where the head of the list is 1st. Performs no bounds check.
*)					   
fun get_nth(s :: ss, n) =
    if n = 1 then s
    else get_nth(ss, n - 1)

(*
   Takes a date and return string in format Month DD, YYYY
*)		
fun date_to_string((y, m, d)) =
    get_nth(months, m) ^ " " ^ Int.toString(d) ^ ", " ^ Int.toString (y)

(*
   Takes a sum and a list of ints and returns an int n such that the first n elements 
   of the list add to less than sum, but the first n+1 elements of the list add to
   sum or more. Assumes all numbers are positive.
*)								     
fun number_before_reaching_sum(sum, n :: ns) =
    let fun stop(acc, idx, []) = idx
	  | stop(acc, idx, n :: ns) =
	    if acc + n >= sum then idx
	    else stop(acc + n, idx + 1, ns)
    in
	stop(0, 0, n :: ns)
    end

(*
   Takes a day of year (an int between 1 and 365) and returns what month that day is in. 
*)	
fun what_month(d) =
    1 + number_before_reaching_sum(d, days)

(*
   Takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn]
   where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the 
   month of day day2. Return empty list if day1 > day2.
*)				  
fun month_range(d1, d2) =
    if d1 > d2 then []
    else what_month(d1) :: month_range(d1 + 1, d2)

(*
   Takes a list of dates and evaluates to a date option. Evaluates to NONE if the 
   list has no dates and SOME d where d is the oldest date in the list.
*)				      
fun oldest([]) = NONE
  | oldest(d :: ds) =
    if null ds then SOME d
    else if is_older(d, hd ds) then oldest(d :: tl ds)
    else oldest(ds)

(*
   Takes a sorted list and returns a list with any duplicate entries removed.
*)	       
fun dedup(first :: second :: tail) =
    if first = second then dedup(second :: tail)
    else first :: dedup(second :: tail)
  | dedup l = l 				

(*
   Like number_in_months but ignores duplicates.
*)		  
fun number_in_months_challenge(ds, ms) =
    number_in_months(ds, dedup(ms))

(*
   Like dates_in_months but ignores duplicates.
*)		    
fun dates_in_months_challenge(ds, ms) =
    dates_in_months(ds, dedup(ms))

(*
   Takes a date and returns true if the ints are plausible as a real date
   in the common era.
*)		   
fun reasonable_date((year, month, day)) =
    let fun is_leap(y) =
	    y mod 400 = 0 orelse (y mod 4 = 0 andalso y mod 100 <> 0)
    in
	year > 0 andalso
	month > 0 andalso month < 13 andalso
	day > 0 andalso (
	    if month = 2 andalso is_leap(year) then day <= 29
	    else day <= get_nth(days, month)
	)
    end
	


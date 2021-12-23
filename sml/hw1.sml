fun is_older(first_date : int * int * int, second_date : int * int * int )=
	    if(#1 first_date = #1 second_date)then
		if(#2 first_date = #2 second_date)then
		    if(#3 first_date = #3 second_date)then
			false	    
		    else
			#3 first_date < #3 second_date			   
		else
		    #2 first_date < #2 second_date	       
	    else
		#1 first_date < #1 second_date

fun number_in_month(dates : (int * int * int)list, month : int)=
    if(null dates) then
	0
    else if(#2 (hd dates) = month)then
	number_in_month(tl dates, month) + 1
    else
	number_in_month(tl dates, month)
	  				      
fun number_in_months(dates : (int * int * int)list, months : int list)=
    if(null months) then
	0
    else
	number_in_months(dates, tl months) + number_in_month(dates, hd months)

fun dates_in_month(dates : (int * int * int)list, month : int)=
    if(null dates) then
	[]
    else if(#2 (hd dates) = month)then
	(hd dates)::dates_in_month(tl dates, month)
    else
	dates_in_month(tl dates, month)

fun dates_in_months(dates : (int * int * int)list, months : int list)=
    let
	fun check_date(date : int * int * int, months : int list)=
	    if(null months)then
		false
	    else if(#2 date = hd months)then
		true
	    else
		check_date(date, tl months)
    in
    if(null dates) then
	[]
    else
	if(check_date(hd dates, months)) then
	   (hd dates)::dates_in_months(tl dates, months) 
	else
	    dates_in_months(tl dates, months)
	    
    end
							  
fun get_nth(string_list : string list, n)=
    if(n = 1)then
	hd string_list
    else
	get_nth(tl string_list, n - 1)

fun date_to_string(date : int * int * int)=
    let
	val months = ["January", "February", "March", "April",
		      "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end
	
fun number_before_reaching_sum(sum : int, input_list : int list)=
    if (sum - (hd(tl input_list) + hd input_list) <= 0)then
	hd input_list
    else
	number_before_reaching_sum(sum - hd input_list, tl input_list)

fun what_month(day : int)=
    let
	val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

	fun what_month_recurssive(sum : int, input_list : int list, current_month : int)=
	    if (sum - (hd input_list) <= 0)then
		current_month
	    else
		what_month_recurssive(sum - hd input_list, tl input_list, current_month + 1)
    in
	what_month_recurssive(day, days_in_month, 1)
    end

fun month_range(day1 : int, day2 : int)=
    if(day1 > day2)then
	[]    
    else
	what_month(day1)::month_range(day1 + 1, day2)	      
    	
fun oldest(dates : (int * int * int) list)=
    let
	fun oldest_recurssive(oldest : (int * int * int), dates : (int * int *int) list)=
	    if(null dates)then
		oldest
	    else if(is_older(hd dates, oldest))then
		oldest_recurssive(hd dates, tl dates)
	    else
		oldest_recurssive(oldest, tl dates)
    in
	if(null dates)then
	    NONE
	else if(null(tl dates)) then
	    SOME(hd dates)
	else
	    SOME(oldest_recurssive(hd dates, tl dates))
    end
	
		
		       
		       
	
    

	  
    
			      
			 
							  

			      
	   

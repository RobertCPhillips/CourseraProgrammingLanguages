(*
Into:

In all problems, a “date” is an SML value of type int*int*int, where the ﬁrst part is the year, the second part is the month, and the third part is the day. 

A “reasonable” date has a positive year, a month between 1 and 12, and a day no greater than 31 (or less depending on the month). Your solutions need to work correctly only for reasonable dates, but do not check for reasonable dates (that is a challenge problem) and many of your functions will naturally work correctly for some/all non-reasonable dates. 

A “day of year” is a number from 1 to 365 where, for example, 33 represents February 2. (We ignore leap years except in one challenge problem.)
*)


(*
1. Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if the ﬁrst argument is a date that comes before the second argument. (If the two dates are the same, the result is false.)

val is_older = fn : (int * int * int) * (int * int * int) -> bool
*)

fun is_older(firstDate:int*int*int, secondDate:int*int*int) = 
  let
    val firstYear =   #1 firstDate
    val secondYear =  #1 secondDate
    val firstMonth =  #2 firstDate
    val secondMonth = #2 secondDate
    val firstDay =    #3 firstDate
    val secondDay =   #3 secondDate
  in
    if firstYear < secondYear
    then true
    else
      if firstYear > secondYear
      then false
      else
        (* year are equal *)
        if firstMonth < secondMonth
        then true
        else
          if firstMonth > secondMonth
          then false
          else
            (* year and month are equal *)
            if firstDay < secondDay
            then true
            else false
  end
  
(*
2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns how many dates in the list are in the given month.

val number_in_month = fn : (int * int * int) list * int -> int
*)

fun number_in_month(dates:(int*int*int) list, month:int) =
  let
    fun number_in_month_acc(lst:(int*int*int) list, acc:int) = 
      if null lst
      then acc
      else
        let 
          val accValue = if (#2 (hd lst)) = month
                         then acc + 1
                         else acc
        in                
          number_in_month_acc(tl lst, accValue)
        end
  in
    number_in_month_acc(dates, 0)
  end

(*
3. Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list) and returns the number of dates in the list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the previous problem

val number_in_months = fn : (int * int * int) list * int list -> int
*)

fun number_in_months(dates:(int*int*int) list, months:int list) = 
  let
    fun number_in_months_acc(lst:int list, acc:int) = 
      if null lst
      then acc
      else
        let
          val accValue = number_in_month(dates, hd lst)
        in
          number_in_months_acc(tl lst, acc + accValue)
        end
  in
    number_in_months_acc(months, 0)
  end
 
 (*
  Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a list holding the dates from the argument list of dates that are in the month. The returned list should contain dates in the order they were originally given.
  
 val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list
 *)
  
fun dates_in_month(dates:(int*int*int) list, month:int) = 
  let
    fun dates_in_month_acc(lst:(int*int*int) list, acc:(int*int*int) list) =
      if null lst
      then acc
      else
        if (#2 (hd lst) = month) 
        then (hd lst)::dates_in_month_acc(tl lst, acc)
        else dates_in_month_acc(tl lst, acc)
  in
    dates_in_month_acc(dates, [])
  end

(*
5.  Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list) and returns a list holding the dates from the argument list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the previous problem and SML’s list-append operator (@). 

val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list
*)

fun dates_in_months(dates:(int*int*int) list, months:int list) =
  let
    fun dates_in_months_acc(lst:int list, acc:(int*int*int) list) = 
      if null lst
      then acc
      else
        let
          val accValue = dates_in_month(dates, hd lst)
        in
          dates_in_months_acc(tl lst, acc@accValue)
        end
  in
    dates_in_months_acc(months, [])
  end

(*
6. Write a function get_nth that takes a list of strings and an int n and returns the nth element of the list where the head of the list is 1st. Do not worry about the case where the list has too few elements: your function may apply hd or tl to the empty list in this case, which is okay.

val get_nth = fn : string list * int -> string 
*)

fun get_nth(lst:string list, index:int) =
  let
    fun get_nth_acc(lst:string list, acc:int) =
    if acc = index
    then hd lst
    else get_nth_acc(tl lst, acc+1)
  in
    get_nth_acc(lst, 1)
  end

(*
7. Write a function date_to_string that takes a date and returns a string of the form January 20, 2013 (for example). Use the operator ^ for concatenating strings and the library function Int.toString for converting an int to a string. For producing the month part, do not use a bunch of conditionals. Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a comma following the day and use capitalized English month names: January, February, March, April, May, June, July, August, September, October, November, December.

val date_to_string = fn : int * int * int -> string 
*)

fun date_to_string(date:(int*int*int)) =
  let
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end
  
(*
8. Write a function number_before_reaching_sum that takes an int called sum, which you can assume is positive, and an int list, which you can assume contains all positive numbers, and returns an int. You should return an int n such that the ﬁrst n elements of the list add to less than sum, but the ﬁrst n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in value; it is okay for an exception to occur if this is not the case.

val number_before_reaching_sum = fn : int * int list -> int
*)

fun number_before_reaching_sum(sum:int, numbers:int list) = 
  let
    fun number_before_reaching_sum_acc(accNumbers:int list, accSum:int, accIndex:int) = 
      if accSum >= sum
      then accIndex-1
      else number_before_reaching_sum_acc(tl accNumbers, accSum+(hd accNumbers), accIndex+1)
  in
    number_before_reaching_sum_acc(numbers, 0, 0)
  end

(*
9. Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your answer to the previous problem.

val what_month = fn : int -> int
*)
fun what_month(dayOfYear:int) = 
  let
    val daysPerMonth = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
    number_before_reaching_sum(dayOfYear,daysPerMonth)+1
  end
  
(*
10. Write a function month_range that takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2

val month_range = fn : int * int -> int list
*)

fun month_range(dayOfYearStart:int, dayOfYearEnd:int) =
  let
    fun month_range_acc(dayAcc:int, lstAcc:int list) = 
      if dayAcc > dayOfYearEnd
      then lstAcc
      else what_month(dayAcc)::month_range_acc(dayAcc+1, lstAcc)
  in
    month_range_acc(dayOfYearStart, [])
  end
  
(*
11. Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.

val oldest = fn : (int * int * int) list -> (int * int * int) option
*)

fun oldest(dates:(int*int*int) list) =
 if null dates 
 then NONE
 else 
      let 
        fun oldest_nonempty(datesNotEmpty:(int*int*int) list) =
		  if null (tl datesNotEmpty)
		  then hd datesNotEmpty
		  else 
            let 
              val tailOldest = oldest_nonempty(tl datesNotEmpty)
		    in
			  if is_older(hd datesNotEmpty, tailOldest)
			  then hd datesNotEmpty
			  else tailOldest
		    end
	  in
	    SOME (oldest_nonempty dates)
	  end

(*
12. Challenge Problem: Write functions number_in_months_challenge and dates_in_months_challenge that are like your solutions to problems 3 and 5 except having a month in the second argument multiple times has no more effect than having it once. (Hint: Remove duplicates, then use previous work.)
*)
fun contains(lst:int list, number:int) =
  if null lst
  then false
  else  (number = hd lst) orelse contains(tl lst, number)

fun removeDuplicates(lst:int list) =
  if null lst
  then lst
  else
    let 
      fun removeDuplicatesAcc(lstOrig:int list, lstAcc:int list) =
        if null lstOrig
        then lstAcc
        else
          let
            val value = hd lstOrig
            val newLstAcc = if not (contains(lstAcc, value))
                            then lstAcc@[value]
                            else lstAcc
          in
            removeDuplicatesAcc(tl lstOrig, newLstAcc)
          end
    in
      removeDuplicatesAcc(lst, [])
    end

fun number_in_months_challenge(dates:(int*int*int) list, months:int list) =
  let
    val noDupes = removeDuplicates(months)
  in
    number_in_months(dates, noDupes)
  end

fun dates_in_months_challenge(dates:(int*int*int) list, months:int list) =
  let
    val noDupes = removeDuplicates(months)
  in
    dates_in_months(dates, noDupes)
  end


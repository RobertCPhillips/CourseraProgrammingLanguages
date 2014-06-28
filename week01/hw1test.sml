(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw1.sml";

(*TODO: create an 'assert' function to allow textual output as result*)

(* assignment 1 tests *)
val test1a = is_older((1,2,3),(2,3,4)) = true

val test1b = is_older((2,3,4),(1,2,3)) = false
val test1c = is_older((2,3,4),(2,2,3)) = false
val test1d = is_older((2,3,4),(2,3,3)) = false

val test1e = is_older((1,2,3),(1,3,4)) = true
val test1f = is_older((1,2,3),(1,2,4)) = true
val test1g = is_older((1,2,3),(1,2,3)) = false

(* assignment 2 tests *)
val test2a = number_in_month([(2012,2,28),(2013,12,1)],2) = 1
val test2b = number_in_month([(2012,2,28),(2013,12,1)],3) = 0
val test2c = number_in_month([(2012,4,28),(2013,4,1)],4) = 2

(* assignment 3 tests *)
val test3a = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3b = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,12]) = 4
val test3c = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2]) = 1
val test3d = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[8,9]) = 0
val test3e = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val test3f = number_in_months([],[2,3,4]) = 0
val test3g = number_in_months([],[]) = 0

(* assignment 4 tests *)
val test4a = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4b = dates_in_month([(2012,2,28),(2013,12,1),(2013,2,1)],2) = [(2012,2,28),(2013,2,1)]
val test4c = dates_in_month([(2012,2,28),(2013,12,1)],9) = []
val test4d = dates_in_month([],2) = []

(* assignment 5 tests *)
val test5a = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test5b = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = []
val test5c = dates_in_months([],[2,3,4]) = []
val test5d = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[9]) = []

(* assignment 6 tests *)
val test6a = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"
val test6b = get_nth(["hi", "there", "how", "are", "you"], 1) = "hi"
val test6c = get_nth(["hi", "there", "how", "are", "you"], 5) = "you"

(* assignment 7 tests *)
val test7 = date_to_string((2013, 6, 1)) = "June 1, 2013"

(* assignment 8 tests *)
val test8a = number_before_reaching_sum(10, [1,2,3,4,5]) = 3
val test8b = number_before_reaching_sum(1, [1,2,3,4,5]) = 0
val test8c = number_before_reaching_sum(14, [1,2,3,4,5]) = 4

(* assignment 9 tests *)
val test9a = what_month(70) = 3
val test9b = what_month(1) = 1
val test9c = what_month(31) = 1
val test9d = what_month(32) = 2
val test9e = what_month(365) = 12

(* assignment 10 tests *)
val test10a = month_range(31, 34) = [1,2,2,2]
val test10b = month_range(1, 1) = [1]
val test10c = month_range(365, 365) = [12]
val test10d = month_range(2,1) = []

(* assignment 11 tests *)
val test11a = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11b = oldest([]) = NONE
val test11c = oldest([(2012,1,1)]) = SOME (2012,1,1)

(* assignment 12 tests *)
val test12a = contains([1,2,3,4],1) = true
val test12b = contains([1,2,3,4],4) = true
val test12c = contains([1,2,3,4],3) = true
val test12d = contains([1,2,3,4],5) = false

val test12e = removeDuplicates([1,2,3,3,4,2,2,5,6,4,5,6,7]) = [1,2,3,4,5,6,7]

val test12f = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,2,3,4,4,3,2]) = 3

val test12g = dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,2,3,4,4,3,2]) = [(2012,2,28),(2011,3,31),(2011,4,28)]


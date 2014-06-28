(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1a = only_capitals ["A","B","C"] = ["A","B","C"]
val test1b = only_capitals ["a","B","c"] = ["B"]
val test1c = only_capitals [] = []

val test2a = longest_string1 ["A","bc","C"] = "bc"
val test2b = longest_string1 ["A","bc","CC"] = "bc"
val test2c = longest_string1 ["A","bc","CCC"] = "CCC"
val test2d = longest_string1 ["AAAA","bc","CCC"] = "AAAA"
val test2e = longest_string1 ["AAAA","bc","CCCC"] = "AAAA"
val test2f = longest_string1 [] = ""

val test3a = longest_string2 ["A","bc","C"] = "bc"
val test3b = longest_string2 ["A","bc","CC"] = "CC"
val test3c = longest_string2 ["A","bc","CCC"] = "CCC"
val test3d = longest_string2 ["AAAA","bc","CCC"] = "AAAA"
val test3e = longest_string2 ["AAAA","bc","CCCC"] = "CCCC"
val test3f = longest_string2 [] = ""

val test4aa = longest_string3 ["A","bc","C"] = "bc"
val test4ab = longest_string3 ["A","bc","CC"] = "bc"
val test4ac = longest_string3 ["A","bc","CCC"] = "CCC"
val test4ad = longest_string3 ["AAAA","bc","CCC"] = "AAAA"
val test4ae = longest_string3 ["AAAA","bc","CCCC"] = "AAAA"
val test4af = longest_string3 [] = ""

val test4ba = longest_string4 ["A","bc","C"] = "bc"
val test4bb = longest_string4 ["A","bc","CC"] = "CC"
val test4bc = longest_string4 ["A","bc","CCC"] = "CCC"
val test4bd = longest_string4 ["AAAA","bc","CCC"] = "AAAA"
val test4be = longest_string4 ["AAAA","bc","CCCC"] = "CCCC"
val test4bf = longest_string4 [] = ""

val test5a = longest_capitalized ["A","bc","C"] = "A";
val test5b = longest_capitalized ["AAAA","bc","CCC"] = "AAAA"
val test5c = longest_capitalized ["AAAA","bc","CCCC"] = "AAAA"
val test5d = longest_capitalized ["aa","bc","cc"] = ""

val test6a = rev_string "abc" = "cba";
val test6b = rev_string "a" = "a";
val test6c = rev_string "" = "";

val test7a = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7b = first_answer (fn x => if x > 6 then SOME x else NONE) [1,2,3,4,5] = 0 handle NoAnswer => true
val test7c = first_answer (fn x => if x > 6 then SOME x else NONE) [] = 0 handle NoAnswer => true

val test8a = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8b = all_answers (fn x => if x >= 2 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME[2,3,4,5,6,7]
val test8c = all_answers (fn x => if x < 5 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8d = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME[]

val test9aa = count_wildcards Wildcard = 1
val test9ab = count_wildcards (TupleP [Wildcard, Wildcard, ConstP(5)]) = 2
val test9ac = count_wildcards (ConstructorP ("dt", TupleP [Wildcard, Wildcard, Wildcard])) = 3 

val test9ba = count_wild_and_variable_lengths (Variable("a")) = 1
val test9bb = count_wild_and_variable_lengths (TupleP [Wildcard, Wildcard, ConstP(5)]) = 2
val test9bc = count_wild_and_variable_lengths (TupleP [Wildcard, Wildcard, Variable("as")]) = 4

val test9ca = count_some_var ("x", Variable("x")) = 1;
val test9cb = count_some_var ("x", Variable("a")) = 0
val test9cc = count_some_var ("x", TupleP [Wildcard, Wildcard, ConstP(5)]) = 0
val test9cd = count_some_var ("as", TupleP [Wildcard, Wildcard, Variable("as")]) = 1
val test9ce = count_some_var ("x", TupleP [Wildcard, Wildcard, Variable("as")]) = 0
val test9cf = count_some_var ("x", TupleP [Wildcard, Variable("x"), Wildcard, Variable("x")]) = 2

val test10a = check_pat (Variable("x")) = true
val test10b = check_pat (TupleP [Wildcard, Variable("x"), Wildcard, Variable("x")]) = false
val test10c = check_pat (TupleP [Wildcard, Variable("a"), Wildcard, Variable("x")]) = true
val test10d = check_pat (TupleP [Wildcard, Wildcard, ConstP(5)]) = true

val test11a = match (Const(1), UnitP) = NONE
val test11b = match (Const(1), Variable("x")) = SOME [("x", Const 1)]
val test11c = match (Constructor("x", Const(1)), ConstructorP("x", Variable("x"))) = SOME [("x", Const 1)]
val test11d = match (Constructor("x", Const(1)), ConstructorP("y", Variable("x"))) = NONE
val test11e = match (Const(1), ConstP(1)) = SOME[]
val test11f = match (Const(1), Wildcard) = SOME[]
val test11g = match (Unit, UnitP) = SOME[]
val test11h = match (Tuple [Unit, Const 17], TupleP [Variable("foo"), Variable("foo"), Wildcard]) = NONE

val test11z = match (Tuple [Const 1, Unit, Const 17], TupleP [Variable("foo"), Variable("foo"), Wildcard]) = SOME [("foo", Const 1), ("foo", Unit)]

val test12a = first_match Unit [UnitP] = SOME []

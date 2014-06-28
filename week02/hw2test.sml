(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1a = all_except_option("string", ["string"]) = SOME []
val test1b = all_except_option("string", ["string","string2","ss"]) = SOME["string2","ss"]
val test1c = all_except_option("string", ["string2","ss","string"]) = SOME["string2","ss"]
val test1d = all_except_option("string", ["string2","string","ss"]) = SOME["string2","ss"]
val test1e = all_except_option("string", ["string2","ss"]) = NONE
val test1f = all_except_option("string", ["string2"]) = NONE
val test1g = all_except_option("string", []) = NONE


val test2a = get_substitutions1([["foo"],["there"]], "foo") = []
val test2b = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test2c = get_substitutions1([], "foo") = []
val test2d = get_substitutions1([["blah"],["there"]], "foo") = []
val test2e = get_substitutions1([["Fred","Fredrick"]], "Fred") = ["Fredrick"]
val test2f = get_substitutions1([["Fred","Fredrick"],["Freddie","Fred", "F"]], "Fred") = ["Fredrick","Freddie","F"]


val test3a = get_substitutions2([["foo"],["there"]], "foo") = []
val test3b = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test3c = get_substitutions2([], "foo") = []
val test3d = get_substitutions2([["blah"],["there"]], "foo") = []
val test3e = get_substitutions2([["Fred","Fredrick"]], "Fred") = ["Fredrick"]
val test3f = get_substitutions2([["Fred","Fredrick"],["Freddie","Fred", "F"]], "Fred") = ["Fredrick","Freddie","F"]


val test4a = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test4b = similar_names([["Fred","Fredrick"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test4c = similar_names([], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}]         
val test4d = similar_names([["Elizabeth","Betty"]], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}]         

val test5a = card_color((Clubs, Num 2)) = Black
val test5b = card_color((Spades, Num 2)) = Black
val test5c = card_color((Hearts, Num 2)) = Red
val test5d = card_color((Diamonds, Num 2)) = Red


val test6a = card_value((Clubs, Num 2)) = 2
val test6b = card_value((Clubs, Num 10)) = 10
val test6c = card_value((Clubs, Ace)) = 11
val test6d = card_value((Clubs, King)) = 10
val test6e = card_value((Clubs, Queen)) = 10
val test6f = card_value((Clubs, Jack)) = 10

val test7a = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7b = remove_card([(Hearts, Ace),(Hearts, King)], (Hearts, Ace), IllegalMove) = [(Hearts, King)]
val test7c = remove_card([(Hearts, King),(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, King)]
val test7d = remove_card([(Hearts, King),(Hearts, Ace), (Hearts, Jack)], (Hearts, Ace), IllegalMove) = [(Hearts, King),(Hearts, Jack)]
val test7e = remove_card([], (Hearts, Ace), IllegalMove) = [] handle IllegalMove => true
val test7f = remove_card([(Hearts, King)], (Hearts, Ace), IllegalMove) = [] handle IllegalMove => true
val test7g = remove_card([(Hearts, King),(Hearts, Jack)], (Hearts, Ace), IllegalMove) = [] handle IllegalMove => true

val test8a = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true
val test8b = all_same_color([(Hearts, Ace)]) = true
val test8c = all_same_color([]) = true
val test8d = all_same_color([(Hearts, Ace), (Spades, Ace)]) = false

val test9a = sum_cards([(Clubs, Num 2),(Spades, Num 4)]) = 6
val test9b = sum_cards([(Clubs, Num 2)]) = 2
val test9c = sum_cards([]) = 0
val test9d = sum_cards([(Clubs, Num 2),(Spades, Ace),(Diamonds, King),(Hearts, Queen)]) = 33

val test10 = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11a = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test11b = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw], 42) = 3
val test11c = ((officiate([(Clubs,Jack),(Spades,Num(8))], [Draw,Discard(Hearts,Jack)], 42); false) handle IllegalMove => true)
             
             

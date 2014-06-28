(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(******************* put your solutions for problem 1 here *******************)

(*****1a*****)
fun all_except_option(s, lst) = 
  case lst of 
    [] => NONE
  | hd::tl => if same_string(s,hd)
              then SOME(tl)
              else case all_except_option(s, tl) of
                NONE => NONE
	          | SOME(x) => SOME(hd::x)

(*****1b*****)
fun get_substitutions1(lst, s) =
  case lst of 
    [] => []
  | hd::tl => case all_except_option(s, hd) of
                NONE => get_substitutions1(tl, s)
              | SOME(x) => x@get_substitutions1(tl, s)

(*****1c*****)
fun get_substitutions2(lst, s) =
  let
    fun get_substitutions2Acc(lstAcc, acc) = 
      case lstAcc of 
        [] => acc
      | hd::tl => case all_except_option(s, hd) of
                    NONE => get_substitutions2Acc(tl, acc)
                  | SOME(x) => get_substitutions2Acc(tl, acc@x)
  in
    get_substitutions2Acc(lst, [])
  end

(*****1d*****)
type fullName = {first:string, middle:string, last:string}

fun similar_names(lst, name:fullName) = 
  case name of 
    {first=f,middle=m,last=l} => 
      let
        val nicknames = get_substitutions2(lst, f)
        
        fun similar_namesAcc(lstAcc, acc) = 
          case lstAcc of
            [] => acc
          | hd::tl => similar_namesAcc(tl, acc@[{first=hd,middle=m,last=l}]) 
      in
        similar_namesAcc(nicknames, [name])
      end
  
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*****2a*****)
fun card_color(theCard) =
  case theCard of
    (Spades,_) => Black
  | (Clubs,_) => Black
  | (Diamonds,_) => Red
  | (Hearts,_) => Red  

(*****2b*****)
fun card_value(theCard) =
  case theCard of
    (_, Ace) => 11
  | (_, King) => 10
  | (_, Queen) => 10
  | (_, Jack) => 10
  | (_, Num x) => x  

(*2c*)
fun remove_card(cardList, theCard, e) =
  case cardList of 
    [] => raise e
  | hd::tl => if theCard = hd
              then tl
              else case remove_card(tl, theCard, e) of
                [] => [hd]
	          | x => hd::x

(*2d*)
fun all_same_color(cardList) = 
  case cardList of
    [] => true 
  | _::[] => true 
  | hd::(hdNext::tl) => card_color(hd) = card_color(hdNext) andalso all_same_color(hdNext::tl) 

(*2e*)
fun sum_cards(cardList) =
  let
    fun sum_cardsAcc(restOfCards, acc) = 
      case restOfCards of
        [] => acc
      | hd::tl => sum_cardsAcc(tl, card_value(hd)+acc)
  in
    sum_cardsAcc(cardList, 0)
  end

(*2f*)
fun score(cardList, goal) = 
  let
    val sum = sum_cards(cardList)
    val areSameColor = all_same_color(cardList)
    val prelimScore = if sum > goal then 3*(sum-goal) else (goal-sum)
  in
    if areSameColor then prelimScore div 2 else prelimScore
  end

(*2e*)  
fun officiate(cardList, moveList, goal) = 
  let
    fun nextMove(restOfCards, restOfMoves, heldCards) = 
      case (restOfCards, restOfMoves) of 
        (_,[]) => score(heldCards, goal)
      | (cs, (Discard c)::mtl) => 
          let
            val hand = remove_card(heldCards, c, IllegalMove)
          in
            nextMove(cs, mtl, hand)
          end 
      | (chd::[], Draw::mtl) => score(chd::heldCards, goal)
      | (chd::ctl, Draw::mtl) => 
          let
            val hand = chd::heldCards
          in
            if sum_cards(hand) > goal
            then score(hand, goal)
            else nextMove(ctl, mtl, hand)
          end 
  in
    nextMove(cardList, moveList, [])
  end


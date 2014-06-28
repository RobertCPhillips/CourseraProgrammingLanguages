(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(*1*)
fun only_capitals(strList) = List.filter(fn s => Char.isUpper(String.sub(s, 0)))(strList)

(*2*)
fun longest_string1(strList) = List.foldl(fn(s,i) => if String.size(s) > String.size(i) then s else i)("")(strList)

(*3*)
fun longest_string2(strList) = List.foldl(fn(s,i) => if String.size(i) > String.size(s) then i else s)("")(strList)

(*4*)
fun longest_string_helper f strlst = List.foldl(fn(x,y) => if f(String.size(x),String.size(y)) then x else y)("")(strlst);

val longest_string3 = fn strList => longest_string_helper(fn(s,i) => s > i)(strList)

val longest_string4 = fn strList => longest_string_helper(fn(s,i) => i > s)(strList)

(*5*)
val longest_capitalized = fn strList =>  (longest_string3 o only_capitals) strList

(*6*)
fun rev_string(str) = (String.implode o List.rev o String.explode) str

(*7*)
fun first_answer f lst = 
  case lst of 
    [] => raise NoAnswer
  | hd::tl => case f(hd) of
      SOME v => v
    | _ => first_answer f tl;

(*8*)
fun all_answers f lst = 
  let 
    fun all_answers_acc(lst, acc) = case lst of
        [] => SOME acc
      | hd::tl => case f(hd) of
          NONE => NONE
        | SOME lst2 => all_answers_acc(tl, acc@lst2)
  in
    all_answers_acc(lst, [])
  end

(*9a*)
fun count_wildcards p = g (fn _ => 1)(fn _ => 0)(p);

(*9b*)
fun count_wild_and_variable_lengths p = g (fn _ => 1)(fn s => String.size(s))(p);

(*9c*)
fun count_some_var(str,p) = g (fn _ => 0)(fn s => if str = s then 1 else 0)(p);

(*10*)
fun check_pat p = g (fn _ => 0)(fn s => count_some_var(s,p) - 1)(p) = 0

(*11*)
fun match(v,p) = case (v,p) of
    (v, Variable(x)) => SOME [(x,v)]
  | (Constructor(x1,y1), ConstructorP(x2,y2)) => if x1 = x2 then match(y1,y2) else NONE
  | (Tuple(vLst), TupleP(pLst)) =>  if List.length vLst <> List.length pLst then NONE
                                    else all_answers (match)(ListPair.zip(vLst,pLst))
  | (_, Wildcard) => SOME []
  | (Unit, UnitP) => SOME []
  | (Const x, ConstP y) => if x = y then SOME [] else NONE
  | _ => NONE
  
(*12
Write a function first_match that takes a value and a list of patterns and returns a (string * valu) list option, namely NONE if no pattern in the list matches or SOME lst where lst is the list of bindings for the ﬁrst pattern in the list that matches. Use first_answer and a handle-expression. Hints: Sample solution is 3 lines*)
fun first_match v pLst = SOME (first_answer(fn(x) => match(v,x))(pLst)) handle NoAnswer => NONE
  

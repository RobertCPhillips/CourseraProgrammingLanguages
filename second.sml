(*second week scratch pad*)

(*
building blocks: each of, one of, self reference

each-of
 - tuples (int and bool)
 - Records
 
one-of
 - options (does or does not)
 - datatype binding

*)

val record1 = {bar=(1+2,true) , foo=22, baz=(false,9)}

(*
bindings
 - variable (val)
 - function (fun)
 - datatype...
*)

datatype myType = TwoInts of (int*int) (*these are the constructors*)
                 | Str of string
                 | Stuff

(* myType -> int *)      
fun f (x:myType) = (* myType can be inferred *) 
  case x of
       Stuff => 3
    |  Str x => 8
    |  TwoInts(i1, i2) => i1 + i2

(* more datatypes *)
datatype Suit = Club | Spade | Heart | Diamond
datatype Rank = Jack | Queen | King | Ace | Num of int

type card = Suit * Rank
type deck = card list



    
     

(* this is a comment *)
(* syntax rules, type checking rules, evaluation rules *)

(* variables are a 'binding' *)

(*static environemt*)
(*type checking*)
(*dynamic envrionment*)

fun powerAcc(x:int, y:int, acc:int) = 
  if y=0 then acc else powerAcc(x, y-1, x*acc)

fun power(x:int, y:int) = powerAcc(x, y, 1)

val p1 = (21,22);
val p2 = (p1, p1);

val list1 = [1,2,3,4];

fun mapAcc(lst: int list, f: int -> int, acc: int list) = 
  if null lst then acc
  else mapAcc(tl lst, f, f(hd lst)::acc)

fun map(lst: int list, f: int -> int) = mapAcc(lst, f, [])

fun map2(lst: int list, f: int -> int) = 
  if null lst then []
  else f(hd lst)::map(tl lst, f)

fun sumList(lst: int list) = 
  let
    fun sumListAcc(lst: int list, acc: int) =
      if null lst then acc
      else sumListAcc(tl lst, hd lst + acc)
  in
    sumListAcc(lst, 0)
  end





(* University of Washington, Programming Languages, Homework 7
   hw7testsprovided.sml *)
(* Will not compile until you implement preprocess and eval_prog *)

(* These tests do NOT cover all the various cases, especially for intersection *)

use "hw7.sml";

(* Must implement preprocess_prog and Shift before running these tests *)

fun real_equal(x,y) = Real.compare(x,y) = General.EQUAL;

(* Preprocess tests *)
let
	val Point(a,b) = preprocess_prog(LineSegment(3.2,4.1,3.2,4.1))
	val Point(c,d) = Point(3.2,4.1)
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "preprocess converts a LineSegment to a Point successfully\n")
	else (print "****preprocess does not convert a LineSegment to a Point succesfully\n")
end;

let 
	val LineSegment(a,b,c,d) = preprocess_prog (LineSegment(3.2,4.1,~3.2,~4.1))
	val LineSegment(e,f,g,h) = LineSegment(~3.2,~4.1,3.2,4.1)
in
	if real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
	then (print "preprocess flips an improper LineSegment1 successfully\n")
	else (print "****preprocess does not flip an improper LineSegment1 successfully\n")
end;

let 
	val LineSegment(a,b,c,d) = preprocess_prog (LineSegment(3.2,4.1,~3.2,4.1))
	val LineSegment(e,f,g,h) = LineSegment(~3.2,4.1,3.2,4.1)
in
	if real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
	then (print "preprocess flips an improper LineSegment2 successfully\n")
	else (print "****preprocess does not flip an improper LineSegment2 successfully\n")
end;

let 
	val LineSegment(a,b,c,d) = preprocess_prog (LineSegment(3.2,4.1,3.2,~4.1))
	val LineSegment(e,f,g,h) = LineSegment(3.2,~4.1,3.2,4.1)
in
	if real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
	then (print "preprocess flips an improper LineSegment3 successfully\n")
	else (print "****preprocess does not flip an improper LineSegment3 successfully\n")
end;

let 
	val LineSegment(a,b,c,d) = preprocess_prog (LineSegment(3.2,~4.1,3.2,4.1))
	val LineSegment(e,f,g,h) = LineSegment(3.2,~4.1,3.2,4.1)
in
	if real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
	then (print "preprocess flips an improper LineSegment4 successfully\n")
	else (print "****preprocess does not flip an improper LineSegment4 successfully\n")
end;

let
  val Intersect(Point(a,b), Point(e,f)) = preprocess_prog(Intersect(LineSegment(~3.7,1.5,~3.7,1.5),LineSegment(~3.7,1.5,~3.7,1.5)))
  val Intersect(Point(i,j), Point(m,n)) = Intersect(Point(~3.7,1.5),Point(~3.7,1.5))
in
  if real_equal(a,i) andalso real_equal(b,j) andalso
     real_equal(e,m) andalso real_equal(f,n)   
  then (print "preprocess flips an improper LineSegment5 successfully\n")
  else (print "****preprocess does not flip an improper LineSegment5 successfully\n")
end;

(* eval_prog tests with Shift*)
let 
	val Point(a,b) = (eval_prog (preprocess_prog (Shift(3.0, 4.0, Point(4.0,4.0))), []))
	val Point(c,d) = Point(7.0,8.0) 
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "eval_prog with empty environment worked\n")
	else (print "eval_prog with empty environment is not working properly\n")
end;

(* Using a Var *)
let 
	val Point(a,b) = (eval_prog (Shift(3.0,4.0,Var "a"), [("a",Point(4.0,4.0))]))
	val Point(c,d) = Point(7.0,8.0) 
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "eval_prog with 'a' in environment is working properly\n")
	else (print "eval_prog with 'a' in environment is not working properly\n")
end;


(* With Variable Shadowing *)
let 
	val Point(a,b) = (eval_prog (Shift(3.0,4.0,Var "a"), [("a",Point(4.0,4.0)),("a",Point(1.0,1.0))]))
	val Point(c,d) = Point(7.0,8.0) 
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "eval_prog with shadowing 'a' in environment is working properly\n")
	else (print "eval_prog with shadowing 'a' in environment is not working properly\n")
end;

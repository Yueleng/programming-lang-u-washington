

(* Records *)
val x = {bar = (3, true), baz = (false, 9), foo = 7};
val my_niece = {id=41111,name="Amelia"};
val id_niece = #id my_niece;
val brain_part = {ego=false, id=true, supergo=false};

(* Tuples as Syntactic Sugar *)
val a_pair = (3+1, 4+2);
val a_record = {first=4, second=6};
val another_pair = {2=5,1=6};

val x1 = {3="hi", 1=true};
val y = {3="hi", 1=true, 2=3+2};

(* Datatype Bindings *)
datatype mytype = TwoInts of int * int
		| Str of string
                | Pizza;

val a = Str " hi";
val b = Str;
val c = Pizza;
val d = TwoInts(1+2,3+4);
val e = a;
	   
(* Case Expression *)
fun f x =
    case x of
	Pizza => 3
      | Str s => 8
      | TwoInts(i1, i2) => i1 + i2;

(*|Pizza => 4; (* redundant case: error *) *)
(* fun g x = case x of Pizza => 3 (* missing cases: warning *)  *)

(* Useful Datatypes *)
datatype suit = Club | Diamond | Heart | Spade;
datatype rank = Jack | Queen | King | Ace | Num of int;

datatype id = StudentNum of int
	| Name of string * (string option) * string

datatype exp = Constant of int
	     | Negate of exp
	     | Add of exp * exp
	     | Multiply of exp * exp;



fun eval e =
    case e of
       Constant i => i
      | Negate e2 => ~(eval e2)
      | Add (e1, e2) => (eval e1) + (eval e2)
      | Multiply(e1, e2)  => (eval e1) * (eval e2);
val example_exp : exp = Add(Constant 19, Negate (Constant 4));
val example_ans : int = eval example_exp;

fun number_of_adds e = (* exp -> int *)
    case e of
	Constant i => 0
      | Negate e2 => number_of_adds e2
      | Add(e1, e2)  => 1 + number_of_adds e1 + number_of_adds e2
      | Multiply(e1, e2)  => number_of_adds e1 + number_of_adds e2;

val example_addcount = number_of_adds(Multiply(example_exp, example_exp));

(* Another Expression Example *)
fun max_constant e =
    case e of
	Constant i => i
      | Negate e2 => max_constant e2
      | Add(e1, e2) => if max_constant e1 > max_constant e2
		       then max_constant e1
		       else max_constant e2
      | Multiply(e1, e2) => if max_constant e1 > max_constant e2
	                    then max_constant e1
	                    else max_constant e2


val test_exp = Add(Constant 19, Negate(Constant 4));
val nineteen = max_constant test_exp;


fun max_constant2 e =
    let fun max_of_two(e1, e2) =
	    let val m1 = max_constant2 e1
	        val m2 = max_constant2 e2
	    in if m1 > m2 then m1 else m2 end
    in
	case e of
	Constant i => i
      | Negate e2 => max_constant2 e2
      | Add(e1, e2) => max_of_two(e1, e2)
      | Multiply(e1, e2) => max_of_two(e1, e2)

    end;
				      
val nineteen2 = max_constant2 test_exp;


fun max_constant3 e =
    let fun max_of_two(e1, e2) =
	    Int.max(max_constant3 e1, max_constant3 e2)
    in
	case e of
	Constant i => i
      | Negate e2 => max_constant3 e2
      | Add(e1, e2) => max_of_two(e1, e2)
      | Multiply(e1, e2) => max_of_two(e1, e2)

    end;

val nineteen3 = max_constant3 test_exp;


fun max_constant4 e =
      case e of
      Constant i => i
    | Negate e2 => max_constant3 e2
    | Add(e1, e2) => Int.max(max_constant4 e1, max_constant4 e2)
    | Multiply(e1, e2) => Int.max(max_constant4 e1, max_constant4 e2);
val nineteen4 = max_constant4 test_exp;


(* Type Synonyms *)
type card = suit * rank;

type name_record = {student_num : int option,
		    first       : string,
		    middle      : string option,
		    last        : string}
fun is_Queen_of_Spades (c : card) =
    #1 c = Spade andalso #2 c = Queen;

val c1 : card = (Diamond, Ace);
val evalc1 = is_Queen_of_Spades c1;
val c2 : suit * rank = (Heart, Ace);
val evalc2 = is_Queen_of_Spades c2;
val c3 = (Spade, Ace);
val evalc3 = is_Queen_of_Spades c3;
(* Use case expression, avoid specifying type in parameter *)
fun is_Queen_of_Spades2 c =
    case c of
	(Spade, Queen) => true
     | _  => false

val c11 : card = (Diamond, Ace);
val evalc11 = is_Queen_of_Spades2 c11;
val c22 : suit * rank = (Heart, Ace);
val evalc22 = is_Queen_of_Spades2 c22;
val c33 = (Spade, Ace);
val evalc33 = is_Queen_of_Spades2 c33;


(* List and Options are datatypes *)
datatype my_int_list = Empty
		     | Cons of int * my_int_list

val x = Cons(4, Cons(23, Cons(2008, Empty)))

fun append_my_list (xs, ys) =
    case xs of
	Empty => ys
      | Cons(x, xs') => Cons(x, append_my_list(xs', ys));
(* Options are datatypes *)
fun inc_or_zero intoption =
    case intoption of
	NONE => 0
      | SOME i => i + 1;
fun sum_list xs =
    case xs of
	[] => 0
      | x::xs' => x + sum_list xs';

fun append (xs, ys) =
    case xs of
	[] => ys
      | x::xs' => x::append(xs', ys);


(* Polymorphic Datatypes *)
datatype 'a myoption = NONE | SOME of 'a;

datatype 'a mylist = Empty | Cons of 'a * 'a mylist;

datatype ('a, 'b) tree =
	 Node of 'a * ('a, 'b) tree * ('a, 'b) tree
	 | Leaf of 'b;
(* type is (int, int) tree -> int *)
fun sum_tree tr =
    case tr of
	Leaf i => i
      | Node(i,lft,rgt) => i + sum_tree lft + sum_tree rgt;
(* type is ('a, int) tree -> int *)
fun sum_leaves tr =
    case tr of
	Leaf i => i
      | Node(i,lft,rgt) => sum_leaves lft + sum_leaves rgt;

(* type is ('a, 'b) tree -> int *)
fun num_leaves tr =
    case tr of
	Leaf i => 1
      | Node(i, lft, rgt) => num_leaves lft + num_leaves rgt;



(* Pattern Matching: Best Section*)
(* poor style*)
fun sum_triple triple =
    case triple of
	(x, y, z) => x + y + z;
(* poor style*)
fun full_name r =
    case r of
	{first=x, middle=y, last=z} =>
	x ^ " " ^ y ^ " " ^ z;

(* better style*)
fun sum_triple_better triple =
    let
	val (x,y,z) = triple
    in
	x + y + z
    end;

fun full_name r =
    let val {first=x, middle=y, last=z} = r
    in
	x ^ " " ^ y ^ " " ^ z
    end;

(*final style: fun f p = e*)

fun sum_triple_final(x, y, z) =
    x + y + z;

fun full_name_final {first = x, middle=y, last = z} =
    x ^ " " ^ y ^ " " ^ z;

(* Enables cute and useful things you cannot do in Java, e.g., *)
fun rotate_left (x, y, z) = (y, z, x);
fun rotate_right t = rotate_left(rotate_left t);

(* A Little Type Inference *)
(* int * 'a * int -> int , unexpected polymorphism*)
fun partial_sum (x, y, z) =
    x + z;

(* euqal type *)
(* ''a * ''a -> string *)
fun same_thing(x,y) =
    if x=y then "yes" else "no";
fun is_three x =
    if x=3 then "yes" else "no";

(* Nested Pattern Match *)

(* don't do this *)
exception ListLengthMismatch;

fun old_zip3(l1, l2, l3) =
    if null l1 andalso null l2 andalso null l3
    then []
    else if null l1 orelse null l2 orelse null l3
    then raise ListLengthMismatch
    else (hd l1, hd l2, hd l3) :: old_zip3(tl l1, tl l2, tl l3);


(* don't do this *)
fun shallow_zip3(l1, l2, l3) =
    case l1 of
	[] =>
	(case l2 of
	     [] => (case l3 of
			[] => []
		      | _ => raise ListLengthMismatch)
	   | _ => raise ListLengthMismatch)
      | hd1::tl1 =>
	(case l2 of
	     [] => raise ListLengthMismatch
	   | hd2::tl2 => (case l3 of
			      [] => raise ListLengthMismatch
			    | hd3::tl3 =>
			      (hd1, hd2, hd3)::shallow_zip3(tl1,tl2,tl3)));

(* do this *)

fun zip3 list_triple =
    case list_triple of
	([],[],[]) => []
      | (hd1::tl1,hd2::tl2,hd3::tl3) => (hd1, hd2, hd3)::zip3(tl1,tl2,tl3)
      | _ => raise ListLengthMismatch;

(* and the inverse *)
fun unzip3 lst =
    case lst of
	[] => ([],[],[])
      | (a,b,c)::tl => let
	                   val (l1, l2, l3) = unzip3 tl
		       in
			   (a::l1, b::l2, c::l3)
		       end
			   

(* more nested patterns *)
fun nondecreasing xs = (* int list -> bool *)
    case xs of
	[] => true
      | x::[] => true
      | x::xs' => case xs' of
		     [] => true
		   | y::ys' => x <= y andalso nondecreasing xs';

fun nondecreasing2 xs =
    case xs of
	[] => true
      | _::[] => true
      | head::(neck::body) => head <= neck andalso nondecreasing2(neck::body);

datatype sgn = P | N | Z;
fun multsign (x1, x2) = (* int * int -> sgn *)
    let fun sign x = if x = 0 then Z else if x > 0 then P else N
    in
	     case (sign x1, sign x2) of
		 (Z, _)  => Z
	       | (_, Z) => Z
               | (P, P) => P
	       | (N, N) => P
	       | _ => N
    (*	       | (N,P) => N
	       | (P, N) => N *)
		
    end;

fun len xs =
    case xs of
	[] => 0
      | _ :: xs' => 1 + len xs';
      

(* Optional: Function Patterns *)
fun alt_eval (Constant i) = i
  | alt_eval (Negate e2) = ~(alt_eval e2)
  | alt_eval (Add(e1,e2)) = (alt_eval e1) + (alt_eval e2)
  | alt_eval (Multiply(e1,e2)) = (alt_eval e1) * (alt_eval e2);

fun append_alt ([], ys) = ys
  | append_alt (x::xs, ys) = x::append_alt(xs, ys);


(* Exceptions *)
fun hd xs =
    case xs of
	[] => raise List.Empty
      | x::_ => x;


exception MyUndesirableCondition;

(* pass data out exception *)
exception MyOtherException of int * int;

fun mydiv(x,y) =
    if y = 0
    then raise MyUndesirableCondition
    else x div y;

fun maxlist(xs, ex) = (* int list * exn -> int *)
    case xs of
	[] => raise ex
      | x::[] => x
      | x::xs' => Int.max(x, maxlist(xs', ex));


val w = maxlist([3,4,5], MyUndesirableCondition);

val z = maxlist([], MyUndesirableCondition) (* 42 *)
	handle MyUndesirableCondition => 42;


(* Tail Recursive *)

fun fact n =
    if n = 0 then 1 else n * fact(n - 1);



fun fact_r n =
    let
	fun aux(n, acc) =
	    if n = 0
	    then acc
	    else aux(n-1, acc*n)
    in
	aux(n,1)
    end;


	
val f = fact_r 3;

fun sum xs =
    case xs of
	[] => 0
      | x::xs' => x + sum xs';


fun sum_r xs =
    let fun aux(xs, acc) =
	    case xs of
		[] => acc
	      | x::xs' => aux(xs',x+acc)
    in
	aux(xs,0)
    end;


	
fun rev xs =
    case xs of
	[] => []
     | x::xs'  => rev xs' @ [x]

fun rev_r xs =
    let
	fun aux(xs, acc) =
	    case xs of
		[] => []
	      | x::xs' => aux(xs', x::acc)
    in
	aux(xs, [])
    end;


(* Section 4: Type Inference Examples *)

(* 
   f : T1 -> T2 [must be a function; all functions take 1 arg]
   x : T1
   
   y : T3
   z : T4
   T1 = T3 * T4 [else pattern match does not type-check]
   T3 = int [abs has type int -> int]
   T4 = int [because we added z to an int]
   So T1 = int * int
   So (abs y) + z : int, so let expression : int, so body : int 
   T2 = int
   f : int * int -> int
*)

fun f x =
    let val (y,z) = x in
	(abs y) + z
    end



(* 
   sum : T1 -> T2
   xs : T1
   
   x: T3
   xs : T3 list [pattern match a T1]
   
   T1 = T3 list
   T2 = int [because 0 might be returned]
   T3 = int [becuase x:T3 and we add x to something]
   from T1 = T3 list and T3 = int, we know T1 = int list
   from that and T2=int, we know f : int list -> int
  *)
fun sum xs =
    case xs of
	[] => 0
      | x::xs' => x + (sum xs');

(* 
lenght : T1 -> T2
xs : T1

x : T3
xs : T3 list

T1 = T3 list
T2 = int

T3 list -> int
'a list -> int
*)
fun length xs =
    case xs of
	[] => 0
      | x::xs' => 1 + (length xs');


(* 'a * 'a * 'b -> 'a * 'a * 'b *)
fun f (x, y, z) =
    if true
    then (x,y,z)
    else (y,x,z);


(* 
('a -> 'b) * ('c -> 'a) -> ('c -> 'b)
*)
fun compose (f, g) = fn x => f (g, x);


(* an example of mutual recursion: a little "state machine" for 
deciding if a list of ints alternates between 1 and 2,
not ending with a 1.
(There are simpler solutions to this problem, but this approach works for /any/finite state machine. *)

fun match xs = (* [1,2,1,2,1,2] *)
    let
	fun s_need_one xs =
	    case xs of
		[] => true
	      | 1::xs' => s_need_two xs'
	      | _ => false
	and s_need_two xs =
	    case xs of
		[] => false
	      | 2::xs' => s_need_one xs'
	      | _ => false
    in
	s_need_one xs
    end;

val x = match [1,2,1,2,1,2];


(* mutual recursive datatype *)
datatype t1 = Foo of int | Bar of t2
     and t2 = Baz of string | Quux of t1;

fun no_zeros_empty_strings_t1 x =
    case x of
	Foo i => i <> 0
      | Bar y => no_zeros_empty_strings_t2 y
and no_zeros_empty_strings_t2 x =
    case x of
	Baz s => size s > 0
      | Quux y => no_zeros_empty_strings_t1 y;


(* code above works fine. This version works without any new language support. *)
(* slower implementation *)
fun no_zeros_or_empty_strings_t1_alternate(f, x) = (* f: t2 -> bool *)
    case x of
	Foo i => i <> 0
      | Bar y => f y;

fun no_zeros_or_empty_string_t2_alternate x =
    case x of
	Baz s => size s > 0
      | Quux y => no_zeros_or_empty_strings_t1_alternate(
		     no_zeros_or_empty_string_t2_alternate,
		     y);


		   
					  
(* Modules for Namespace Management *)
structure MyMathLib =
struct
fun fact x =
    if x = 0
    then 1
    else x * fact(x - 1);
val half_pi = Math.pi / 2.0;
fun doubler y = y + y;
end;

val pi = MyMathLib.half_pi + MyMathLib.half_pi;
val twenty_eight = MyMathLib.doubler 14;


(* Signatures and Hidings Things *)
signature MATHLIB =
sig
    val fact : int -> int
    val half_pi : real
    val doubler : int -> int
end

structure MyMathLib1 :> MATHLIB =
struct
fun fact x =
    if x = 0
    then 1
    else x * fact(x - 1);

val half_pi = Math.pi / 2.0;

fun doubler y = y + y;

end;

(* A Module Example *)

(* A signature's type binding for a function must be more specific than the type binding for the function in the module *)
signature RATIONAL_A =
sig
    (* datatype rational = Frac of int * int | Whole of int *)
    (* Hide this in the implementation only or use the following*)
    type rational
    
    exception BadFrac
    val Whole: int -> rational (* reveal sth, hide sth*)
val make_frac : int * int -> rational
val add : rational * rational -> rational
val toString : rational -> string
end;

structure Rational1 :> RATIONAL_A =
struct
datatype rational = Whole of int | Frac of int * int;
exception BadFrac;

fun gcd(x, y) =
    if x=y
    then x
    else if x < y
    then gcd(x, y-x)
    else gcd(y, x-y);

fun reduce r =
    case r of Whole _ => r
	    | Frac(x, y) =>
	      if x = 0
	      then Whole 0
	      else let
		  val d = gcd(abs x, abs y)
	      in
		  if d = y
		  then Whole(x div d)
		  else Frac(x div d, y div d)
	      end;
fun make_frac(x,y) =
    if y = 0
    then raise BadFrac
    else if y < 0
    then reduce(Frac(~x, ~y))
    else reduce(Frac(x,y));

fun add(r1, r2) =
    case (r1, r2) of
	(Whole(i), Whole(j)) => Whole(i+j)
      | (Whole(i),Frac(j,k)) => Frac(j+k*i, k)
      | (Frac(j,k), Whole(i)) => Frac(j+k*i, k)
      | (Frac(a,b), Frac(c,d)) => reduce( Frac(a*d + b*c, b*d) );

fun toString r =
    case r of
	Whole i => Int.toString i
      | Frac(a,b) => (Int.toString a) ^ "/" ^ (Int.toString b);

end ;

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

fun only_capitals strs =
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) strs

fun longest_string1 strs =
    List.foldl (fn (x, acc) => if String.size acc >= String.size x then acc else x) "" strs

fun longest_string2 strs =
    List.foldl (fn (x, acc) => if String.size acc > String.size x then acc else x) "" strs

fun longest_string_helper f strs =
    List.foldr (fn (x, acc) => if f (String.size acc, String.size x) then acc else x) "" strs
	       
val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)
					    
val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of SOME x => x 
			    | NONE => first_answer f xs'
						  
fun all_answers f xs =
    let
	fun helper f xs acc =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of SOME (y::ys') => helper f xs' (y::ys' @ acc)
				    | SOME [] => helper f xs' acc
				    | NONE => NONE
    in
	helper f xs []
    end

fun count_wildcards pat =
    g (fn () => 1) (fn x => 0) pat
      
fun count_wild_and_variable_lengths pat =
    g (fn () => 1) (fn x => String.size x) pat

fun count_some_var (str, pat) =
    g (fn () => 0) (fn x => if x=str then 1 else 0) pat

fun get_var_strings pat =
    case pat of
	Variable x => [x]
      | TupleP ps => List.foldl (fn (p, acc) => (get_var_strings p) @ acc) [] ps
      | ConstructorP (_, p) => get_var_strings p
      | _ => []

fun only_uniques xs =
    case xs of
	[] => true
      | x::xs' => if List.exists (fn y => y = x) xs' then false else only_uniques(xs')

val  check_pat = only_uniques o get_var_strings

fun match (vl, pat) =
    case (vl, pat) of
	(v, Variable s) => SOME [(s, v)]
      | (Tuple vs, TupleP ps) => if (List.length vs) = (List.length ps)
				 then all_answers match (ListPair.zip (vs, ps))
				 else NONE
      | (_, Wildcard) => SOME[]
      | (Unit, UnitP) => SOME []
      | (Const v, ConstP p) => if v=p then SOME [] else NONE
      | (Constructor (s1, v), ConstructorP (s2, p)) => if s1=s2
						       then match (v, p)
						       else NONE
      | _ => NONE


fun curry f x y = f (x, y)
		    
fun first_match v pats =
    case pats of
	[] => NONE
      | p::ps' => SOME (first_answer (curry match v) pats) handle NoAnswer => NONE

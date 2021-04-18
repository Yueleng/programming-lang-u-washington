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

(* 1. Returns a string list with strings that start with uppercase letters *)
fun only_capitals string_list =
  List.filter (fn str => Char.isUpper(String.sub(str, 0))) string_list


(* 2. Returns the longest string in a list, "" if the list is empty *)
fun longest_string1 string_list =
    foldl (fn (str1, str2) => if String.size str1 > String.size str2
			      then str1
			      else str2) "" string_list


(* 3. Returns the longest string in a list. In a tie, the latest longest string is returned *)
fun longest_string2 string_list =
    foldl (fn (str1, str2) => if String.size str1 >= String.size str2
			      then str1
			      else str2) "" string_list


(* 4. Functions similar to longest_string1 and longest_string2 with helper functions, val bindings and partial applications*)
fun longest_string_helper f string_list =
    foldl (fn (str1, str2) => if f (String.size str1, String.size str2)
			      then str1
			      else str2) "" string_list

val longest_string3 = longest_string_helper (fn (str1, str2) => str1 > str2)

val longest_string4 = longest_string_helper (fn (str1, str2) => str1 >= str2)
    

(* 5. Returns the longest string that begins with an uppercase letter *)
val longest_capitalized = longest_string1 o only_capitals


(* 6. Returns the string in reverse order *)
val rev_string = String.implode o rev o String.explode


(* 7. Returns SOME v which is the result of the function call *)
fun first_answer f list =
    case list of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      SOME v => v
		    | NONE => first_answer f xs'


(* 8. Returns NONE if any element returns NONE, else SOME lst1, SOME lst2, ... *)
fun all_answers f list =
    let
	fun aux list acc =
	    case list of
		[] => SOME acc
	      | x::xs' => case f x of
			      NONE => NONE
			    | SOME y => aux xs' (acc @ y)
    in
	aux list []
    end


(* 9. a) Returns number of Wildcard patterns  *)
fun count_wildcards p =
    g (fn () => 1) (fn string => 0) p

(* 9. b) Returns sum of number of Wildcard patterns and string lengths of all its strings *)
fun count_wild_and_variable_lengths p =
    g (fn () => 1) String.size p

(* 9. c) Returns number of times string appears as variable in pattern *)
fun count_some_var (s, p) =
    g (fn () => 0) (fn string => if s = string
				 then 1
				 else 0) p


(* 10. Returns true iff all variables in a pattern are distinct *)
fun check_pat p =
    let
	fun variables x =
	    case x of
		Variable s => [s]
	      | TupleP ps => foldl (fn (p, i) => (variables p) @ i) [] ps
	      | ConstructorP(_, p) => variables p
	      | _ => []
	fun duplicates list =
	    case list of
		[] => false
	      | x::xs' => if List.exists (fn y => y = x) xs'
			  then true
			  else duplicates xs'
	val f = not o duplicates o variables
    in
	f p
    end
	

(* 11. Returns NONE if pattern doesn't match, SOME lst if it does *)
fun match (v, p) =
    case (v, p) of
	(_, Wildcard) => SOME []
      | (var, Variable str) => SOME [(str, var)]
      | (Unit, UnitP) => SOME []
      | (Const i, ConstP pat) => if i = pat
				 then SOME []
				 else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
				 then all_answers match (ListPair.zip(vs, ps))
				 else NONE
      | (Constructor (str1, cv), ConstructorP (str2, cp)) => if str1 = str2
							     then match (cv,cp)
							     else NONE
      | (_, _) => NONE


(* 12. Returns NONE if no pattern in the list matches, SOME lst otherwise *)
fun first_match v p = 
  SOME (first_answer (fn x => match(v, x)) p)
  handle NoAnswer => NONE
			   

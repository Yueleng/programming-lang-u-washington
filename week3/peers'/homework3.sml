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
	    Wildcard => f1()
	  | Variable x =>  f2 x
	  | TupleP ps => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _ => 0
    end
	
fun only_capitals(xs: string list) = List.filter (fn x => Char.isUpper (String.sub (x, 0))) xs

fun longest_string1(xs: string list) =
    foldl (fn (x, longest) => if String.size x > String.size longest then x else longest) "" xs
	       
fun longest_string2(xs: string list) =
    foldl (fn (x, longest) => if String.size x >= String.size longest then x else longest) "" xs
	  
fun longest_string_helper f xs =
    foldl (fn (x, longest) => if f (String.size x, String.size longest) then x else longest) "" xs

val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)
					    
fun longest_capitalized(xs: string list) = (longest_string1 o only_capitals) xs

fun rev_string(x: string) = (String.implode o List.rev o String.explode) x

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | v::vs => case f v of
		     NONE => first_answer f vs
		   | SOME element => element

fun all_answers f xs =
    case xs of
	[] => SOME []
      | v::vs => case f v of
		    NONE => NONE
		  | SOME element => case all_answers f vs of
					NONE => NONE
				      | SOME element' => SOME (element @ element')

						      
val count_wildcards = g (fn () => 1) (fn v => 0)
val count_wild_and_variable_lengths = g (fn () => 1) String.size
fun count_some_var (str, pattern) = g (fn () => 0) (fn v => if v = str then 1 else 0) pattern
				      
fun check_pat pattern =
    let
	fun getValues p =
	    case p of
		Variable x => [x]
	      | TupleP xs => List.foldl (fn (xs', acc) => acc @ getValues xs') [] xs
	      | ConstructorP(_,x) => getValues x
	      | _ => []
	fun getUnique values =
	    case values of
		[] => true
	      | x::xs => if List.exists (fn x' => x=x') xs then false else getUnique xs
    in
	getUnique (getValues pattern)
    end

fun match (value, pattern) =
    case (value, pattern) of
	(_, Wildcard) => SOME []
      | (v, Variable x) => SOME [(x,v)]
      | (Unit,UnitP) => SOME []
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
				 then all_answers (fn (vs',ps') => match (vs',ps')) (ListPair.zip (vs,ps))
				 else NONE
      | (Constructor(s1,v'), ConstructorP(s2,p'))  => if s1 = s2
						      then match (v',p')
						      else NONE
							      
      | _ => NONE

fun first_match v ps =
    SOME (first_answer (fn p => match(v,p)) ps)
    handle NoAnswer => NONE
			   
    
	

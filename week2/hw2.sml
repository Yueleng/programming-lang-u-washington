(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* a *)
fun all_except_option(s: string, slst: string list) = 
    case slst of 
        [] => NONE
        | x::xs' => if same_string(s,x) then SOME xs' else 
        let 
            val res = all_except_option(s, xs')
        in
            case res of 
		NONE => NONE
             | SOME lst => SOME( x :: lst)
        end;
                       
(* b *)
(* get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")*)
(* => ["Fredrick", "Freddie", "F"] *)
fun get_substitutions1(lsts: string list list, substi: string) =
    case lsts of
	[] => []
      | x::xs' =>
	let
	    val xoption = all_except_option(substi, x)
        in
		case xoption of
		    NONE => get_substitutions1(xs', substi)
		| SOME lst => lst @ get_substitutions1(xs', substi)
	end;

(* c *)
(* Tail Recursive *)
fun get_substitutions2(lsts: string list list, substi: string) =
    let
	fun aux(lsts: string list list, substi: string, acc: string list) =
	    case lsts of
		[] => acc
	      | lst::xs' =>
		let
		    val xoption = all_except_option(substi, lst)
		in
		    case xoption of
			NONE => aux(xs', substi, acc)
		     | SOME res  => aux(xs', substi, acc @ res)
		end
    in
	aux(lsts, substi, [])
    end;


(* D *)
fun similar_names(lsts: string list list, {first = x, middle=y, last=z}) =
    let
	val substis = get_substitutions1(lsts, x);
	fun substi_first ({first=x, middle=y,last=z}, names: string list) =
	    case names of
		[] => []
		   | head::neck => {first=head,middle=y,last=z} :: substi_first({first=x,middle=y,last=z}, neck)
    in
	{first=x, middle=y,last=z} :: substi_first({first=x,middle=y,last=z}, substis)
    end;
	
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* a *)
fun card_color(c: card) =
    case c of
	(Clubs,_) => Black
      | (Spades,_) => Black
      | _ => Red;

(* b *)
fun card_value(c:card) =
    case c of
	(_,Jack) => 10
      | (_, Queen) => 10
      | (_, King) => 10
      | (_, Ace) => 11
      | (_, Num n) => n;

(* c *)
fun remove_card(cs: card list, c: card, e) =
    case cs of
	[] => raise e
      | x::xs' => if x = c then xs' else
		  x :: remove_card(xs', c, e);


(* d *)
fun all_same_color(cs: card list) =
    case cs of
	[] => true
      | _::[] => true
      | c1::(c2::cs') =>
	if card_color(c1) = card_color(c2) then all_same_color(c2::cs') else false;
		  
(* e *)
fun sum_cards(cs: card list) =
    let
	fun aux(cs: card list, acc: int) =
	    case cs of
		[] => acc


	      | c1::cs' => aux(cs', card_value(c1) + acc)
	
	
    in
	aux(cs, 0)
    end;
	

(* f *)
fun score(cs: card list, goal: int) =
    let
	val sum = sum_cards cs
        val same_color = all_same_color cs
        val diff = if sum - goal > 0 then 3 *( sum - goal) else goal - sum
    in
	if same_color then diff div 2
        else diff
    end;
	
(* g *)
fun officiate(cs: card list, mvs: move list, goal: int) =
    let
	val held_cards = []
	fun aux(cs: card list, mvs: move list, goal: int, acc: card list) =
	    if sum_cards acc > goal then score(acc, goal) (* if drawing causes the sum of the held cards to exceed the goal, game over *)
	    else
		case mvs of
		    [] => score(acc, goal)
		  | mv::mvs' => case(cs,  mv) of
				    (c1::cs', Draw) => aux(cs', mvs', goal, c1::acc)
				  | ([], Draw) => score(acc, goal)
				  | (cs, Discard c) => aux(cs, mvs', goal, remove_card(held_cards, c, IllegalMove))
    in
	aux(cs, mvs, goal, [])
    end;


	
(* Challenge *)


fun score_challenge(cs: card list, goal: int) =
    let
	fun better(score1: int, score2: int) = if score1 < score2 then score1 else score2
	fun score_iter(remain_cs: card list, acc: card list) =
	    case remain_cs of
		 [] => score(acc, goal)
		 | c::cs' =>
		   case c of
		   (suit, Ace) => better(
				  score_iter(cs', (suit, Num 1)::acc),
				  score_iter(cs', (suit, Num 11)::acc)
				  )
                   | (suit, rank) => score_iter(cs', (suit, rank)::acc)
    in
	score_iter(cs, [])
    end;
	
fun officiate_challenge(cs: card list, mvs: move list, goal: int) =
    let
	val held_cards = []
	fun sum_cards_smart(acc: card list) =
	    case acc of
		[] => 0
	      | c::cs' =>
		case c of
		   (_, Ace) => 1 + sum_cards_smart(cs')
			| rank  => card_value rank + sum_cards_smart(cs')		     
	fun aux(cs: card list, mvs: move list, goal: int, acc: card list) =
	    (* if drawing causes the sum of the held cards to exceed the goal, game over *)
	    if sum_cards_smart acc > goal then score_challenge(acc, goal)
	    else
		case mvs of
		    [] => score_challenge(acc, goal)
		  | mv::mvs' => case(cs,  mv) of
				    (c1::cs', Draw) => aux(cs', mvs', goal, c1::acc)
				  | ([], Draw) => score_challenge(acc, goal)
				  | (cs, Discard c) => aux(cs, mvs', goal, remove_card(held_cards, c, IllegalMove))
    in
	aux(cs, mvs, goal, [])
    end;
    

fun careful_player(cs: card list, goal: int) = (* returns a move list *)
    let
	fun discard_draw(cs: card list, c: card) =
	    let
		fun discard_draw_iter(cs: card list, c: card, acc: card list) =
		    case cs of
			[] => NONE
		         | d :: cs' => if sum_cards cs' + card_value c + sum_cards acc = goal then SOME (Discard d)
				  else discard_draw_iter(cs', c, d::acc)
	    in
		discard_draw_iter(cs, c, [])
	    end
		

		
        fun play(cs: card list, held_cards: card list, moves: move list) =
	    if sum_cards held_cards > goal then moves
            else if sum_cards held_cards + 10 < goal then
		case cs of
		    [] => Draw :: moves
		  | c::cs' => play(cs', c::held_cards, Draw :: moves)
            else if sum_cards held_cards = goal then moves (* score of 0 is reached*)
            else
		case cs of
		    [] => Draw:: moves
		  | c::cs' =>
		    let
			val res = discard_draw(held_cards, c)
		    in
			case res of
			    NONE => moves
			 | SOME (Discard d) => Draw :: (Discard d :: moves)
		    end
	fun rev_r xs =
	    let
		fun aux(xs, acc) =
		    case xs of
			[] => []
		      | x::xs' => aux(xs', x::acc)
	    in
		aux(xs, [])
	    end
		
    in
	rev_r(play(cs, [], []))
    end;
	


(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* Part 1 *)

fun all_except_option (stringToRemove, stringList) =
	case stringList of
		  [] => NONE
	    | head::tail =>
	    	if same_string (stringToRemove, head)
			then SOME tail
			else case all_except_option (stringToRemove, tail) of
				  NONE => NONE
			    | SOME result => SOME (head::result)


fun get_substitutions1 (substitutions, s) =
	case substitutions of
		  [] => []
		| substitutionsHead::substitutionsTail =>
			let val matches = all_except_option (s, substitutionsHead)
			in case matches of
				  NONE => get_substitutions1 (substitutionsTail, s)
				| SOME matches => matches @ get_substitutions1 (substitutionsTail, s)
			end


fun get_substitutions2 (substitutions, s) =
	let 
		fun accumulateSubstitutions (substitutions', accumulator) =
			case substitutions' of
				[] => accumulator
			  | substitutionsHead::substitutionsTail => 
			  		let
			  			val matches = all_except_option (s, substitutionsHead)
			  			val accumulator' = case matches of
			  				NONE => accumulator
			  			  | SOME matches => accumulator @ matches
			  		in accumulateSubstitutions (substitutionsTail, accumulator')
			  		end
	in
		accumulateSubstitutions (substitutions, [])
	end


fun similar_names (substitutions, { first, middle, last }) =
	let
		val matching_substitutions = get_substitutions1 (substitutions, first)

		fun get_names_with_substitutions (substitutions') =
			case substitutions' of
				  [] => []
			    | x::xs => ({ first=x, middle=middle, last=last })::(get_names_with_substitutions(xs))

		val substituted_names = get_names_with_substitutions(matching_substitutions)
	in
		({ first=first, middle=middle, last=last })::substituted_names
	end


(* Part 2 *)

fun card_color (suit, _ : rank) =
	case suit of
		Clubs => Black
	  | Diamonds => Red
	  | Hearts => Red
	  | Spades => Black


fun card_value (_ : suit, rank) =
	case rank of
		Num x => x
	  | Ace => 11
	  | _ => 10


fun remove_card (cs : card list, c : card, e) =
	case cs of
		[] => raise e
	  | hd::tl =>
	  		if hd = c
	  		then tl
	  		else remove_card (tl, c, e)


fun all_same_color (cs : card list) =
	case cs of
		[] => true
	  | c1::[] => true
	  | c1::c2::tl => (card_color c1) = (card_color c2) andalso (all_same_color (c2::tl))


fun sum_cards (cs : card list) =
	let
		fun sum (cs', acc) =
			case cs' of
			    [] => acc
			  | hd::tl => sum (tl, card_value hd + acc)
	in
		sum (cs, 0)
	end





fun score (cs : card list, goal : int) =
	let
		val sum = sum_cards cs
		val preliminary_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
	in
		if all_same_color cs
		then preliminary_score div 2
		else preliminary_score
	end


fun officiate (cards : card list, moves : move list, goal : int) =
	let
		fun make_moves (cards' : card list, moves' : move list, held_cards : card list) =
			case moves' of
				[] => score (held_cards, goal)
			  | (Discard card)::moves_tail => make_moves (cards', moves_tail, remove_card (held_cards, card, IllegalMove))
			  | Draw::moves_tail =>
			  		case cards' of
			  			[] => score (held_cards, goal)
			  		  | cards_head::cards_tail =>
			  		  		let
			  		  			val new_held_cards = cards_head::held_cards
			  		  		in
				  		  		if sum_cards (new_held_cards) > goal
				  		  		then score (new_held_cards, goal)
				  		  		else make_moves (cards_tail, moves_tail, new_held_cards)
			  		  		end
	in
		make_moves (cards, moves, [])
	end


(* Part 3: Challenge problems *)

fun count_aces cs =
	case cs of
		[] => 0
	  | (_, Ace)::tl => 1 + count_aces tl
	  | _::tl => count_aces tl

fun score_challenge (cs : card list, goal : int) =
	let
		fun best_sum_using_aces (sum_of_cards, no_of_aces_left) =
			if sum_of_cards > goal + 2 andalso no_of_aces_left > 0
			then best_sum_using_aces (sum_of_cards - 10, no_of_aces_left - 1)
			else sum_of_cards

		val greatest_sum = sum_cards cs
		val number_of_aces = count_aces cs
		val closest_sum_to_goal = best_sum_using_aces (greatest_sum, number_of_aces) 

		val difference = abs (closest_sum_to_goal - goal)
		val preliminary_score = if closest_sum_to_goal > goal then 3 * difference else difference
	in
		if all_same_color cs
		then preliminary_score div 2
		else preliminary_score
	end


fun officiate_challenge (cards : card list, moves : move list, goal : int) =
	let
		fun make_moves (cards' : card list, moves' : move list, held_cards : card list) =
			case moves' of
				[] => score_challenge (held_cards, goal)
			  | (Discard card)::moves_tail => make_moves (cards', moves_tail, remove_card (held_cards, card, IllegalMove))
			  | Draw::moves_tail =>
			  		case cards' of
			  			[] => score_challenge (held_cards, goal)
			  		  | cards_head::cards_tail =>
			  		  		let
			  		  			val new_held_cards = cards_head::held_cards
			  		  			val no_of_aces = count_aces new_held_cards
			  		  		in
				  		  		if (sum_cards (new_held_cards) - (10 * no_of_aces)) > goal
				  		  		then score_challenge (new_held_cards, goal)
				  		  		else make_moves (cards_tail, moves_tail, new_held_cards)
			  		  		end
	in
		make_moves (cards, moves, [])
	end


fun careful_player (cards : card list, goal : int) =
	let
		fun choose_moves (cards', held_cards) =
			if score (held_cards, goal) = 0
			then []
			else
				case cards' of
					[] =>
						if goal > (sum_cards held_cards) + 10
						then [Draw]
						else []
				  | cards_head'::cards_tail' =>
				  		if goal > (sum_cards held_cards) + 10
				  		then Draw::choose_moves(cards_tail', cards_head'::held_cards)
				  		else
				  			let
				  				fun find_card_to_discard (prev_held_cards, curr_card, rest_held_cards) =
				  					if score (prev_held_cards @ rest_held_cards, goal) = 0
				  					then SOME curr_card
				  					else
				  						case rest_held_cards of
				  							[] => NONE
				  						  | rest_hd::rest_tl =>
				  						  		find_card_to_discard (curr_card::prev_held_cards, rest_hd, rest_tl)
				  			in
				  				case find_card_to_discard ([], cards_head', held_cards) of
				  					NONE => []
				  				  | SOME c => (Discard c)::Draw::choose_moves(cards_tail', cards_head'::held_cards)
				  			end
	in
		choose_moves (cards, [])
	end

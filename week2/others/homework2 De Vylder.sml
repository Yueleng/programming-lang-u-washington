fun same_string(s1: string, s2: string) =
    s1 = s2

fun all_except_option(X: string, Xs: string list) =
    case Xs of
	[] => NONE
     |  head :: tail => if same_string(head,X) then SOME tail					      
				  else case all_except_option(X, tail) of
					   NONE => NONE
					|  SOME tail => SOME (head :: tail)
							     
									 
fun get_substitutions1(Xs: string list list, X: string) =
    case Xs of
	[] => []
      | head :: tail => case all_except_option(X, head) of
			    NONE => get_substitutions1(tail,X)
			 | SOME Xs => Xs @ get_substitutions1(tail,X)

fun get_substitutions2(Xs: string list list, X: string) =
    let fun get_subst(Xs: string list list, Acc: string list) =
		 case Xs of
		     [] => Acc
		   | head :: tail => case all_except_option(X, head) of
					 NONE => get_subst(tail, Acc)
				       | SOME Ys => get_subst(tail, Acc @ Ys)
    in
	get_subst(Xs, [])
    end


fun similar_names(Xs: string list list, name: {first:string,middle:string,last:string}) =
    let
	val {first=f,middle=m,last=l} = name
	val subs = get_substitutions2(Xs,f)
	fun replaceSubstitutions(Acc, Subs: string list) =
	    case Subs of
		[] => Acc
	      | head :: tail  => replaceSubstitutions(Acc @ [{first=head,middle=m,last=l}], tail)
    in
	replaceSubstitutions([name], subs)
    end

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

fun card_color((suit, _):card)=
    case suit of
	Spades => Black
      | Clubs  => Black
      | Diamonds => Red
      | Hears => Red

fun card_value((_,rank):card)=
    case rank of
	Num value => value
      | Ace => 11
      | _ => 10

fun remove_card(cards: card list, card: card, err: exn)=
    case cards of
	[] => raise err
      | head::tail => if head = card
		      then tail
		      else head :: remove_card(tail,card, err)

fun all_same_color(cards: card list) =
    case cards of
	[] => true (* I'm not sure about this *)
       | _::[]  => true
       | head::(second::tail) => card_color(head) = card_color(second)
				 andalso all_same_color(second::tail)
						       
									    
fun sum_cards(cards: card list)=
    let fun sum_cards_acc(cards: card list, sum: int) =
	    case cards of
		[] => sum
	      | head :: tail => sum_cards_acc(tail, card_value(head) + sum)
    in
	sum_cards_acc(cards, 0)
    end
	
fun score(hand: card list, goal: int) =
    let
	val handSum = sum_cards(hand)
	val preliminary = if handSum > goal
			  then 3 * (handSum - goal)
			  else goal - handSum
    in
	case all_same_color (hand) of
	    true => preliminary div 2
	  | false => preliminary
    end
	
fun officiate (cards,plays,goal) =
    let
	fun loop (currCards,cards,plays) =
	    case plays of
		[] => score(currCards,goal)
	      | (Discard card)::tail =>
		loop (remove_card(currCards,card,IllegalMove),cards,tail)
	      | Draw::tail =>
		case cards of
		    [] => score(currCards,goal)
		  | h::rest => if sum_cards (h::currCards) > goal
			       then score(h::currCards,goal)
			       else loop (h::currCards,rest,tail)
    in
	loop ([],cards,plays)
    end
				
				
	  
	      
				 
				      
	    
					    
    
    

			       
						
			    
	
				       
						 

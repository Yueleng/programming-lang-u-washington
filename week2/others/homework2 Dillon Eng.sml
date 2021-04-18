(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(mystr, xs) = 
  let fun aux(mystr, xs) =
    case xs of [] => []
       | x::xs' => if same_string(mystr, x) then aux(mystr, xs')
                   else x::aux(mystr, xs');
   fun is_in(mystr, xs) =
      case xs of [] => false
         | x::xs' => same_string(x, mystr) orelse is_in(mystr, xs');
  in 
    if is_in(mystr, xs) then SOME(aux(mystr, xs)) else NONE
  end

fun get_substitutions1(subs, name) = 
    case subs of [] => []
       | x::xs => case all_except_option(name, x) of 
                       NONE => get_substitutions1(xs, name)
                     | SOME(lst) => lst @ get_substitutions1(xs, name)

fun get_substitutions2(subs, name) =
let fun helper(subs, name, acc) =
  case subs of [] => acc
     | x::xs => case all_except_option(name, x) of 
                     NONE => helper(xs, name, acc)
                   | SOME(lst) => helper(xs, name, acc@lst)
   in
     helper(subs, name, [])
end

fun similar_names(subs, {first=fname, middle=mname, last=lname}) =
  let 
    val sub_list = get_substitutions2(subs, fname)
    fun helper(sub_list, y, z) = 
     case sub_list of [] => []
        |x::xs => {first=x, middle=y, last=z} :: helper(xs, y, z) 
  in
    helper(fname::sub_list, mname, lname)
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(card) =
  case card of 
       (Clubs, _)  => Black
     |(Spades, _)  => Black
     | (_,_) => Red

fun card_value(card) = 
  case card of 
       (_, Num(v)) => v
     | (_, Ace) => 11
     | (_, _) => 10

(* remove the card c from the deck cs. If cs is not in there, throw e *)
fun remove_card(cs, c, e) =
let fun helper(cs, c, is_here) =
case (cs, is_here) of 
     ([], true) => [] 
   | ([], false) => raise e
   |  (x::xs, false) => if x = c then helper(xs, c, true) else x::helper(xs, c,
   is_here)
   |  (x::xs, true) => x::helper(xs, c, is_here)
in
  helper(cs, c, false)
end

fun all_same_color(cs) = 
  case cs of 
       [] => true
     | _::[] => true
     | head::neck::rest => card_color(head) = card_color(neck) andalso
     all_same_color(neck::rest)

fun sum_cards(cs) =
let fun helper(cs, acc) = 
case cs of [] => acc
   | x::xs => helper(xs, card_value(x)+acc)
in
  helper(cs, 0)
end

fun score(cs, goal) = 
let val sum = sum_cards(cs)
val prel_s = if sum > goal then 3 * (sum - goal) else goal - sum
in
  if all_same_color(cs) then prel_s div 2 else prel_s
end

fun officiate(cs, ms, goal) = 
let fun helper(cs, ms, goal, hcs) = 
let val sum = sum_cards(cs) in
  if sum > goal then score(hcs, goal) else 
    case (ms, cs) of 
         ([], _) => score(hcs, goal)
       | (Draw::mvs, []) => score(hcs, goal)
       | (Draw::mvs, x::xs) => helper(xs, mvs, goal, x::hcs)
       | (Discard(c)::mvs, _) => helper(remove_card(cs, c, IllegalMove), mvs, goal,
       hcs)
end
in 
  helper(cs, ms, goal, [])
end

(* if the sum is greater than the goal, then iteratively move through the cards
* that are aces and subtract 10 until it's less than the goal. Then evaluate the
* score to see if that's better or worse than the original. *)
fun score_challenge(cs, goal) = 
let val sum = sum_cards(cs) 
  fun subtractor(cs, overshoot) = 
case cs of 
     [] => overshoot
   | (_, Ace)::xs => if overshoot >= 0 andalso overshoot - 10 > 3 * ~overshoot then
     subtractor(xs, overshoot - 10)
                     else overshoot
   | (_, _)::xs => subtractor(xs, overshoot)
  val prel_s = if sum > goal 
                     then 
                       let val new_overshoot = subtractor(cs, sum - goal) 
                       in
                         if new_overshoot < 0 then ~new_overshoot else 3 *
                         new_overshoot
                       end
               else goal - sum
in
  if all_same_color(cs) then prel_s div 2 else prel_s
end
      


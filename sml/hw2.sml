(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(*string, string list -> option string list*)
(*returns list that does not include the string*)
fun all_except_option(string, list)=
    let
	fun recurssion(string, list, acc, found)=
	    case list of
		[] => if(found)then
			  SOME(acc)
		      else
			  NONE
	      | a::b' => if(same_string(a, string))then
			     recurssion(string, b', acc, true)
			 else
			     recurssion(string, b', a::acc, found)
    in
	recurssion(string, list, [], false)
    end
	
(*string list list, string -> string list*)
(*returns words that have substitutions of string but does not return the word itself*)
fun get_substitutions1(lists, string)=
    case lists of
	[] => []
      | a::b' => case all_except_option(string, a) of
		     NONE => get_substitutions1(b', string)
		   | SOME(substitions) => substitions @ get_substitutions1(b', string)

(*string list list, string -> string list*)
(*same as get_substitutions but with helper function*)
fun get_substitutions2(lists, string)=
    let
	fun get_substitutions_helper(lists, string, acc)=
	    case lists of
		[] => acc	  
	      | a::b' => case all_except_option(string, a) of
			     NONE => get_substitutions_helper(b', string, acc)
			   | SOME(substitions) => get_substitutions_helper(b', string, substitions @ acc)
									 
    in
	get_substitutions_helper(lists, string, [])
    end
	
(* string list list, {first, middle, last} -> {first, last, middle} list*)
(* returns simliar names that are in list*)
fun similar_names(lists, name)=
    let
	val first_name = case name of
			     {first = x, middle = y, last = z} => x
	val middle_name = case name of
			      {first = x, middle = y, last = z} => y
	val last_name = case name of
			     {first = x, middle = y, last = z} => z
								 
	fun get_names(names, name)=
	    case names of
		[] => []
	      | a::b' => case name of
			     {first = x, middle = y, last = z} => {first = a, last = z, middle = y}::get_names(b', name)
    in
	{first = first_name, last = last_name, middle = middle_name}::get_names(get_substitutions1(lists, first_name),name)   
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

(* card -> color *)
(* returns color of the card*)	      
fun card_color(card)=
    case card of
	(Clubs,_)=> Black
      | (Spades,_) => Black
      | (Diamonds,_) => Red
      | (Hears,_) => Red

(* card -> int *)
(* returns value of the card *)	 
fun card_value(card)=
    case card of
	(_,Num(a)) => a
      | (_,Jack) => 10
      | (_,Queen) => 10
      | (_,King) => 10
      | (_,Ace) => 11
		   
(* card list, card, expection -> card list *)
(* returns list with card removed *)
fun remove_card(cards, card_to_remove, e)=
    let
	fun find_and_remove(cards, card_to_remove, e, acc, found)=
	    case cards of
		[] => if(found)then
			  acc
		      else
			  raise e
	      | a::b' => if(a = card_to_remove andalso not found)then
			     find_and_remove(b', card_to_remove, e, acc, true)
			 else
			     find_and_remove(b', card_to_remove, e, a::acc, found)
    in
	find_and_remove(cards, card_to_remove, e, [], false)
    end

(* card list -> bool *)
(* is all cards same color? *)
fun all_same_color(cards)=
    let
	fun same_color(cards, color)=
	    case cards of
		[] => true
	      | a::b' => if(card_color a = color)then
			     same_color(b', color)
			 else
			     false
    in
	case cards of
	    [] => false
	  | a::b' => same_color(b', card_color(a))
    end

(* card list -> int *)
(*returns sum of cards *)	
fun sum_cards(cards)=
    let
	fun sum(cards, acc)=
	    case cards of
		[] => acc
	      | a::b' => sum(b', card_value(a) + acc)
    in
	sum(cards, 0)
    end

(* cards, int -> int *)
(* returns score with these cards and goal *)
fun score(cards, goal)=
    let
	val sum = sum_cards(cards)
	val is_same_color = all_same_color(cards)
	val score_value = if(sum > goal)then
			      (sum - goal) * 3
			  else 
			      (goal - sum)
    in
	if(is_same_color)then
	   score_value div 2  
	else
	    score_value
    end

(* card list, move list, int -> int *)
(* runs the game with spefic cards and moves and returns the score *)
fun officiate(cards, moves, goal)=
    let
	fun game_state(cards, moves, goal, held_cards)=
	    let
		val points = sum_cards(held_cards)
		val top_card = case cards of
				   [] => NONE
				 | a::b' => SOME(a)
		val rest_of_cards = case cards of
					[] => []
				      | a::b' => b'
	    in
		if(points > goal)then
		    held_cards
		else
		    case top_card of
			NONE => held_cards
		      | SOME(card_on_top) => case moves of
						 [] => held_cards
					       | a::b' => case a of
							      Discard(card) => game_state(cards, b', goal, remove_card(held_cards, card, IllegalMove))
							    | Draw =>  game_state(rest_of_cards, b', goal, card_on_top::held_cards)
	    end
		
    in
	score(game_state(cards, moves, goal, []), goal)
    end
	

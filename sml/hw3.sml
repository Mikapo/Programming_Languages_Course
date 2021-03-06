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

(* string list -> string list *)
(* filters non capital strings out *)
fun only_capitals string_list=
    List.filter (fn string => Char.isUpper(String.sub(string, 0))) (string_list)

(* string list -> string *)
(* gets longest string and will pick word closest to begining of the list *)
fun longest_string1 string_list=
    let
	fun compare (first, second)=
	    if(String.size first > String.size second)then
		first
	    else
		second
    in
	List.foldl compare "" string_list
    end

(* string list -> string *)
(* gets longest string and will pick word closest to end of the list *)
fun longest_string2 string_list=
    let
	fun compare (first, second)=
	    if(String.size first >= String.size second)then
		first
	    else
		second
    in
	List.foldl compare "" string_list
    end
	
(* helper for getting longest_string *)
(* (int * int -> bool) -> string list -> string *)
fun longest_string_helper f string_list =
    let
	fun compare (first, second)=
	    if(f(String.size(first), String.size(second)))then
		first
	    else
		second
    in
	List.foldl compare "" string_list
    end

val longest_string3 = longest_string_helper (fn (a,b) => a > b)
val longest_string4 = longest_string_helper (fn (a,b) => a >= b)
val longest_capitalized = longest_string3 o only_capitals
val rev_string = String.implode o List.rev o String.explode

(* (???a -> ???b option) -> ???a list -> ???b *)
(* returns first answer *)	 
fun first_answer f list=
    case list of
	[] => raise NoAnswer
      | x::xs' => case f x of
		     NONE => first_answer f xs'
		   | SOME(a) => a

(* (???a -> ???b list option) -> ???a list -> ???b list option *)
(* returns all answers *)
fun all_answers f list=
    let
	fun iterator(acc, list)=
	    case list of
		[] => SOME(acc)
	      | x::xs' => case f x of
			      NONE => NONE
			    | SOME(a) => iterator(a @ acc, xs')
    in
	iterator([], list)
    end

val count_wildcards = g(fn unit => 1)(fn s => 0)
val count_wild_and_variable_lengths = g(fn unit => 1)(fn s => String.size s)

(* string * pattern -> int*)
(* counts all variables with this name *)
fun count_some_var(string, pattern)=
    g(fn unit => 0)(fn s => if string = s then 1 else 0)(pattern)

fun check_pat pattern =
    let
	fun get_variable_names (patterns, acc)=
	    case patterns of
		[] => acc
	      | x::xs' => case x of
			      Variable(a) => get_variable_names(xs', a::acc)
			    | TupleP(a) => get_variable_names(xs', get_variable_names(a, acc))		     
			    | ConstructorP(a,b) => get_variable_names(xs', get_variable_names([b], acc))	   
			    | _ => get_variable_names(xs', acc)
	fun check_for_dublicates (names, acc)=
	    case names of
		[] => true
	      | x::xs' => if(List.exists(fn string => string = x)(acc))then
			     false
			 else
			     check_for_dublicates(xs', x::acc)
						     
						     
    in
	check_for_dublicates(get_variable_names([pattern], []), [])
    end

(* valu * pattern -> (string * valu) list option *)
fun match (value, pattern)=
    case pattern of
	Variable(s) => SOME([(s, value)])
      | UnitP => (case value of
		     Unit => SOME([])
		   | _ => NONE)
      | ConstP(v1) => (case value of
			  Const(v2) => if(v1 = v2) then
					   SOME([])      
				       else 
					   NONE	       
			| _ => NONE)
      | TupleP(t1) => (case value of
			   Tuple(t2) => if(List.length t1 = List.length t2)then
					 all_answers(match)(ListPair.zip(t2, t1))
				     else
					 NONE
			 | _  => NONE)
      | ConstructorP(name1,patter) => (case value of
					   Constructor(name2, value) => if(name1 = name2)then  
									  match(value, pattern)       
								      else
									  NONE				      
					 | _ => NONE)
      | _ => SOME([])

fun first_match value patterns=
    case patterns of
	[] => NONE
      | x::xs' => case match(value, x) of
		     SOME(a) => SOME(a)
		   | NONE => first_match value xs'
					  

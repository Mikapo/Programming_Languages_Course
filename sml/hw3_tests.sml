val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,1,1,1,1,1]

val test1004 = count_wildcards (ConstructorP("x", (TupleP [Wildcard, Variable "x", Wildcard]))) = 2

val test1007 =  count_wild_and_variable_lengths (ConstructorP("x", (TupleP [Wildcard, Variable "x", Wildcard])))

val test1008 =  count_some_var ("x",(ConstructorP("x", (TupleP [Variable "y", Variable "x", Variable "y"]))))

val test1009 =  check_pat ((ConstructorP("x", (TupleP [Variable "a", Variable "z", Variable "y"]))))

val test12 = first_match Unit [UnitP] = SOME []
					     
			      
					   
			       
						
												      

			 

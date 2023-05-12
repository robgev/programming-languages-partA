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

(*
	1. Write a function only_capitals that takes a string list and returns a string list that has only
	the strings in the argument that start with an uppercase letter. Assume all strings have at least 1
	character. Use List.filter, Char.isUpper, and String.sub to make a 1-2 line solution.
*)
val only_capitals = List.filter (fn i => Char.isUpper(String.sub(i, 0)))

(*
	2. Write a function longest_string1 that takes a string list and returns the longest string in the
	list. If the list is empty, return "". In the case of a tie, return the string closest to the beginning of the
	list. Use foldl, String.size, and no recursion (other than the implementation of foldl is recursive).
*)
val longest_string1 = List.foldl (fn (s, acc) => 
	if String.size s > String.size acc
	then s
	else acc)
	""

(*
	3. Write a function longest_string2 that is exactly like longest_string1 except in the case of ties
	it returns the string closest to the end of the list. Your solution should be almost an exact copy of
	longest_string1. Still use foldl and String.size.
*)
val longest_string2 = List.foldl (fn (s, acc) => 
	if String.size s >= String.size acc
	then s
	else acc)
	""

(*
	4. Write functions longest_string_helper, longest_string3, and longest_string4 such that:
	• longest_string3 has the same behavior as longest_string1 and longest_string4 has the
	same behavior as longest_string2.
	• longest_string_helper has type (int * int -> bool) -> string list -> string
	(notice the currying). This function will look a lot like longest_string1 and longest_string2
	but is more general because it takes a function as an argument.
	• If longest_string_helper is passed a function that behaves like > (so it returns true exactly
	when its first argument is stricly greater than its second), then the function returned has the same
	behavior as longest_string1.
	• longest_string3 and longest_string4 are defined with val-bindings and partial applications
	of longest_string_helper.
*)
fun longest_string_helper comparator = List.foldl (fn (s, acc) => 
	if comparator (String.size s, String.size acc)
	then s
	else acc) 
	""
(*
	Since operators like > and >= are already functions 
	of type (int * int) -> bool, the more elegant way
	would be to retrieve them immediately like below:
	val longest_string3 = longest_string_helper op>
	val longest_string3 = longest_string_helper op>=
*)
val longest_string3 = longest_string_helper (fn (a, b) => a > b)
val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

(*
	5. Write a function longest_capitalized that takes a string list and returns the longest string in
	the list that begins with an uppercase letter, or "" if there are no such strings. Assume all strings
	have at least 1 character. Use a val-binding and the ML library’s o operator for composing functions.
	Resolve ties like in problem 2.
*)
val longest_capitalized = longest_string1 o only_capitals

(*
	6. Write a function rev_string that takes a string and returns the string that is the same characters in
	reverse order. Use ML’s o operator, the library function rev for reversing lists, and two library functions
	in the String module. (Browse the module documentation to find the most useful functions.)
*)
val rev_string = implode o rev o explode

(*
	7. Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b (notice the 2 arguments 
	are curried). The first argument should be applied to elements of the second argument in order
	until the first time it returns SOME v for some v and then v is the result of the call to first_answer.
	If the first argument returns NONE for all list elements, then first_answer should raise the exception
	NoAnswer.
*)
fun first_answer lookup_func list =
	case list of 
		[] => raise NoAnswer
	  | hd::tl => case lookup_func hd of
	  				NONE => first_answer lookup_func tl
				  | SOME v => v
(*
	8. Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option
	(notice the 2 arguments are curried). The first argument should be applied to elements of the second
	argument. If it returns NONE for any element, then the result for all_answers is NONE. Else the
	calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of
	all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together (order doesn’t matter).
*)
fun all_answers lookup_func list = 
	let 
		fun all_answers_helper lookup_func list acc = 
			case list of
				[] => SOME acc
			  | hd::tl => case lookup_func hd of
							  NONE => NONE
						    | SOME l => all_answers_helper lookup_func tl (acc @ l)
	in
		all_answers_helper lookup_func list []
	end

(*
	9. (This problem uses the pattern datatype but is not really about pattern-matching.) A function g has
	been provided to you.
	(a) Use g to define a function count_wildcards that takes a pattern and returns how many Wildcard
	patterns it contains.
	(b) Use g to define a function count_wild_and_variable_lengths that takes a pattern and returns
	the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables
	in the variable patterns it contains. (Use String.size. We care only about variable names; the
	constructor names are not relevant.)
	(c) Use g to define a function count_some_var that takes a string and a pattern (as a pair) and
	returns the number of times the string appears as a variable in the pattern. We care only about
	variable names; the constructor names are not relevant.
*)

val count_wildcards = g (fn () => 1) (fn _ => 0)
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size(x))
fun count_some_var (str, p) = g (fn () => 0) (fn x => if x = str then 1 else 0) p

(*
	10. Write a function check_pat that takes a pattern and returns true if and only if all the variables
	appearing in the pattern are distinct from each other (i.e., use different strings). The constructor
	names are not relevant.
*)
fun check_pat p = 
	let 
		fun all_vars p = 
			case p of
			  Variable x        => [x]
			| TupleP ps         => List.foldl (fn (p, acc) => (all_vars p) @ acc) [] ps
			| ConstructorP(_,p) => all_vars p
			| _                 => []
		fun has_no_repeats strings =
			case strings of 
				[] => true
			  | hd::tl => (not (List.exists (fn x => x = hd) tl)) andalso has_no_repeats tl
	in
		has_no_repeats (all_vars p)
	end

(*
	11. Write a function match that takes a valu * pattern and returns a (string * valu) list option,
	namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does.
	Note that if the value matches but the pattern has no patterns of the form Variable s, then the result
	is SOME [].
*)
fun match (value, pattern) = 
	case (value, pattern) of
		(_, Wildcard) => SOME []
	  | (_, Variable s) => SOME [(s, value)]
	  | (Unit, UnitP) => SOME []
	  | (Const v, ConstP s) => if s = v then SOME [] else NONE
	  | (Tuple v, TupleP p) => if length v = length p 
	  						   then all_answers match (ListPair.zip(v, p))
							   else NONE
	  | (Constructor (s1, v), ConstructorP (s2, p)) =>  if s1 = s2
														then match (v, p)
														else NONE
	  | _ => NONE
(*
	12. Write a function first_match that takes a value and a list of patterns and returns a
	(string * valu) list option, namely NONE if no pattern in the list matches or SOME lst where
	lst is the list of bindings for the first pattern in the list that matches. Use first_answer and a
	handle-expression. Hints: Sample solution is 3 lines
*)
fun first_match v ps = SOME (first_answer (fn p => match(v, p)) ps)
						handle NoAnswer => NONE
(*
	(Challenge Problem) Write a function typecheck_patterns that “type-checks” a pattern list.
	typecheck_patterns should have type ((string * string * typ) list) * (pattern list) -> typ option.
	The first argument contains elements that look like ("foo","bar",IntT), which means constructor foo
	makes a value of type Datatype "bar" given a value of type IntT. Assume list elements all have different
	first fields (the constructor name), but there are probably elements with the same second field (the datatype
	name). Under the assumptions this list provides, you “type-check” the pattern list to see if there exists
	some typ (call it t) that all the patterns in the list can have. If so, return SOME t, else return NONE.

	You must return the “most lenient” type that all the patterns can have. For example, given patterns
	TupleP[Variable("x"),Variable("y")] and TupleP[Wildcard,Wildcard], return TupleT[Anything,Anything]
	even though they could both have type TupleT[IntT,IntT]. As another example, if the only patterns
	are TupleP[Wildcard,Wildcard] and TupleP[Wildcard,TupleP[Wildcard,Wildcard]], you must return
	TupleT[Anything,TupleT[Anything,Anything]].
*)
datatype typ = Anything (* any type of value is okay *)
| UnitT (* type for Unit *)
| IntT (* type for integers *)
| TupleT of typ list (* tuple types *)
| Datatype of string (* some named datatype *)

fun typecheck_single_pattern constructor_types p = 
	let 
		fun is_lenient (inferred, input) = 
			case (inferred, input) of
				(Anything, _) => true
			  | (_, Anything) => true
			  |	(TupleT t, TupleT nt) => List.all is_lenient (ListPair.zip(t, nt))
			  |	(IntT, IntT) => true
			  | (UnitT, UnitT) => true
			  | (Datatype t, Datatype nt) => t = nt
			  |	_ => false

		fun find_constructor constructor_types cons pattern = 
			case constructor_types of
				[] => raise NoAnswer
			| (c, dt, t)::tl => if c = cons andalso is_lenient ((typecheck_single_pattern constructor_types pattern), t)
								then Datatype dt
								else find_constructor tl cons pattern
	in 
		case p of
			Wildcard => Anything
			| Variable s => Anything
			| UnitP => UnitT
			| ConstP _ => IntT
			| TupleP p => TupleT (List.map (fn x => typecheck_single_pattern constructor_types x) p)
			| ConstructorP (s, p) => find_constructor constructor_types s p
	end

fun infer_type (known_type, newTypeInfo) = 
	case (known_type, newTypeInfo) of
		  (Anything, nt) => nt
		| (t, Anything) => t
		| (UnitT, UnitT) => UnitT
		| (IntT, IntT) => IntT
		| (TupleT t, TupleT nt) => TupleT (List.map infer_type (ListPair.zip(t, nt)))
		| (Datatype t, Datatype nt) => if t = nt then Datatype t else raise NoAnswer
		| _ => raise NoAnswer

fun typecheck_patterns (constructor_types, patterns) =
	let 
		fun typecheck_patterns_aux constructor_types patterns answer =
			case patterns of 
				[] => answer
			  | pattern::tl => typecheck_patterns_aux constructor_types tl (infer_type ((typecheck_single_pattern constructor_types pattern), answer))
	in
		SOME (typecheck_patterns_aux constructor_types patterns Anything)
		handle NoAnswer => NONE
	end
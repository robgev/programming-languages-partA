(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*
    1. This problem involves using first-name substitutions to come up with alternate names. For example,
    Fredrick William Smith could also be Fred William Smith or Freddie William Smith. Only part (d) is
    specifically about this, but the other problems are helpful.
*)

(*
    (a) Write a function all_except_option, which takes a string and a string list. Return NONE if the
    string is not in the list, else return SOME lst where lst is identical to the argument list except the string
    is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
    to compare strings. Sample solution is around 8 lines.
*)
fun all_except_option (string, list) = 
        case list of
            [] => NONE 
          | hd::tl => 
                case all_except_option(string, tl) of
                    NONE => if same_string(hd, string) 
                            then SOME tl 
                            else NONE 
                  | SOME l => SOME (hd::l)

(*
    (b) Write a function get_substitutions1, which takes a string list list (a list of list of strings, the
    substitutions) and a string s and returns a string list. The result has all the strings that are in
    some list in substitutions that also has s, but s itself should not be in the result. 
    Assume each list in substitutions has no repeats. The result will have repeats if s and another string are
    both in more than one list in substitutions.
*)
fun get_substitutions1 (substitutions, string) = 
    case substitutions of 
        [] => [] 
      | hd::tl => 
            let val substitutions_in_tail = get_substitutions1(tl, string)
            in 
                case all_except_option(string, hd) of
                    NONE => substitutions_in_tail
                  | SOME list => substitutions_in_tail @ list
            end

(*
    (c) Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
    local helper function.
*)
fun get_substitutions2 (substitutions, string) = 
    let fun get_substitutions2_tl (substitutions, string, acc) =
        case substitutions of
            [] => acc
          | hd::tl => 
                case all_except_option(string, hd) of
                    NONE => get_substitutions2_tl(tl, string, acc)
                  | SOME list => get_substitutions2_tl(tl, string, acc @ list)
    in
        get_substitutions2_tl(substitutions, string, [])
    end

(*
    Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and
    (c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
    names (type {first:string,middle:string,last:string} list). The result is all the full names you
    can produce by substituting for the first name (and only the first name) using substitutions and parts (b)
    or (c). The answer should begin with the original name (then have 0 or more other names). 
    Do not eliminate duplicates from the answer.
*)
fun similar_names (substitutions, full_name) = 
    let 
        val {first=first, middle=middle, last=last} = full_name
        val similar_first_names = get_substitutions2(substitutions, first)
        fun append_names (names_to_append) =
            case names_to_append of
                [] => []
              | hd::tl => ({first=hd, middle=middle, last=last})::append_names(tl)
    in
        full_name::append_names(similar_first_names)
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

(*
    2. This problem involves a solitaire card game invented just for this question. You will write a program that
    tracks the progress of a game; writing a game player is a challenge problem. You can do parts (a)–(e) before
    understanding the game if you wish.

    A game is played with a card-list and a goal. The player has a list of held-cards, initially empty. The player
    makes a move by either drawing, which means removing the first card in the card-list from the card-list and
    adding it to the held-cards, or discarding, which means choosing one of the held-cards to remove. The game
    ends either when the player chooses to make no more moves or when the sum of the values of the held-cards
    is greater than the goal.

    The objective is to end the game with a low score (0 is best). Scoring works as follows: Let sum be the sum
    of the values of the held-cards. If sum is greater than goal, the preliminary score is three times (sum−goal),
    else the preliminary score is (goal − sum). The score is the preliminary score unless all the held-cards are
    the same color, in which case the score is the preliminary score divided by 2 (and rounded down as usual
    with integer division; use ML’s div operator).
*)

(*
    (a) Write a function card_color, which takes a card and returns its color (spades and clubs are black,
    diamonds and hearts are red). Note: One case-expression is enough.
*)
fun card_color (suit, _) =
    case suit of 
        Clubs => Black
      | Spades => Black
      | _ => Red

(*
    (b) Write a function card_value, which takes a card and returns its value (numbered cards have their
    number as the value, aces are 11, everything else is 10). Note: One case-expression is enough.
*)
fun card_value (_, rank) =
    case rank of 
        Num n => n
      | Ace => 11
      | _ => 10

(*
    (c) Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
    list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
    If c is not in the list, raise the exception e. You can compare cards with =.
*)
fun remove_card (cs, c, e) =
    case cs of 
        [] => raise e
      | current_card::cs' => 
        if current_card = c 
        then cs' 
        else current_card::remove_card(cs', c, e)

(*
    (d) Write a function all_same_color, which takes a list of cards and returns true if all the cards in the
    list are the same color. 
*)
fun all_same_color cs =
    case cs of
        [] => true
      | _::[] => true
      | c1::(c2::tl) => (card_color c1 = card_color c2) andalso all_same_color (c2::tl)

(*
    (e) Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally
    defined helper function that is tail recursive. (Take “calls use a constant amount of stack space” as a
    requirement for this problem.)
*)
fun sum_cards cs =
    let 
        fun sum_cards_tailed (cs, acc) =
            case cs of
                [] => acc
              | c::cs' => sum_cards_tailed(cs', card_value c + acc)
    in
        sum_cards_tailed(cs, 0)
    end

(*
    (f) Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes
    the score as described above.
*)
fun score (cs, goal) = 
    let 
        val sum = sum_cards cs
        val preliminary_score = 
            if sum > goal 
            then 3 * (sum - goal) 
            else (goal - sum)
    in
        if all_same_color cs 
        then
            preliminary_score div 2
        else 
            preliminary_score
    end

(* 
    (g) Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list
        (what the player “does” at each point), and an int (the goal) and returns the score at the end of the
        game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive
        helper function that takes several arguments that together represent the current state of the game. As
        described above:
            • The game starts with the held-cards being the empty list.
            • The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
            • If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
            not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
            exception.
            • If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes
            the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues
            with a larger held-cards and a smaller card-list.
        Sample solution for (g) is under 20 lines.
*)
fun officiate (cs, mvs, goal) =
    let 
        fun run_game (cs, mvs, held) = 
            case (cs, mvs) of
                (_, []) => score(held, goal)
              | ([], Draw::mvs') => score(held, goal)
              | (c::cs', Draw::mvs') => 
                    if sum_cards (c::held) > goal
                    then score (c::held, goal)
                    else  run_game(cs', mvs', c::held)
              | (cs, Discard c::mvs') => run_game(cs, mvs', remove_card(held, c, IllegalMove))
    in
        run_game(cs, mvs, [])
    end

(* Another version of officiate
    fun officiate (cs, mvs, goal) =
        let 
            fun draw_card (cs, mvs, held) = 
                case cs of
                    [] => (cs, [], held)
                | c::cs' => 
                    if sum_cards (c::held) > goal
                    then (cs, [], (c::held))
                    else (cs', mvs, (c::held))
            fun make_move (cs, mv, mvs', held) =
                case mv of 
                    Draw => draw_card(cs, mvs', held)
                | Discard c => (cs, mvs', remove_card(held, c, IllegalMove))
            fun run_game (cs, mvs, held) = 
                case mvs of 
                    [] => score(held, goal)
                | mv::mvs' => run_game(make_move(cs, mv, mvs', held))
        in
            run_game(cs, mvs, [])
        end
*)

(* Challenge Problems *)
(*
    (a) Write score_challenge and officiate_challenge to be like their non-challenge counterparts except
    each ace can have a value of 1 or 11 and score_challenge should always return the least (i.e., best)
    possible score. (Note the game-ends-if-sum-exceeds-goal rule should apply only
*)

(*
    As the score is depending on how close goal and sum are, we need to minimize the absolute
    value of their difference. Hence choose to add 1 or 11 depending on how closwe we can get to the goal
    (no matter if we exceed or not)
*)
fun sum_cards_challenge (cs, goal) =
    let 
        fun abs (a, b) = if a > b then a - b else b - a
        fun sum_cards_challenge_tailed (cs, goal, acc) = 
            case cs of 
                [] => acc
                | (suit, Ace)::cs' =>  
                        if abs(acc + 1, goal) < abs(acc + 11, goal)
                        then
                            sum_cards_challenge_tailed(cs', goal, acc + 1)
                        else
                            sum_cards_challenge_tailed(cs', goal, acc + 11)
                |  c::cs' => sum_cards_challenge_tailed(cs', goal, acc + card_value c)
    in
        sum_cards_challenge_tailed(cs, goal, 0)
    end

(*
    Would be neat here to pass a function by an argument to score instead ;)
*)
fun score_challenge (cs, goal) = 
    let 
        val sum = sum_cards_challenge(cs, goal)
        val preliminary_score =
            if sum > goal 
            then 3 * (sum - goal) 
            else (goal - sum)
    in
        if all_same_color cs 
        then
            preliminary_score div 2
        else 
            preliminary_score
    end

fun officiate_challenge (cs, mvs, goal) =
    let 
        fun run_game (cs, mvs, held) = 
            case (cs, mvs) of
                (_, []) => score_challenge(held, goal)
              | ([], Draw::mvs') => score_challenge(held, goal)
              | (c::cs', Draw::mvs') => 
                    if sum_cards_challenge ((c::held), goal) > goal
                    then score_challenge (c::held, goal)
                    else  run_game(cs', mvs', c::held)
              | (cs, Discard c::mvs') => run_game(cs, mvs', remove_card(held, c, IllegalMove))
    in
        run_game(cs, mvs, [])
    end

(*
    (b) Write careful_player, which takes a card-list and a goal and returns a move-list such that calling
    officiate with the card-list, the goal, and the move-list has this behavior:
    • The value of the held cards never exceeds the goal.
    • A card is drawn whenever the goal is more than 10 greater than the value of the held cards. As a
    detail, you should (attempt to) draw, even if no cards remain in the card-list.
    • If a score of 0 is reached, there must be no more moves.
    • If it is possible to reach a score of 0 by discarding a card followed by drawing a card, then this
    must be done. Note careful_player will have to look ahead to the next card, which in many card
    games is considered “cheating.” Also note that the previous requirement takes precedence: There
    must be no more moves after a score of 0 is reached even if there is another way to get back to 0.
    Notes:
    • There may be more than one result that meets the requirements above. The autograder should
    work for any correct strategy — it checks that the result meets the requirements.
    • This problem is not a continuation of problem 3(a). In this problem, all aces have a value of 11.
*)

datatype sgn = P | N | Z

fun careful_player (cs, goal) = 
    let
        fun sign z = if z = 0 then Z else if z > 0 then P else N
        fun find_card_to_zero (c, held) =
            case held of
                [] => NONE
              | hd::tl => 
                if sum_cards held - card_value hd + card_value c = goal
                then SOME hd
                else find_card_to_zero(c, tl)
        fun run_game (cs, mvs, held) = 
            case (cs, sign (sum_cards held - goal), sum_cards held - goal > 10, held) of 
                ([], _, false, _) => mvs 
              | ([], _, true, _) => run_game([], Draw::mvs, held)
              | (_, Z, _, _) => mvs
              | (c::cs', P, true, _) => run_game(cs', Draw::mvs, c::held)
              | (c::cs', P, false, hc::hcs) => if (sum_cards held + card_value c > goal)
                                           then 
                                            case find_card_to_zero(c, held) of
                                                NONE => run_game(cs', Discard hc::mvs, hcs)
                                              | SOME found => Discard found::Draw::mvs
                                           else run_game(cs', Draw::mvs, c::hc::hcs)
              | _ => mvs
    in
        run_game(cs, [], [])
    end
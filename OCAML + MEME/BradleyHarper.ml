(* CSC 3710 starter code for exploring lists, sorting, and DFA's.
   Author: Martha Kosa   
   Date: 11.19.2020 *)

(* TODO: Please change XXX to your name, and YYY and ZZZ if needed. *)   
(* Modifications by Bradley Harper (and YYY and ZZZ if you have a partner or partners. *)

(****************************)
(* list utility functions *)

(* converts from String to list of chars *)
(* from https://caml.inria.fr/mantis/view.php?id=5367 *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) [];;

(* TODO: Call explode with your name as input.  If you are working in a team, call
   explode once for each team member. *)

explode "BradleyHarper";;

(* converts from list of chars to String *)
(* single char conversion from https://stackoverflow.com/questions/20441263/convert-char-to-string-in-ocaml/20463186 *)
let rec to_one_string char_list =
  match char_list with
    | [] -> ""
    | h::t -> (String.make 1 h)^(to_one_string t);; (* ^ does string concatenation *)

(* TODO: Call to_one_string with the result(s) of your call(s) to explode. *)

to_one_string (explode "BradleyHarper");;

let length xs =
  let rec length_tr xs acc =
    match xs with
      | [] -> acc
      | _::t -> length_tr t (1 + acc)
  in length_tr xs 0;;

(* TODO: Complete (and uncomment) the following recursive function. *)
let rec are_lists_equal xs ys =
  match (xs,ys) with
    | ([],[]) -> true (* What are the smallest possible xs and ys? *)
    | (x::tx, y::ty) -> 
        if x = y then are_lists_equal tx ty
        else false 
    | _ -> false;;

(* TODO: Write 8 test cases to test your are_lists_equal function thoroughly. *)

are_lists_equal [1;2;5;4;3] [1;2;6;4;3];;

are_lists_equal [1;2;5;4;3] [1;2;5;4;3];;

are_lists_equal [] [];;

are_lists_equal [1;2;5;4;3] [1;2;3;4;5];;

are_lists_equal [1] [1;2;3;4;5];;

are_lists_equal [1;2] [1;2];;

are_lists_equal [1;2;6;4;3] [1;2;5;4;3];;

are_lists_equal [5;5;5;5;5;5;5;5] [5;5;5;5;5;5;5;5];;

let rec contains xs item =
  match xs with
    | [] -> false
    | h::t -> if h = item then true else contains t item;;

contains [1;2;5;4;3] 5;;

contains [1;2;6;4;3] 5;;


(* tail-recursive list reversal *)
let reverse xs =
  let rec reverse_tail xs rxs =
    match xs with
      | [] -> rxs
      | h::t -> reverse_tail t (h::rxs)
  in reverse_tail xs [];;

(* TODO: Call reverse with the result(s) of your call(s) to explode. *)

reverse (explode "BradleyHarper");;

(* TODO: Complete (& uncomment) the below recursive function to determine whether
   a list is a prefix of another list. See the notes from September 25 for hints. *)
let rec prefix xs ys = 
  match (xs, ys) with
    | ([], _) -> true
    | (_, []) -> false
    | (x::xss, y::yss) ->
        if x = y then prefix xss yss else false;;

(* TODO: Write 8 test cases to test your prefix function thoroughly. *)

prefix [] [];;

prefix [] [1;2;3;4;5];;

prefix [1] [1;2;3;4;5];;

prefix [1;2] [1;2;3;4;5];;

prefix [1;2;3;4;5] [];;

prefix [1;2;3;4;5] [1];;

prefix [4;5] [7;8;9;2;4];;

prefix [2] [4;5];;


(* TODO: Complete (and uncomment) the below function to determine whether
   a list is a suffix of another list. See the notes from September 28 for hints. *)
let suffix xs ys = prefix (reverse xs) (reverse ys);;

(* TODO: Write 8 test cases to test your suffix function thoroughly. *)

suffix [] [];;

suffix [] [1;2;3;4;5];;

suffix [1] [1;2;3;4;5];;

suffix [1;2] [1;2;3;4;5];;

suffix [1;2;3;4;5] [];;

suffix [1;2;3;4;5] [1];;

suffix [4;5] [7;8;9;2;4];;

suffix [2] [4;5];;

let rec sublist xs ys ks =
  match (xs, ys, ks) with
    | ([], _, _) -> true
    | (_, [], _) -> false
    | (a::xss, b::yss, kss) ->
        if a != b then sublist (kss@(a::xss)) yss []
        else sublist xss yss (kss@[a]);;

let sublist_public xs ys = sublist xs ys [];;

(* TODO: Complete (and uncomment) the below function to determine whether
   a string is a substring of another string. Hint: The function that you need
   to call is somewhere before line 81. *)
let substring x y = sublist_public (reverse x) (reverse y);;

substring [3] [1;2;3;4;5];;

substring [] [];;

substring [] [1;2;3;4;5];;

substring [3;4] [1;2;3;4;5];;

substring [1;2] [1;2;3;4;5];;

substring [1;2;3;4;5] [];;

substring [1;2;3;4;5] [1;2;3;4;5;7;8;9];;

substring [4;5] [7;8;9;2;4];;

(* TODO: Write 8 test cases to test your substring function thoroughly. *)

(* TODO: Write a comment to explain what map does. Run the calls to map below for
   hints. *)



let rec map f xs =
  match xs with
    | [] -> []
    | h::t -> (f h)::(map f t);;

map (fun x -> x * x) [1;2;3;4];;

map sqrt [1.;2.;3.;4.;5.];;


(*creates a "mapping", connects two lists for example usernames to their passwords*)

(* TODO: Write a comment to explain the purpose of the OCaml keyword "fun".
   I think that the keyword "fun" is fun.  Do you? :) *)
map (fun x -> ceil (sqrt x)) [1.;2.;3.;4.;5.];;

(*The keyword fun is needed with anonymous functions.*)

let floor_sqrt x = floor (sqrt x);;

map floor_sqrt [1.;2.;3.;4.;5.];;

(* TODO: What does OCaml compute for the square root of a negative number? *)
map sqrt [1.0; 2.0; 2.1; 2.2; 3.3; 3.4; 3.5; 3.6; -5.7; 0.; 2.; 4.; -6.];; 

(* nan *)

let double_with_space str = str^" "^str;;

map double_with_space ["Martha"; "Kosa"];;

let mega_dws = map double_with_space;;

map mega_dws [["Martha"; "Kosa"]; ["Mark"; "Boshart"]];;

(* TODO: Write a comment to explain what filter does. Run the calls to filter
   below for hints. *)
let rec filter f xs =
  match xs with
    | [] -> []
    | h::t ->
        if (f h) then h::(filter f t) else (filter f t);;

filter (fun x -> x mod 2 = 0) [1;2;2;2;3;3;3;3;5;-5;0;2;4;-6];;

filter (fun x -> (sqrt x) < 2.) [1.0;2.0;2.1;2.2;3.3;3.4;3.5;3.6;-5.7;0.;2.;4.;-6.];;

let length_threshold t xs = (length xs) > t;;

filter (length_threshold 2) [[1;2]; [3]; [4;5;6]];;

filter (length_threshold 3) [[1;2]; [3]; [4;5;6]];;

filter (length_threshold 3) [["Martha"; "April"; "Mark"; "Ambareen"]; []];;

let is_length_at_least x str = String.length str >= x;;

let is_length_less_than x = fun y -> not (is_length_at_least x y);;

filter (is_length_at_least 5) ["tired"; "lazy"; "sad"; "happy"; "funny"; "real"; "fake"];;

filter (is_length_less_than 5) ["tired"; "lazy"; "sad"; "happy"; "funny"; "real"; "fake"];;

filter (fun x -> x * x >= 100) [9;8;10;7;22;33;44;6;-1;-10];;

(*Takes out certain list items that do not meet our condition*)

(* TODO: Write a comment to explain what my_fold_left does. Run the calls to
   my_fold_left below for hints. *)
let rec my_fold_left f id xs =
  match xs with
    | [] -> id
    | h::t -> my_fold_left f (f id h) t;;

my_fold_left ( - ) 0 [1; 2; 3];;

reverse [1;2;3];;

my_fold_left ( - ) 0 (reverse [1; 2; 3]);;

my_fold_left ( ^ ) "" (map double_with_space ["Martha"; "Kosa"]);;

(*applying a function and accumulating a result in a particular manner, with the first part of each variable being put on the left side performing operand before it*)

let join2ws s1 s2 = 
  if s1 = "" then s2 else s1^" "^s2;; 

my_fold_left join2ws "" (map double_with_space ["Martha"; "Kosa"]);;

my_fold_left ( *. ) 1. [1.; 1.5; 2.; 2.5];;

(* TODO: Write a test case for flatten with your full name and your partner's name.
   Write a comment to explain what flatten does. Hint: Find out what the @
   operator in OCaml does. *)
let flatten xs = my_fold_left (@) [] xs;;

flatten  [["BradleyHarper"];["NoPartner"];[":("]];;

let make_ordered_pair a b = (a,b);;

let make_CSC_ordered_pair = make_ordered_pair "CSC";;

(* TODO: Complete (and uncomment) the following function call to produce ordered
   pairs corresponding to your CSC courses this semester.  Make a call for
   each person on your team. *)		 
map make_CSC_ordered_pair ([3710;3410;4200;3040]);;

let rec cartesian_product xs ys =
  match xs with
    | [] -> []
    | h::t -> (map (make_ordered_pair h) ys)@(cartesian_product t ys);;

(* TODO: Write a test case for cartesian_product with your full name and your
   partner's full name in the first list and a list of your choice having at
   least 3 items. *)

cartesian_product (["BradleyHarper";"NoPartner"]) (["videogames";"icecubetray";"towel"]);;

let cartesian_product_better xs ys =
  let rec cartesian_product_tr xs ys acc =
    match xs with
      | [] -> acc
      | h::t -> cartesian_product_tr t ys (acc@(map (make_ordered_pair h) ys))
  in cartesian_product_tr xs ys [];;

(* TODO: Call cartesian_product_better with the same test case for
   cartesian_product. *)

cartesian_product_better (["BradleyHarper";"NoPartner"]) (["videogames";"icecubetray";"towel"]);;

(* TODO: Write a comment to tell what sorting algorithm is implemented by the
   neg, partition, and sort functions below. *)
let neg f x = not (f x);;

let partition f l = (filter f l, filter (neg f) l);;

(*quicksort*)

let rec sort xs gt =
  match xs with 
    | [] -> []
    | (pivot::t) -> let (l, r) = partition (gt pivot) t in
          (sort l gt)@(pivot::(sort r gt));;

(* TODO: Complete (and uncomment) the below recursive function that removes
   duplicate items from a list. *)
let rec remove_dupes xs =
  match xs with
    | [] -> []
    | [h] -> [h]
    | x1::x2::t ->
        let after = remove_dupes (x2::t) in
          if x1 = x2 then after else x1::after;;
		  
(* TODO: Write 3 test cases to test your remove_dupes function thoroughly. *)

remove_dupes [1;1;1;1;2;3;3;3];;

remove_dupes [1;1;1;1;1;1;1;1;1;1;2;2;2;2;2;2;2;2;2;2;3;4];;

remove_dupes ["BradleyHarper";"BradleyHarper";"BradleyHarper";"BradleyHarper";"BradleyHarper"];;
		  
(* ********************************************************* *)
(* DFA and RG code begins *)
let ab_alphabet = ['a'; 'b'];;

(* assume states are numbered 0, 1, 2, etc. *)

let rec generate_states n =
  if n <= 1 then [0] else (generate_states (n-1))@[n-1];;

(* Note that the set of transitions is a finite subset of Q x Sigma x Q.  The
   form is (i, 'x', j), indicating that delta(q_i, x) = q_j. *)

let ab_transitions = [(0, 'a', 1); (0, 'b', 2);
                      (1, 'a', 0); (1, 'b', 3);
                      (2, 'a', 3); (2, 'b', 0);
                      (3, 'a', 2); (3, 'b', 1)];;

length ab_transitions;;

generate_states 4;;

let find_alphabet transitions =
  let rec collect_all_symbols transitions acc =
    match transitions with
      | [] -> acc
      | (_,symbol,_)::tail -> collect_all_symbols tail acc@[symbol]
  in collect_all_symbols transitions [];;

find_alphabet ab_transitions;;

let find_states_used transitions =
  let rec find_all_states transitions acc =
    match transitions with
      | [] -> acc
      | (state,_,_)::tail -> find_all_states tail acc@[state]
  in find_all_states transitions [];;

find_states_used ab_transitions;;

sort (find_alphabet ab_transitions) (>);;

sort (find_states_used ab_transitions) (>);;

(* TODO: Uncomment the following calls to remove_dupes. *)

remove_dupes (sort (find_alphabet ab_transitions) (>));;

remove_dupes (sort (find_states_used ab_transitions) (>));;

sort (generate_states 4) (>);;

(* TODO: Uncomment the following function and its calls. *) 
let are_states_valid states =
  are_lists_equal (sort states (>)) (generate_states (length states));;

are_states_valid [3;2;1;0];;

are_states_valid [5;4;3;2];;

(* TODO: Complete (and uncomment) the following two functions and associated
   calls. *)
let get_transition_inputs (q_i, x, _) = (q_i, x);;

get_transition_inputs (0, 'a', 1);;

let get_transition_output (_, _, q_j) = q_j;;

get_transition_output (0, 'a', 1);;

let q_sigma_gt (q_i, x) (q_j, y) =
  (q_i > q_j) || (x > y);;

(* TODO: Write a comment to explain what are_transitions_valid does. *)
(* TODO: Uncomment the are_transitions_valid function and associated call. *)
let are_transitions_valid states sigma transitions =
  (length transitions) = (length states) * (length sigma) &&
  (are_lists_equal 
     (remove_dupes (sort (find_states_used transitions) (>)))
     (sort states (>))) &&
  (are_lists_equal
     (sort (cartesian_product_better states sigma) q_sigma_gt)
     (sort (map get_transition_inputs transitions) q_sigma_gt));;

(*checks to see if transitions are possible to make from certain inputs*)

are_transitions_valid (generate_states 4) ab_alphabet ab_transitions;;

let is_start_valid start states = contains states start;;

is_start_valid 0 [3;2;1;0];;

is_start_valid 0 [5;4;3;2];;

(* TODO: What quantifier is tested in the below function?  Your choices are
   universal and existential.  Answer with a comment. *)
let are_finals_valid states finals = my_fold_left (&&) true (map (contains states) finals);;

(*existential*)

are_finals_valid [0; 1; 2; 3] [2; 1];;

are_finals_valid [0; 1; 2; 3] [1; 4];;

are_finals_valid [0; 1; 2; 3] [];;

are_finals_valid [0; 1; 2; 3] [3; 2; 1; 0];;

are_finals_valid [0; 1; 2; 3] [4; 3; 2; 1; 0];;

(* TODO: Complete and (uncomment) the following function and its call below. *)
let is_valid_DFA (states, sigma, transitions, start, finals) =
  (are_states_valid states) && (are_transitions_valid states sigma transitions)
  && (is_start_valid start states) && (are_finals_valid states finals);;

is_valid_DFA ((generate_states 4), ab_alphabet, ab_transitions, 0, [1;2]);;

(* TODO: Uncomment the following function and associated calls below. *)
let build_DFA transitions start finals =
  ( remove_dupes (sort (find_states_used transitions) (>)) ,
    remove_dupes (sort (find_alphabet transitions) (>)) ,
    transitions,
    start,
    finals );;

build_DFA ab_transitions 0 [1;2];;

is_valid_DFA (build_DFA ab_transitions 0 [1;2]);;

(* TODO: Uncomment the following code below. *)
let rec next_state transitions current ch =
  match transitions with
    | [] -> -1 (* transition not found - invalid state # *)
    | d::t -> 
        if (get_transition_inputs d) = (current, ch) then 
          get_transition_output d
        else
          next_state t current ch;;

let run_string (states, sigma, transitions, start, finals) w =
  let rec delta_hat ws current trace =
    match ws with
      | [] -> 
          trace@[(current, "")]
      | h::t ->
          delta_hat t (next_state transitions current h)
            trace@[(current, to_one_string (h::t))]
  in reverse (delta_hat (explode w) start []);;

run_string (build_DFA ab_transitions 0 [1;2]) "";;

run_string (build_DFA ab_transitions 0 [1;2]) "a";;

run_string (build_DFA ab_transitions 0 [1;2]) "aabbaab";;

let get_finals (_, _, _, _, finals) = finals;;

let is_string_valid dfa w =
  match (reverse (run_string dfa w)) with
    | (q, "")::_ -> contains (get_finals dfa) q
    | _ -> false;;

is_string_valid (build_DFA ab_transitions 0 [1;2]) "";;

is_string_valid (build_DFA ab_transitions 0 [1;2]) "a";;

is_string_valid (build_DFA ab_transitions 0 [1;2]) "aabbaab";;

let run_all_strings dfa w_list =
  map (run_string dfa) w_list;;

run_all_strings (build_DFA ab_transitions 0 [1;2]) [];;

run_all_strings (build_DFA ab_transitions 0 [1;2]) [""; "a"; "aabbaab"];;

let validate_all_strings dfa w_list =
  map (is_string_valid dfa) w_list;;

validate_all_strings (build_DFA ab_transitions 0 [1;2]) [];;

validate_all_strings (build_DFA ab_transitions 0 [1;2]) [""; "a"; "aabbaab"];;

let rec zip xs ys =
  match (xs, ys) with
    | ([], _) -> []
    | (_, []) -> []
    | (hx::tx, hy::ty) -> (hx, hy)::(zip tx ty);;

zip [1;2;3] ["Martha"; "Mark"; "Tasha"];;

let rec unzip zs =
  match zs with
    | [] -> ([], [])
    | (x, y)::zt ->
        let (xt, yt) = unzip zt in
          (x::xt, y::yt);;

let (xs, ys) = unzip (zip [1;2;3] ["Martha"; "Mark"; "Tasha"]);;

let ab_unit_tests = zip [""; "a"; "aabbaab"] [false; true; true];;

let ab_unit_tests_2 = zip [""; "a"; "aabbaab"] [true; false; false];;

let expect_true (s, v) = v = true;;

let (trues, falses) = (filter expect_true ab_unit_tests,
                       filter (neg expect_true) ab_unit_tests);;

let (xs, _) = unzip trues;;

let (ys, _) = unzip falses;;

(* TODO: What law in Discrete Math did I use in implementing the two
   functions below?  Write a comment. *)

(*Totalian Contradiction*)

let all_trues xs = my_fold_left ( && ) true xs;;

let all_falses xs = not (my_fold_left ( || ) false xs);;

let rec rezip zl jl =
  match (zl, jl) with
    | ([], _) -> []
    | (_, []) -> []
    | ((w, x)::zt, y::jt) -> (w, (x, y))::(rezip zt jt);;

let certify_pass_fail (test_case, (expected, actual)) =
  if expected = actual then (test_case, "Passed") else (test_case, "Failed");;

let check_unit_tests dfa tests =
  let (trues, falses) = (filter expect_true tests,
                         filter (neg expect_true) tests) in
  let ((accept_ws, _), (reject_ws, _)) = (unzip trues, unzip falses) in
  let (check_trues, check_falses) =
    (validate_all_strings dfa accept_ws, validate_all_strings dfa reject_ws) in
    (map certify_pass_fail (rezip trues check_trues),
     map certify_pass_fail (rezip falses check_falses));;

check_unit_tests (build_DFA ab_transitions 0 [1;2]) ab_unit_tests;;

check_unit_tests (build_DFA ab_transitions 0 [1;2]) ab_unit_tests_2;;

(* TODO: the last one.  Yay!!!!! Develop and test a DFA that accepts
   {(first)^(2i) (last)^(2j+1) | i, j >= 0}, assuming first represents
   your first name, and last represents your last name.  Do this for
   each team member. *)
ab_alphabet = ['b'; 'r'; 'a';'d';'e';'h';'p'];;

(* assume states are numbered 0, 1, 2, etc. *)

(* Note that the set of transitions is a finite subset of Q x Sigma x Q.  The
   form is (i, 'x', j), indicating that delta(q_i, x) = q_j. *)

ab_transitions = [ (0, 'b', 1);(1, 'r', 0);
                   (0, 'a', 2); (2, 'd', 0);
                   (0, 'h', 3);(3, 'a', 1);
                   (1, 'r', 0);(0, 'p', 4);
                   (4, 'e', 0);(0, 'r', 5)];;


length ab_transitions;;

generate_states 6;;

find_alphabet ab_transitions;;

find_states_used ab_transitions;;

sort (find_alphabet ab_transitions) (>);;

sort (find_states_used ab_transitions) (>);;

remove_dupes (sort (find_alphabet ab_transitions) (>));;

remove_dupes (sort (find_states_used ab_transitions) (>));;

sort (generate_states 6) (>);;

are_states_valid [3;2;1;0];;

are_states_valid [5;4;3;2];;

get_transition_inputs (0, 'a', 1);;

get_transition_output (0, 'a', 1);;

are_transitions_valid (generate_states 4) ab_alphabet ab_transitions;;

is_start_valid 0 [3;2;1;0];;

is_start_valid 0 [5;4;3;2];;

are_finals_valid [0; 1; 2; 3] [2; 1];;

are_finals_valid [0; 1; 2; 3] [1; 4];;

are_finals_valid [0; 1; 2; 3] [];;

are_finals_valid [0; 1; 2; 3] [3; 2; 1; 0];;

are_finals_valid [0; 1; 2; 3] [4; 3; 2; 1; 0];;

is_valid_DFA ((generate_states 5), ab_alphabet, ab_transitions, 0, [1;2]);;

build_DFA ab_transitions 0 [1;2];;

is_valid_DFA (build_DFA ab_transitions 0 [1;2]);;

run_string (build_DFA ab_transitions 0 [1;2]) "";;

run_string (build_DFA ab_transitions 0 [1;2]) "brad";;

run_string (build_DFA ab_transitions 0 [1;2]) "harper";;

is_string_valid (build_DFA ab_transitions 0 [1;2]) "";;

is_string_valid (build_DFA ab_transitions 0 [1;2]) "brad";;

is_string_valid (build_DFA ab_transitions 0 [1;2]) "harper";;

run_all_strings (build_DFA ab_transitions 0 [1;2]) [];;

run_all_strings (build_DFA ab_transitions 0 [1;2]) [""; "brad"; "harper"];;

validate_all_strings (build_DFA ab_transitions 0 [1;2]) [];;

validate_all_strings (build_DFA ab_transitions 0 [1;2]) [""; "brad"; "harper"];;

zip [1;2;3] ["Brad";"NoPartner"];;

let ab_unit_tests_3 = zip [""; "brad"; "harper"] [true; false; false];;

let ab_unit_tests_4 = zip [""; "harper"; "brad"] [true; false; false];;

check_unit_tests (build_DFA ab_transitions 0 [1;2]) ab_unit_tests_3;;

check_unit_tests (build_DFA ab_transitions 0 [1;2]) ab_unit_tests_4;;














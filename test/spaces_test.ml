open Experiment.Spaces.Predicate

let is_true = function Universally b -> b | _ -> false
let is_false = function Universally b -> not b | _ -> false
let is_neither = function Indeterminate _ -> true | _ -> false

let check_sorted str list prop =
  Alcotest.(check bool) str (run_predicate sorted list |> prop) true

let nil_sorted () = check_sorted "[] is sorted" nil is_true
let singleton_sorted () = check_sorted "[1] is sorted" singleton_one is_true

let one_two_three_sorted () =
  check_sorted "[1;2;3] is sorted" one_two_three is_true

let one_two_three_bot_indetermined () =
  check_sorted "1::2::3::bot] is indetermined" one_two_three_bot is_neither

let three_two_one_not_sorted () =
  check_sorted "3::2::1::[] not sorted" three_two_one is_false

let three_two_one_bot_not_sorted () =
  check_sorted "3::2::1::bot not sorted" three_two_one_bot is_false

let suite =
  ( "Predicate",
    [
      ("[]", `Quick, nil_sorted);
      ("[1]", `Quick, singleton_sorted);
      ("1::2::3::[]", `Quick, one_two_three_sorted);
      ("1::2::3::bot", `Quick, one_two_three_bot_indetermined);
      ("3::2::1::[]", `Quick, three_two_one_not_sorted);
      ("3::2::1::bot", `Quick, three_two_one_bot_not_sorted);
    ] )

let () = Alcotest.run "predicates on partial values" [ suite ]

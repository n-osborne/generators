open Experiment.Transformation.Examples
open Experiment.Transformation.Examples2

let is_sorted l =
  let rec aux = function
    | [] -> true
    | [ _ ] -> true
    | m :: n :: ns -> m <= n && aux (n :: ns)
  in
  List.map to_int l |> aux

let nat_gen = QCheck.Gen.(oneof List.(init 10 (fun i -> return (from_int i))))
let gen = QCheck.Gen.(list_size (return 10) nat_gen)

let sorted_test =
  QCheck.Test.make (QCheck.make gen)
    (fun l ->
      let s = sorted l in
      Seq.(for_all is_sorted (take 100000 s)))
    ~count:1000 ~name:"sorted return sorted lists"

let gen_complete_test =
  let gen = QCheck.Gen.(int_bound 20 >>= gen_complete int) in
  let prop = Option.fold ~none:false ~some:complete in
  QCheck.Test.make (QCheck.make gen) prop ~count:1000
    ~name:"gen_complete generate complete trees"

let gen_bst_test =
  let gen =
    QCheck.Gen.(
      int_bound 20 >>= fun size -> gen_bst size Int.min_int Int.max_int)
  in
  QCheck.Test.make (QCheck.make gen)
    (bst Int.min_int Int.max_int)
    ~count:1000 ~name:"gen_bst generates bsts"

let gen_bst_opt_test =
  let prop = Option.fold ~none:false ~some:(bst Int.min_int Int.max_int) in
  let gen =
    QCheck.Gen.(
      int_bound 20 >>= fun size -> gen_bst_opt size Int.min_int Int.max_int)
  in
  QCheck.Test.make
    (QCheck.make ~print:(Option.fold ~none:"None" ~some:to_string) gen)
    prop ~count:1000 ~name:"gen_bst generates bsts"

let _ =
  QCheck_runner.run_tests ~verbose:true
    [
      (*sorted_test; *)
      (* gen_complete_test; *)
      (* gen_bst_test; *)
      gen_bst_opt_test;
    ]

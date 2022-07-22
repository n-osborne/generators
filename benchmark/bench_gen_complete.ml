open Experiment.Transformation.Examples2

let n = 1000

let time g =
  let st = Random.State.make_self_init () in
  let t = Mtime_clock.counter () in
  for _ = 0 to n do
    try g st |> ignore with _ -> ()
  done;
  Mtime.Span.to_ms (Mtime_clock.count t)

(* let () = *)
(*   Out_channel.with_open_text "time_gen_complete.data" (fun oc -> *)
(*       for i = 0 to 20 do *)
(*         let t = time (gen_complete QCheck.Gen.int i) in *)
(*         Printf.fprintf oc "%i,%f\n" i t *)
(*       done) *)

(* let () = *)
(*   Out_channel.with_open_text "time_gen_bst.data" (fun oc -> *)
(*       for i = 0 to 20 do *)
(*         let t = time (gen_bst_opt i Int.min_int Int.max_int) in *)
(*         Printf.fprintf oc "%i,%f\n" i t *)
(*       done) *)

let distribution a g =
  let st = Random.State.make_self_init () in
  for _ = 0 to n do
    try
      match g st with
      | None -> ()
      | Some t ->
          let i = size t in
          a.(i) <- a.(i) + 1
    with _ -> ()
  done

let () =
  let a = Array.make 20 0 in
  let () = distribution a (gen_bst_opt 20 Int.min_int Int.max_int) in
  Out_channel.with_open_text "distribution_bst.data" (fun oc ->
      Array.to_list a |> List.map Int.to_string |> String.concat ","
      |> Printf.fprintf oc "%s")

open Experiment.Transformation.Examples2

let n = 1000

let time i =
  let st = Random.State.make_self_init () in
  let g = gen_complete QCheck.Gen.int i in
  let t = Mtime_clock.counter () in
  for _ = 0 to n do
    try g st |> ignore with _ -> ()
  done;
  Mtime.Span.to_ms (Mtime_clock.count t)

let () =
  Out_channel.with_open_text "time_gen_complete.data" (fun oc ->
      for i = 0 to 20 do
        let t = time i in
        Printf.fprintf oc "%i,%f\n" i t
      done)

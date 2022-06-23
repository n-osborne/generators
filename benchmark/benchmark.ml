open Experiment.Spaces

let int_of_binary l =
  let rec aux i = function
    | [] -> i
    | true :: xs -> aux ((i * 2) + 1) xs
    | false :: xs -> aux (i * 2) xs
  in
  aux 0 l

let time f i =
  let t = Mtime_clock.counter () in
  let g = f i in
  for _ = 0 to 9 do
    try g () |> ignore with _ -> ()
  done;
  Mtime.Span.to_ms (Mtime_clock.count t)

let mk_data f l = List.map f l |> String.concat ","

let () =
  let bs =
    List.init 10000 (fun _ -> StoreCardinal.(uniform_sized sp_bool_list) 10 ())
  in
  let ns = List.map int_of_binary bs in
  let a = Array.make 512 0 in
  let () = List.iter (fun i -> a.(i) <- a.(i) + 1) ns in
  let data = Array.to_list a |> mk_data Int.to_string in
  Out_channel.with_open_text "distribution_bool_list.data" (fun oc ->
      Printf.fprintf oc "%s\n" data)

let space path g n to_string () =
  let sizes = List.init n succ in
  let data = List.map (time g) sizes |> mk_data to_string in
  Out_channel.with_open_text path (fun oc -> Printf.fprintf oc "%s\n" data)

let () =
  space "time_nat_list_naive.data"
    Naive.(uniform_sized sp_nat_list)
    21 Float.to_string ()

let () =
  space "time_nat_list_store.data"
    StoreCardinal.(uniform_sized sp_nat_list)
    25 Float.to_string ()

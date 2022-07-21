open Experiment.Transformation.Examples

let int_of_nat_list l =
  let rec aux i = function
    | [] -> i
    | x :: xs -> aux ((i * 10) + to_int x) xs
  in
  aux 0 l

let is_sorted s =
  let b = ref true in
  let len = String.length s in
  for i = 0 to len - 2 do
    if s.[i] > s.[i + 1] then b := false
  done;
  !b

let init () = List.init 4 (fun _ -> Random.int 10 |> from_int)

let filter_mapi f =
  let rec aux i f = function
    | [] -> []
    | x :: xs -> (
        match f i x with
        | Some a -> a :: aux (i + 1) f xs
        | None -> aux (i + 1) f xs)
  in
  aux 0 f

let p i x =
  let s = Int.to_string i in
  if is_sorted s then Some (i, x) else None

let () =
  let a = Array.make 10000 0 in
  let seq = init () |> sorted |> Seq.take 10000 in
  let () =
    Seq.iter
      (fun x ->
        let i = int_of_nat_list x in
        a.(i) <- a.(i) + 1)
      seq
  in
  let data = Array.to_list a |> filter_mapi p in
  Out_channel.with_open_text "distribution_sorted_nat_list.data" (fun oc ->
      List.iter (fun (x, y) -> Printf.fprintf oc "%i,%i\n" x y) data)

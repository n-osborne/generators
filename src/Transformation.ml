(* transformation of inductive predicate into recursive function *)

module Examples = struct
  let ( %-> ) x f = Seq.flat_map f x
  let ( ++ ) = Seq.append

  (* vendored version of Seq.iterate (since 4.14) *)
  let iterate f x () =
    let open Seq in
    let y = f x in
    Cons (y, iterate f y)

  (* Append:
     - (nil, ys, ys) \in Append
     - (xs, ys, zs) \in Append -> (x::xs, ys, x::zs) \in Append
  *)
  let rec append_1_2 (input : 'a list * 'a list) : 'a list Seq.t =
    (Seq.return input %-> function [], xs -> Seq.return xs | _ -> Seq.empty)
    ++ Seq.return input %-> function
       | x :: xs, ys -> append_1_2 (xs, ys) %-> fun zs -> Seq.return (x :: zs)
       | _ -> Seq.empty

  let rec append_3 (input : 'a list) : ('a list * 'a list) Seq.t =
    (Seq.return input %-> function zs -> Seq.return ([], zs))
    ++ Seq.return input %-> function
       | z :: zs -> append_3 zs %-> fun (xs, ys) -> Seq.return (z :: xs, ys)
       | _ -> Seq.empty

  type nat = Z | S of nat

  let succ n = S n
  let rec from_int = function 0 -> Z | n -> from_int (n - 1) |> succ
  let rec to_int = function Z -> 0 | S n -> 1 + to_int n
  let nat_gen () = Random.int 10 |> from_int

  let gte n =
    let i = to_int n in
    assert (i < 10);
    List.(to_seq (init (10 - i) (fun x -> x + i |> from_int)))

  let nats = gte Z

  (* preorder:
     - (Z, m) \in Preorder
     - (n, m) \in Preoder -> (S n, S m) \in Preorder
  *)
  let rec preorder_1 (input : nat) : nat Seq.t =
    (Seq.return input %-> function Z -> nats | _ -> Seq.empty)
    ++ Seq.return input %-> function
       | S n -> preorder_1 n %-> fun m -> Seq.return (S m)
       | _ -> Seq.empty

  let rec preorder_2 (input : nat) : nat Seq.t =
    (Seq.return input %-> fun _ -> Seq.return Z)
    ++ Seq.return input %-> function
       | S m -> preorder_2 m %-> fun n -> Seq.return (S n)
       | _ -> Seq.empty

  (* addition
     - (Z, n, n) \in Add
     - (n, m, r) \in Add -> (S n, m, S r) \in Add
  *)
  let rec addition_1_2 (input : nat * nat) : nat Seq.t =
    (Seq.return input %-> function Z, n -> Seq.return n | _, _ -> Seq.empty)
    ++ Seq.return input %-> function
       | S n, m -> addition_1_2 (n, m) %-> fun r -> Seq.return (S r)
       | _, _ -> Seq.empty

  (* element
     - (x, x :: ys) \in In
     - (x, ys) \in In -> (x, y :: ys) \in In *)
  let rec in_2 (input : 'a list) : 'a Seq.t =
    (Seq.return input %-> function x :: _ -> Seq.return x | _ -> Seq.empty)
    ++ Seq.return input %-> function
       | _ :: ys -> in_2 ys %-> fun x -> Seq.return x
       | _ -> Seq.empty

  (* sorted traditional definition as unary predicate
     - [] \in Sorted
     - [x] \in Sorted
     - (x, y) \in Preorder -> y::ys \in Sorted -> x::y::ys \in Sorted
  *)
  let rec sorted (input : nat list) : nat list Seq.t =
    (* first case: the empty list is sorted *)
    (Seq.return input %-> function [] -> Seq.return [] | _ -> Seq.empty)
    (* second case: singleton is sorted *)
    ++ (Seq.return input %-> function
        | [ x ] -> Seq.return [ x ]
        | _ -> Seq.empty)
    (* third case: the inductive one *)
    ++ Seq.return input %-> function
       | x :: _ :: xs ->
           gte x %-> fun y ->
           sorted (y :: xs) %-> fun xs -> Seq.return (x :: xs)
       | _ -> Seq.empty

  (* sorted as a binary relation between the head and the rest of the lsit
     - (None, []) \in Sorted
     - (None, [x])  \in Sorted
     - (x, y) \in Preoder -> (None, y::ys) \in Sorted -> (Some x, y::ys) \in Sorted
  *)
  let rec sorted_1 size (input : nat option) : nat list Seq.t =
    (* base case to avoid infinite lists *)
    if size = 0 then Seq.return []
    else
      (* first case: empty list is sorted *)
      (Seq.return input %-> function None -> Seq.return [] | _ -> Seq.empty)
      (* second case: singleton is sorted *)
      ++ (Seq.return input %-> function
          | None -> Seq.map (fun x -> [ x ]) nats
          | _ -> Seq.empty)
      (* third case: inductive case *)
      ++ Seq.return input %-> function
         | Some x ->
             preorder_1 x %-> fun y ->
             sorted_1 (size - 1) (Some y) %-> fun xs -> Seq.return (x :: xs)
         | None -> Seq.empty
end

module Examples2 = struct
  (** this module proposes examples of translation of inductive predicates to QCheck generators inspired by
      *Generating Good Generators for Inductive Relations*
  *)

  open QCheck

  (* examples from the paper talk about binary trees *)
  type tree = Leaf | Node of int * tree * tree

  let rec size = function
    | Leaf -> 0
    | Node (_, l, r) -> 1 + max (size l) (size r)

  let rec to_string = function
    | Leaf -> "leaf"
    | Node (i, l, r) ->
        Int.to_string i ^ "(" ^ to_string l ^ ", " ^ to_string r ^ ")"

  (* complete :=
     | CompleteLeaf 0 Leaf
     | \forall n x l r, complete n l -> complete n r -> complete (Node (S n) l r)
  *)
  let rec complete : tree -> bool = function
    | Leaf -> true
    | Node (_, l, r) -> size l = size r && complete l && complete r

  let rec gen_complete gen in1 : tree option Gen.t =
    let open Gen in
    match in1 with
    | 0 -> return (Some Leaf)
    | n when n > 0 -> (
        gen_complete gen (n - 1) >>= function
        | Some l -> (
            gen_complete gen (n - 1) >>= function
            | Some r -> gen >>= fun x -> return (Some (Node (x, l, r)))
            | None -> return None)
        | None -> return None)
    | _ -> return None

  let rec rm l i =
    match l with [] -> [] | x :: xs -> if i = 0 then xs else x :: rm xs (i - 1)

  exception Internal_error of string

  let rec backtrack_opt l =
    match l with
    | [] -> raise (Internal_error "backtrack_opt")
    | xs -> (
        let i = Random.int (List.length xs) in
        let open Gen in
        List.nth xs i >>= function
        | Some x -> return (Some x)
        | None -> backtrack_opt (rm l i))

  let rec backtrack = function
    | [] -> raise (Internal_error "backtrack")
    | xs -> (
        let i = Random.int (List.length xs) in
        try
          (* for some obscure reason, the exception is not caught *)
          let open Gen in
          List.nth xs i >>= fun x -> return x
        with _ -> backtrack (rm xs i))

  let rec bst lo hi = function
    | Leaf -> true
    | Node (i, l, r) -> lo <= i && i <= hi && bst lo i l && bst i hi r

  (* example from the paper but with exception instead of options *)
  let rec gen_bst size in1 in2 : tree Gen.t =
    let open Gen in
    match size with
    | 0 -> return Leaf
    | s when s > 0 ->
        backtrack
          [
            return Leaf;
            ( small_nat >>= fun n ->
              let x = n + in1 in
              assume (x < in2);
              (* original version uses a conditional branchment here *)
              gen_bst (s - 1) in1 x >>= fun l ->
              gen_bst (s - 1) x in2 >>= fun r -> return (Node (x, l, r)) );
          ]
    | _ -> raise (Internal_error "negative size")

  (* in1 is lo and in2 is hi *)
  let rec gen_bst_opt size in1 in2 : tree option Gen.t =
    let open Gen in
    match size with
    | 0 -> return (Some Leaf)
    | s when s > 0 ->
        backtrack_opt
          [
            return (Some Leaf);
            (* either a leaf so that we can alwayse generate something *)
            (* or a node; but that can fail *)
            ( int_bound 10 >>= fun n ->
              let x = n + in1 in
              if x < in2 then
                gen_bst_opt (s - 1) in1 x >>= function
                | None -> return None
                | Some l -> (
                    gen_bst_opt (s - 1) x in2 >>= function
                    | None -> return None
                    | Some r -> return (Some (Node (x, l, r))))
              else return None );
          ]
    | _ -> failwith "negative size"
end

(* transformation of inductive predicate into recursive function *)

module Examples = struct
  let ( %-> ) x f = Seq.flat_map f x
  let ( ++ ) = Seq.append

  (* vendored version of Seq.iterate (since 4.14) *)
  let rec iterate f x () =
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

  (* preorder:
     - (Z, m) \in Preorder
     - (n, m) \in Preoder -> (S n, S m) \in Preorder
  *)
  let rec preorder_1 (input : nat) : nat Seq.t =
    (Seq.return input %-> function Z -> iterate succ Z | _ -> Seq.empty)
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
end

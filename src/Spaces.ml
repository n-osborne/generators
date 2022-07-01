(* Experiment based on:
   *Generating constrained random data with uniform distribution*, Koen Claessen, Jonas Duregard, Michal H. Palka (2015) DOI: 10.1007/978-3-319-07151-0_2 *)
type _ space =
  | Void : 'a space
  | Pure : 'a -> 'a space
  | Sum : 'a space * 'a space -> 'a space
  | Product : 'a space * 'b space -> ('a * 'b) space
  | Pay : 'a space -> 'a space
  | Map : ('a -> 'b) * 'a space -> 'b space

type 'a predicate = 'a -> bool option

let sp_bool = Sum (Pure true, Pure false)

type nat = Z | S of nat

let pp_nat n =
  let rec aux i = function Z -> Int.to_string i | S n -> aux (i + 1) n in
  aux 0 n

let s n = S n
let rec sp_nat : nat space = Pay (Sum (Pure Z, Map (s, sp_nat)))
let cons (n, ns) = n :: ns

let rec sp_nat_list = Pay (Sum (sp_nil, sp_cons))
and sp_nil = Pure []
and sp_cons = Map (cons, Product (sp_nat, sp_nat_list))

let sorted : nat list predicate = function
  | [] -> Some true
  | [ _ ] -> Some true
  | [ m; n ] -> Some (m <= n)
  | m :: n :: _ -> if m <= n then None else Some false

let rec sp_bool_list : bool list space =
  Pay (Sum (Pure [], Map (cons, Product (sp_bool, sp_bool_list))))

type lambda_term =
  | One
  | Var of nat
  | Lam of lambda_term
  | App of (lambda_term * lambda_term)

let var n = Var n
let lam t = Lam t
let app (f, a) = App (f, a)

let rec sp_lambda_term =
  Pay (Sum (Sum (value, variable), Sum (abstraction, redex)))

and value = Pure One
and variable = Map (var, sp_nat)
and abstraction = Map (lam, sp_lambda_term)
and redex = Map (app, Product (sp_lambda_term, sp_lambda_term))

module Naive = struct
  type _ set =
    | Empty : 'a set
    | Singleton : 'a -> 'a set
    | Union : 'a set * 'a set -> 'a set
    | CartesianProduct : 'a set * 'b set -> ('a * 'b) set
    | Map : ('a -> 'b) * 'a set -> 'b set

  let union a b = Union (a, b)

  let big_union = function
    | [] -> assert false
    | [ s ] -> s
    | s :: xs -> List.fold_left union s xs

  let rec cardinal : type a. a set -> int = function
    | Empty -> 0
    | Singleton _ -> 1
    | Union (l, r) -> cardinal l + cardinal r
    | CartesianProduct (l, r) -> cardinal l * cardinal r
    | Map (_, s') -> cardinal s'

  let rec index : 'a. 'a set -> int -> 'a =
    fun (type a) (s : a set) i ->
     match s with
     | Empty -> assert false
     | Singleton a when i = 0 -> a
     | Singleton _ -> assert false
     | Union (l, _) when i < cardinal l -> index l i
     | Union (l, r) -> index r (i - cardinal l)
     | CartesianProduct (l, r) ->
         (index l (i / cardinal r), index r (i mod cardinal r))
     | Map (f, s') -> f (index s' i)

  let uniform s =
    let c = cardinal s in
    if c = 0 then failwith "empty set"
    else
      let i = Random.int c in
      index s i

  let rec sized : 'a. 'a space -> int -> 'a set =
    fun (type a) (s : a space) k ->
     match s with
     | Void -> Empty
     | Pure a -> if k = 0 then Singleton a else Empty
     | Sum (l, r) -> union (sized l k) (sized r k)
     | Product (l, r) ->
         let ks =
           List.map (fun k0 -> (k0, k - k0)) (List.init (k + 1) Fun.id)
         in
         big_union
           (List.map
              (fun (k0, k1) -> CartesianProduct (sized l k0, sized r k1))
              ks)
     | Pay s -> if k = 0 then Empty else sized s (k - 1)
     | Map (f, s) -> Map (f, sized s k)

  (* partial application allow to compute [sized s k] only one time *)
  let uniform_sized s k =
    let s = sized s k in
    fun () -> uniform s
  (* let uniform_sized s k = uniform (sized s k) *)
end

module StoreCardinal = struct
  type _ set' =
    | Empty : 'a set'
    | Singleton : 'a -> 'a set'
    | Union : 'a set * 'a set -> 'a set'
    | CartesianProduct : 'a set * 'b set -> ('a * 'b) set'
    | Map : ('a -> 'b) * 'a set -> 'b set'

  and 'a set = { set : 'a set'; cardinal : int }

  let set set cardinal = { set; cardinal }
  let singleton a = set (Singleton a) 1
  let union a b = set (Union (a, b)) (a.cardinal + b.cardinal)
  let product a b = set (CartesianProduct (a, b)) (a.cardinal * b.cardinal)
  let map f a = set (Map (f, a)) a.cardinal

  let big_union = function
    | [] -> assert false
    | [ s ] -> s
    | s :: xs -> List.fold_left union s xs

  let rec index : 'a. 'a set -> int -> 'a =
    fun (type a) (s : a set) i ->
     match s.set with
     | Empty -> failwith "empty set"
     | Singleton a when i = 0 -> a
     | Singleton _ -> failwith "index out of range"
     | Union (l, _) when i < l.cardinal -> index l i
     | Union (l, r) -> index r (i - l.cardinal)
     | CartesianProduct (l, r) ->
         (index l (i / r.cardinal), index r (i mod r.cardinal))
     | Map (f, s') -> f (index s' i)

  let uniform s =
    if s.cardinal = 0 then failwith "cardinality is 0"
    else
      let i = Random.int s.cardinal in
      index s i

  let rec sized : 'a. 'a space -> int -> 'a set =
    fun (type a) (s : a space) k ->
     match s with
     | Void -> { set = Empty; cardinal = 0 }
     | Pure a -> if k = 0 then singleton a else { set = Empty; cardinal = 0 }
     | (Sum (Pure _, s) | Sum (s, Pure _)) when k > 0 -> sized s k
     | (Sum (Pay _, s) | Sum (s, Pay _)) when k = 0 -> sized s k
     | Sum (l, r) -> union (sized l k) (sized r k)
     | (Product (Pure _, _) | Product (_, Pure _)) when k > 0 ->
         { set = Empty; cardinal = 0 }
     | (Product (Pay _, _) | Product (_, Pay _)) when k = 0 ->
         { set = Empty; cardinal = 0 }
     | Product (l, r) ->
         let ks =
           List.map (fun k0 -> (k0, k - k0)) (List.init (k + 1) Fun.id)
         in
         big_union
           (List.map (fun (k0, k1) -> product (sized l k0) (sized r k1)) ks)
     | Pay s -> if k = 0 then { set = Empty; cardinal = 0 } else sized s (k - 1)
     | Map (f, s) -> sized s k |> map f

  let rec sizedP :
            'a. 'a predicate -> 'a space -> int -> ('a, 'a space) Either.t =
    fun (type a) p (s : a space) k ->
     let open Either in
     match s with
     | Void -> Right Void
     | Pure a ->
         if k = 0 then match p a with Some true -> Left a | _ -> Right Void
         else Right Void
     | (Sum (Pure _, s) | Sum (s, Pure _)) when k > 0 -> sizedP p s k
     | (Sum (Pay _, s) | Sum (s, Pay _)) when k = 0 -> sizedP p s k
     | Sum (l, r) -> assert false
     | (Product (Pure _, _) | Product (_, Pure _)) when k > 0 -> Right Void
     | (Product (Pay _, _) | Product (_, Pay _)) when k = 0 -> Right Void
     | Product (l, r) -> assert false
     | Pay s -> if k = 0 then Right Void else sizedP p s (k - 1)
     | Map (f, s) ->
         let _p' x = p (f x) in
         let _apply f = function
           | Left x -> Left (f x)
           | Right sp -> Right (Map (f, sp))
         in
         assert false

  (* partial application allow to compute [sized s k] only one time *)
  let uniform_sized s k =
    let s = sized s k in
    fun () -> uniform s
  (* uniform (sized s k) *)
end

module Predicate = struct
  (** partially defined value and predicate on them in order to lazyly
      explore a space, and prune it when needed *)

  (* a type for partial values

     - this should obviously include bottom
     - a totally defined value is a partially defined value
     - a partially evaluated (functional) constructor is a partially defined value

     partially defined list is: `1::2::⊥`

     Pure 1 : int partial
     App (fun x -> Leaf x, Bot) : tree partial ≡  Leaf ⊥
     App (Pure (fun x y -> x :: y), 1) : int list -> int list partial ≡ fun l -> 1::l
  *)
  type _ partial =
    | Bottom : 'a partial
    | Pure : 'a -> 'a partial (* injection of total values in partial values *)
    | App1 :
        ('a -> 'a) partial * 'a partial
        -> 'a partial (* partial is an applicative functor *)
    | App2 : ('a -> 'b -> 'b) partial * 'a partial * 'b partial -> 'b partial

  let cons : (int -> int list -> int list) partial = Pure (fun x xs -> x :: xs)
  let nil : int list partial = Pure []
  let one : int partial = Pure 1
  let two : int partial = Pure 2
  let three : int partial = Pure 3
  let ( @@@ ) (x : int partial) (xs : int list partial) = App2 (cons, x, xs)
  let one_two_three : int list partial = one @@@ two @@@ three @@@ nil
  let three_two_one : int list partial = three @@@ two @@@ one @@@ nil
  let one_two_three_bot : int list partial = one @@@ two @@@ three @@@ Bottom
  let three_two_one_bot : int list partial = three @@@ two @@@ one @@@ Bottom

  (* utop #  one_two_three_bot;;
     - : int list partial =
     App (App (Pure <fun>, Pure <poly>),
      App (App (Pure <fun>, Pure <poly>),
       App (App (Pure <fun>, Pure <poly>), Bottom)))
  *)

  let rec reduce : type a. a partial -> a partial = function
    | App1 (f, a) -> (
        match (reduce f, reduce a) with
        | Pure f, Pure a -> Pure (f a)
        | f, a -> App1 (f, a))
    | App2 (f, a, b) -> (
        match (reduce f, reduce a, reduce b) with
        | Pure f, Pure a, Pure b -> Pure (f a b)
        | Pure f, Pure a, b -> App1 (Pure (f a), b)
        | f, a, b -> App2 (f, a, b))
    | p -> p

  (* utop # reduce one_two_three_bot;;
     - : int list partial =
     App (Pure <fun>, App (Pure <fun>, App (Pure <fun>, Bottom)))
  *)

  let rec unwrap : type a. a partial -> a option = function
    | Bottom -> None
    | Pure a -> Some a
    | App1 (f, a) -> (
        match (unwrap f, unwrap a) with
        | Some f, Some a -> Some (f a)
        | _, _ -> None)
    | App2 (f, a, b) -> (
        match (unwrap f, unwrap a, unwrap b) with
        | Some f, Some a, Some b -> Some (f a b)
        | _, _, _ -> None)

  type 'a predicate =
    | Universally of bool
    | Indeterminate of ('a partial -> 'a predicate)

  let sorted : int list predicate =
    let rec p : int list partial -> int list predicate = function
      | Pure [] -> Universally true
      | Pure [ _ ] -> Universally true
      | Pure (m :: n :: ns) ->
          if m > n then Universally false else p (Pure (n :: ns))
      | App2 (Pure f, Pure m, App2 (Pure f, Pure n, ns)) ->
          if m > n then Universally false else p (App2 (f, Pure n, ns))
      | _ -> Indeterminate p
    in
    Indeterminate p

  let run_predicate : 'a predicate -> 'a partial -> 'a predicate =
   fun p a ->
    match (p, reduce a) with
    | Indeterminate p, Pure a -> p (Pure a)
    | Indeterminate _, App1 _ -> p
    | Indeterminate _, Bottom -> p
    | _, _ -> p

  (* utop # run_predicate sorted one_two_three;;
     - : int list predicate = Universally true

     utop # run_predicate sorted three_two_one;;
     - : int list predicate = Universally false

     utop # run_predicate sorted one_two_three_bot;;
     - : int list predicate = Indeterminate <fun>

     utop # run_predicate sorted three_two_one_bot;;
     - : int list predicate = Indeterminate <fun>

     XXX: does not work
  *)
end

module Partial = struct
  (* given an ADT, it is possible to derive (e.g. with a ppx) the corresponding type of partial value *)
  type 'a plist = Bot | Nil | Cons of 'a * 'a plist
  (* *Note:*
     the value can only be partial in the inductive argument (Bot : 'a plist)
     or 'a need to be a type of partial value
  *)

  type 'a predicate = Always of bool | Indeterminate of ('a -> 'a predicate)

  let run (p : 'a predicate) (a : 'a) =
    match p with Indeterminate f -> f a | _ -> p

  let ( @@@ ) x xs = Cons (x, xs)
  let one_two_three = 1 @@@ 2 @@@ 3 @@@ Nil
  let three_two_one = 3 @@@ 2 @@@ 1 @@@ Nil
  let one_two_three_bot = 1 @@@ 2 @@@ 3 @@@ Bot
  let three_two_one_bot = 3 @@@ 2 @@@ 1 @@@ Bot

  let sorted : int plist predicate =
    let rec p = function
      | Bot -> Indeterminate p
      | Cons (_, Bot) -> Indeterminate p
      | Nil -> Always true
      | Cons (_, Nil) -> Always true
      | Cons (m, Cons (n, ns)) ->
          if m > n then Always false else p (Cons (n, ns))
    in
    Indeterminate p
end

(* Experiment based on:
   *Generating constrained random data with uniform distribution*, Koen Claessen, Jonas Duregard, Michal H. Palka (2015) DOI: 10.1007/978-3-319-07151-0_2 *)
type _ space =
  | Void : 'a space
  | Pure : 'a -> 'a space
  | Sum : 'a space * 'a space -> 'a space
  | Product : 'a space * 'b space -> ('a * 'b) space
  | Pay : 'a space -> 'a space
  | Map : ('a -> 'b) * 'a space -> 'b space

let sp_bool = Sum (Pure true, Pure false)

type nat = Z | S of nat

let pp_nat n =
  let rec aux i = function Z -> Int.to_string i | S n -> aux (i + 1) n in
  aux 0 n

let s n = S n
let rec sp_nat : nat space = Pay (Sum (Pure Z, Map (s, sp_nat)))
let cons (n, ns) = n :: ns

let rec sp_nat_list : nat list space =
  Pay (Sum (Pure [], Map (cons, Product (sp_nat, sp_nat_list))))

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
  Pay
    (Sum
       ( Sum (Pure One, Map (var, sp_nat)),
         Sum
           ( Map (lam, sp_lambda_term),
             Map (app, Product (sp_lambda_term, sp_lambda_term)) ) ))

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

  let uniform_sized s k = uniform (sized s k)
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
     | Empty -> assert false
     | Singleton a when i = 0 -> a
     | Singleton _ -> assert false
     | Union (l, _) when i < l.cardinal -> index l i
     | Union (l, r) -> index r (i - l.cardinal)
     | CartesianProduct (l, r) ->
         (index l (i / r.cardinal), index r (i mod r.cardinal))
     | Map (f, s') -> f (index s' i)

  let uniform s =
    if s.cardinal = 0 then failwith "empty set"
    else
      let i = Random.int s.cardinal in
      index s i

  let rec sized : 'a. 'a space -> int -> 'a set =
    fun (type a) (s : a space) k ->
     match s with
     | Void -> { set = Empty; cardinal = 0 }
     | Pure a -> if k = 0 then singleton a else { set = Empty; cardinal = 0 }
     | Sum (l, r) -> union (sized l k) (sized r k)
     | Product (l, r) ->
         let ks =
           List.map (fun k0 -> (k0, k - k0)) (List.init (k + 1) Fun.id)
         in
         big_union
           (List.map (fun (k0, k1) -> product (sized l k0) (sized r k1)) ks)
     | Pay s -> if k = 0 then { set = Empty; cardinal = 0 } else sized s (k - 1)
     | Map (f, s) -> sized s k |> map f

  let uniform_sized s k = uniform (sized s k)
end

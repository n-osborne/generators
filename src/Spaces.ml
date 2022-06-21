type _ space =
  | Void : 'a space
  | Pure : 'a -> 'a space
  | Sum : 'a space * 'a space -> 'a space
  | Product : 'a space * 'b space -> ('a * 'b) space
  | Pay : 'a space -> 'a space
  | Map : ('a -> 'b) * 'a space -> 'b space

let ( $ ) f s = Map (f, s)

let ( <*> ) s0 s1 =
  let f (f, a) = f a in
  Map (f, Product (s0, s1))

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
       let ks = List.map (fun k0 -> (k0, k - k0)) (List.init (k + 1) Fun.id) in
       big_union
         (List.map
            (fun (k0, k1) -> CartesianProduct (sized l k0, sized r k1))
            ks)
   | Pay s -> if k = 0 then Empty else sized s (k - 1)
   | Map (f, s) -> Map (f, sized s k)

let uniform_sized s k = uniform (sized s k)

type nat = Z | S of nat

let s n = S n
let rec sp_nat : nat space = Pay (Sum (Pure Z, Map (s, sp_nat)))

let rec sp_list_nat : nat list space =
  let cons (n, ns) = n :: ns in
  Pay (Sum (Pure [], Map (cons, Product (sp_nat, sp_list_nat))))

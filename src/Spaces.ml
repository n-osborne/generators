module Naive = struct
  type _ space =
    | Void : 'a space
    | Pure : 'a -> 'a space
    | Sum : 'a space * 'a space -> 'a space
    | Product : 'a space * 'b space -> ('a * 'b) space
    | Pay : 'a space -> 'a space
    | Map : ('a -> 'b) * 'a space -> 'b space

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

  type nat = Z | S of nat

  let pp_nat n =
    let rec aux i = function Z -> Int.to_string i | S n -> aux (i + 1) n in
    aux 0 n

  let s n = S n
  let rec sp_nat : nat space = Pay (Sum (Pure Z, Map (s, sp_nat)))

  let rec sp_list (sp : 'a space) : 'a list space =
    let cons (n, ns) = n :: ns in
    Pay (Sum (Pure [], Map (cons, Product (sp, sp_list sp))))

  let sp_nat_list = sp_list sp_nat
end

module Memoize = struct
  type _ space =
    | Void : 'a space
    | Pure : 'a -> 'a space
    | Sum : 'a space * 'a space -> 'a space
    | Product : 'a space * 'b space -> ('a * 'b) space
    | Pay : 'a space -> 'a space
    | Map : ('a -> 'b) * 'a space -> 'b space

  type _ set =
    | Empty : 'a set
    | Singleton : 'a -> 'a set
    | Union : 'a mset * 'a mset -> 'a set
    | CartesianProduct : 'a mset * 'b mset -> ('a * 'b) set
    | Map : ('a -> 'b) * 'a mset -> 'b set

  and 'a mset = { set : 'a set; cardinal : int }

  let mset set cardinal = { set; cardinal }
  let msingleton a = mset (Singleton a) 1
  let munion a b = mset (Union (a, b)) (a.cardinal + b.cardinal)
  let mproduct a b = mset (CartesianProduct (a, b)) (a.cardinal * b.cardinal)
  let mmap f a = mset (Map (f, a)) a.cardinal

  let big_union = function
    | [] -> assert false
    | [ s ] -> s
    | s :: xs -> List.fold_left munion s xs

  let rec index : 'a. 'a mset -> int -> 'a =
    fun (type a) (s : a mset) i ->
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

  let rec sized : 'a. 'a space -> int -> 'a mset =
    fun (type a) (s : a space) k ->
     match s with
     | Void -> { set = Empty; cardinal = 0 }
     | Pure a -> if k = 0 then msingleton a else { set = Empty; cardinal = 0 }
     | Sum (l, r) -> munion (sized l k) (sized r k)
     | Product (l, r) ->
         let ks =
           List.map (fun k0 -> (k0, k - k0)) (List.init (k + 1) Fun.id)
         in
         big_union
           (List.map (fun (k0, k1) -> mproduct (sized l k0) (sized r k1)) ks)
     | Pay s -> if k = 0 then { set = Empty; cardinal = 0 } else sized s (k - 1)
     | Map (f, s) -> sized s k |> mmap f

  let uniform_sized s k = uniform (sized s k)

  type nat = Z | S of nat

  let pp_nat n =
    let rec aux i = function Z -> Int.to_string i | S n -> aux (i + 1) n in
    aux 0 n

  let s n = S n
  let rec sp_nat : nat space = Pay (Sum (Pure Z, Map (s, sp_nat)))

  let rec sp_list (sp : 'a space) : 'a list space =
    let cons (n, ns) = n :: ns in
    Pay (Sum (Pure [], Map (cons, Product (sp, sp_list sp))))

  let sp_nat_list = sp_list sp_nat
end

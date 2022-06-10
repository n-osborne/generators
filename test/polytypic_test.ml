open QCheck
open Polytypic

(* setup *)

(** a model for the list type constructor *)
module MList = struct
  type 'a t = 'a list

  let polynome = FSum (FT, FProd (FPar, FRec))
  let rec inn = function [] -> T | x :: xs -> Prod (Par x, Rec (inn xs))

  let rec out = function
    | T -> []
    | Rec m -> out m
    | Prod (Par x, m) -> x :: out m
    | _ -> failwith "this is not a list"
end

module PList = Make (MList)

(* let's declare a type constructor tree *)
type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree

(* the model for the tree type constructor *)
module MTree = struct
  type 'a t = 'a tree

  let polynome = FSum (FPar, FProd (FPar, FProd (FRec, FRec)))

  let rec inn = function
    | Leaf a -> Par a
    | Node (a, l, r) -> Prod (Par a, Prod (Rec (inn l), Rec (inn r)))

  let rec out = function
    | Par a -> Leaf a
    | Prod (Par a, Prod (Rec l, Rec r)) -> Node (a, out l, out r)
    | _ -> failwith "this is not a tree"
end

(* the expected behaviour of the functions *)
module Tree = struct
  let rec size = function Leaf _ -> 1 | Node (_, l, r) -> 1 + size l + size r

  let rec map f = function
    | Leaf a -> Leaf (f a)
    | Node (a, l, r) -> Node (f a, map f l, map f r)

  let rec fold_right f t acc =
    match t with
    | Leaf x -> f x acc
    | Node (x, l, r) -> fold_right f r acc |> fold_right f l |> f x

  let rec zip t0 t1 =
    let open Result in
    let ( >>= ) = bind in
    match (t0, t1) with
    | Leaf x, Leaf y -> ok (Leaf (x, y))
    | Node (x, l0, r0), Node (y, l1, r1) ->
        zip l0 l1 >>= fun l ->
        zip r0 r1 >>= fun r -> ok (Node ((x, y), l, r))
    | _, _ -> Result.error "not the same structure"

  let rec print pp = function
    | Leaf a -> "Leaf(" ^ pp a ^ ")"
    | Node (a, l, r) ->
        "Node(" ^ pp a ^ "," ^ print pp l ^ "," ^ print pp r ^ ")"
end

module PTree = Make (MTree)

(* a tree generator *)
let tree gen i =
  let rec aux i j =
    Gen.(
      frequency
        [
          (1, gen j >>= fun a -> return (Leaf a));
          ( i,
            gen j >>= fun a ->
            aux (i - 1) j >>= fun l ->
            aux (i - 1) j >>= fun r -> return (Node (a, l, r)) );
        ])
  in
  aux i i

(* a tree arbitrary with a printer *)
let tree gen pp i = make (tree gen i) ~print:(Tree.print pp)
let f c s = Char.escaped c ^ s

(* Tests *)

(* it is a good idea to test whether [inn] and [out] form an isomorphims *)

let list_iso =
  Test.make (list int)
    (fun l -> MList.out (MList.inn l) = l)
    ~name:"isomorphism between List and model" ~count:1000

let tree_iso =
  Test.make
    (tree (fun _ -> Gen.int) Print.int 10)
    (fun l -> MTree.out (MTree.inn l) = l)
    ~name:"isomorphism between Tree and model" ~count:1000

(* now let's test the polytypic function against the expected behaviour *)

let list_size =
  Test.make (list int)
    (fun l -> List.length l = PList.size l)
    ~name:"Derived list size is consistent with Stdlib.List.length" ~count:1000

let list_map =
  Test.make (list int)
    (fun l -> List.map succ l = PList.map succ l)
    ~name:"Derived list map is consistent with Stdlib.List.map" ~count:1000

let list_fold_right =
  Test.make (list char)
    (fun l -> List.fold_right f l "" = PList.fold_right f l "")
    ~name:"Derived list fold_right is consistent with Stdlib.List.fold_right"
    ~count:1000

let list_zip =
  Test.make
    (pair (list char) (list int))
    (fun (l, r) ->
      (try Result.ok (List.combine l r)
       with _ -> Result.error "not the same structure")
      = PList.zip l r)
    ~name:"Derived list zip is consistent with Stdlib.List.combine" ~count:1000

let tree_size =
  Test.make
    (tree (fun _ -> Gen.int) Print.int 10)
    (fun t -> Tree.size t = PTree.size t)
    ~name:"Derived tree size is consistent with Tree.size" ~count:1000

let tree_map =
  Test.make
    (tree (fun _ -> Gen.int) Print.int 10)
    (fun t -> Tree.map succ t = PTree.map succ t)
    ~name:"Derived tree map is consistent with Tree.map" ~count:1000

let tree_fold_right =
  Test.make
    (tree (fun _ -> Gen.char) Print.char 10)
    (fun l -> Tree.fold_right f l "" = PTree.fold_right f l "")
    ~name:"Derived tree fold_right is consistent with Tree.fold_right"
    ~count:1000

let tree_zip =
  Test.make
    (pair
       (tree (fun _ -> Gen.int) Print.int 10)
       (tree (fun _ -> Gen.char) Print.char 10))
    (fun (t0, t1) -> Tree.zip t0 t1 = PTree.zip t0 t1)
    ~name:"Derived tree zip is consistent with Tree.zip" ~count:1000

let _ =
  QCheck_runner.run_tests ~verbose:true
    [
      list_iso;
      tree_iso;
      list_size;
      list_map;
      list_fold_right;
      list_zip;
      tree_size;
      tree_map;
      tree_fold_right;
      tree_zip;
    ]

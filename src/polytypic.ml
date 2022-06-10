(* ******************************************* *)
(*      Polytypic programmation in OCaml       *)
(*           Nicolas Osborne                   *)
(* ******************************************* *)

type fpolynome =
  | FT
  | FPar
  | FRec
  | FProd of (fpolynome * fpolynome)
  | FSum of (fpolynome * fpolynome)

type 'a data =
  | T
  | Par of 'a
  | Rec of 'a data
  | Prod of ('a data * 'a data)
  | Sum of ('a data * 'a data)

(** A module of polytypic functions defined by recursion on a [data] *)
module PolyFun = struct
  let rec fold_right f m acc =
    match m with
    | T -> acc
    | Par x -> f x acc
    | Rec m -> fold_right f m acc
    | Prod (l, r) ->
        let acc = fold_right f r acc in
        fold_right f l acc
    | Sum (_, _) -> assert false

  let size m = fold_right (Fun.const succ) m 0

  let rec map f = function
    | T -> T
    | Par x -> Par (f x)
    | Rec m -> Rec (map f m)
    | Prod (l, r) -> Prod (map f l, map f r)
    | Sum (_, _) -> assert false

  let rec zip m n =
    let open Result in
    let ( >>= ) = bind in
    match (m, n) with
    | T, T -> ok T
    | Par x, Par y -> ok (Par (x, y))
    | Rec m, Rec n -> zip m n >>= ok
    | Prod (l, r), Prod (l', r') ->
        zip l l' >>= fun x ->
        zip r r' >>= fun y -> ok (Prod (x, y))
    | Sum (_, _), _ -> assert false
    | _, Sum (_, _) -> assert false
    | _, _ -> error "not the same structure"
end

module Generator = struct
  open QCheck

  let data polynome (gen : int -> 'a Gen.t) size : 'a data Gen.t =
    let open Gen in
    let rec aux polynome i j =
      match polynome with
      | FT -> return T
      | FPar -> map (fun a -> Par a) (gen j)
      | FRec -> aux polynome (i - 1) j >>= fun a -> return (Rec a)
      | FProd (l, r) ->
          aux l (i / 2) j >>= fun l' ->
          aux r (i / 2) j >>= fun r' -> return (Prod (l', r'))
      | FSum (l, r) -> frequency [ (1, aux l (i - 1) j); (i, aux r (i - 1) j) ]
    in
    aux polynome size size
end

module type Model = sig
  type 'a t
  (** The type [t] of the container *)

  val polynome : fpolynome

  val inn : 'a t -> 'a data
  (** [inn t] computes the data describing the data *)

  val out : 'a data -> 'a t
  (** [out t] computes back the data from the [data] *)
end

module Make (M : Model) : sig
  val fold_right : ('a -> 'b -> 'b) -> 'a M.t -> 'b -> 'b
  val size : 'a M.t -> int
  val map : ('a -> 'b) -> 'a M.t -> 'b M.t
  val zip : 'a M.t -> 'b M.t -> (('a * 'b) M.t, string) result
  val gen : (int -> 'a QCheck.Gen.t) -> int -> 'a M.t QCheck.Gen.t
end = struct
  let fold_right f x acc =
    let m = M.inn x in
    PolyFun.fold_right f m acc

  let size (x : 'a M.t) : int = M.inn x |> PolyFun.size

  let map (f : 'a -> 'b) (x : 'a M.t) : 'b M.t =
    M.inn x |> PolyFun.map f |> M.out

  let zip x y =
    let m = M.inn x in
    let n = M.inn y in
    Result.map M.out (PolyFun.zip m n)

  let gen g size =
    let open QCheck.Gen in
    Generator.data M.polynome g size >>= fun d -> return (M.out d)
end

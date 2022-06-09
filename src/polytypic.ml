(* ******************************************* *)
(*      Polytypic programmation in OCaml       *)
(*           Nicolas Osborne                   *)
(* ******************************************* *)

(** A [monome] describes a generic data *)
type 'a monome =
  | T
  | Par of 'a
  | Rec of 'a monome
  | Prod of ('a monome * 'a monome)

type 'a polynome = 'a monome list
(** A [polynome] describes an inductive data types parametric on one argument *)

(** A module of polytypic functions defined by recursion on a [monome] *)
module PolyFun = struct
  let rec fold_right f m acc =
    match m with
    | T -> acc
    | Par x -> f x acc
    | Rec m -> fold_right f m acc
    | Prod (l, r) ->
        let acc = fold_right f r acc in
        fold_right f l acc

  let size m = fold_right (Fun.const succ) m 0

  let rec map f = function
    | T -> T
    | Par x -> Par (f x)
    | Rec m -> Rec (map f m)
    | Prod (l, r) -> Prod (map f l, map f r)

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
    | _, _ -> error "not the same structure"
end

module type Model = sig
  type 'a t
  (** The type [t] of the container *)

  val inn : 'a t -> 'a monome
  (** [inn t] computes the monome describing the data *)

  val out : 'a monome -> 'a t
  (** [out t] computes back the data from the [monome] *)
end

module Make (M : Model) : sig
  val fold_right : ('a -> 'b -> 'b) -> 'a M.t -> 'b -> 'b
  val size : 'a M.t -> int
  val map : ('a -> 'b) -> 'a M.t -> 'b M.t
  val zip : 'a M.t -> 'b M.t -> (('a * 'b) M.t, string) result
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
end

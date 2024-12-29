let rec foldr_seq f xx b =
  Seq.uncons xx
  |> Option.fold ~none:b ~some:(fun (x, xx) -> foldr_seq f xx b |> f x)

let traverse_seq f xx =
  foldr_seq
    (fun f x -> Result.bind f (fun f -> Result.map f x))
    (Seq.map (Fun.compose (Result.map Seq.cons) f) xx)
    (Ok Seq.empty)

let sequence_seq xx = traverse_seq Fun.id xx

module Name : sig
  type t

  type e = Empty

  val make : string -> (t, e) result

  val equal : t -> t -> bool

  val to_string : t -> string
end = struct
  type t = string [@@deriving eq]

  type e = Empty

  let make s =
    let s = String.trim s in
    if String.equal s String.empty then Error Empty else Ok s

  let to_string = Fun.id
end

module Id : sig
  type t

  type e = Empty | Spaced

  val make : string -> (t, e) result

  val compare : t -> t -> int
end = struct
  type t = string

  type e = Empty | Spaced

  let make s =
    let ( let* ) = Result.bind in
    let* id =
      Name.make s |> Result.map_error (function Name.Empty -> Empty)
    in
    let id = Name.to_string id in
    if String.exists (fun x -> Uchar.of_char x |> Uucp.White.is_white_space) id
    then Error Spaced
    else Ok id

  let compare = String.compare
end

module IdMap = Map.Make (Id)

module Day : sig
  type t

  type e = OffRange of int

  val min : t

  val max : t

  val make : int -> (t, e) result

  val compare : t -> t -> int

  val values : t Seq.t

  val to_int : t -> int

  val to_string : t -> string
end = struct
  type t = int

  type e = OffRange of int

  let min = 1

  let max = 6

  let make x = if x < min || max < x then Error (OffRange x) else Ok x

  let compare = Int.compare

  let values = Seq.init (1 + max - min) (fun i -> min + i)

  let to_int = Fun.id

  let to_string n = [|"Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|].(n - min)
end

module Time : sig
  type t

  type e = OffRange of int

  val min : t

  val max : t

  val make : int -> (t, e) result

  val to_int : t -> int
end = struct
  type t = int

  type e = OffRange of int

  let min = 7

  let max = 22

  let make x = if x < min || max < x then Error (OffRange x) else Ok x

  let to_int = Fun.id
end

module Span : sig
  type t = private {lo: int; hi: int}

  type e = Empty

  val make : Time.t -> Time.t -> (t, e) result

  val disjoint : t -> t -> bool

  val inter : t -> t -> (t, e) Either.t

  val values : t -> int list
end = struct
  type t = {lo: int; hi: int}

  type e = Empty

  let make lo hi =
    let lo, hi = Time.(to_int lo, to_int hi) in
    if lo < hi then Ok {lo; hi} else Error Empty

  let disjoint a b = a.hi <= b.lo || b.hi <= a.lo

  let inter a b =
    let a, b = if b.lo < a.hi then (a, b) else (b, a) in
    let lo, hi =
      let bounds =
        let ( let* ) = Result.bind in
        let* lo = Time.make b.lo in
        let* hi = Time.make a.hi in
        Ok (lo, hi)
      in
      Result.get_ok bounds
    in
    make lo hi |> Result.fold ~ok:Either.left ~error:Either.right

  let values {lo; hi} = List.init (1 + hi - lo) (fun i -> lo + i)
end

module Times : sig
  type t

  type e = Collision

  val empty : t

  val of_seq : Span.t Seq.t -> (t, e) result

  val add : Span.t -> t -> (t, e) result

  val fuse : t -> t -> (t, e) result
end = struct
  type t = Span.t list

  type e = Collision

  let empty = []

  let add x xx =
    let split_by p xx =
      let map_fst f (a, b) = (f a, b) in
      let map_snd f (a, b) = (a, f b) in
      let f x g =
        List.cons x |> (if p x then map_fst else map_snd) |> Fun.compose g
      in
      List.fold_right f xx Fun.id ([], [])
    in
    let aa, bb = split_by Span.(fun a -> a.lo < x.lo) xx in
    if
      bb
      |> List.exists (fun a ->
             Span.inter a x
             |> Either.fold ~left:(Fun.const true) ~right:(Fun.const false) )
    then Error Collision
    else List.fold_left List.append [] [aa; [x]; bb] |> Result.ok

  let of_seq =
    let f acc x = Result.bind acc (add x) in
    Seq.fold_left f (Ok empty)

  let fuse xx =
    let f acc x = Result.bind acc (add x) in
    List.fold_left f (Ok xx)
end

module Schema = struct
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives

  type span = int * int [@@deriving yojson]

  type entry = {id: string; name: string; table: span array array}
  [@@deriving yojson]

  type t = entry array [@@deriving yojson]

  type e =
    | BadId of Id.e
    | BadName of Name.e
    | BadTime of Time.e
    | BadSpan of Span.e
    | TimesCollision of Times.e
    | NameCollision of string * string

  let to_courses schema =
    let ( let* ) = Result.bind in
    let make_pair {id; name; table} =
      let* id = Id.make id |> Result.map_error (fun e -> BadId e) in
      let* name = Name.make name |> Result.map_error (fun e -> BadName e) in
      let* time_table =
        let* bounds_matrix =
          Seq.init (Seq.length Day.values) (fun i ->
              try Array.to_seq table.(i) with Invalid_argument _ -> Seq.empty )
          |> traverse_seq
               (traverse_seq (fun (lo, hi) ->
                    let* lo = Time.make lo in
                    let* hi = Time.make hi in
                    Ok (lo, hi) ) )
          |> Result.map_error (fun e -> BadTime e)
        in
        let* span_matrix =
          bounds_matrix
          |> traverse_seq (traverse_seq (fun (lo, hi) -> Span.make lo hi))
          |> Result.map_error (fun e -> BadSpan e)
        in
        span_matrix |> traverse_seq Times.of_seq |> Result.map Array.of_seq
        |> Result.map_error (fun e -> TimesCollision e)
      in
      Ok (id, (name, time_table))
    in
    let add_pair acc x =
      let* acc = acc in
      let* id, ((name, time_table) as desc) = x in
      match IdMap.find id acc with
      | o_name, o_time_table ->
          if Name.equal o_name name then
            let* tss =
              Array.map2 Times.fuse time_table o_time_table
              |> Array.to_seq |> sequence_seq |> Result.map Array.of_seq
              |> Result.map_error (fun e -> TimesCollision e)
            in
            IdMap.add id (name, tss) acc |> Result.ok
          else
            NameCollision (Name.to_string o_name, Name.to_string name)
            |> Result.error
      | exception Not_found ->
          IdMap.add id desc acc |> Result.ok
    in
    Array.to_seq schema |> Seq.map make_pair
    |> Seq.fold_left add_pair (Ok IdMap.empty)
end

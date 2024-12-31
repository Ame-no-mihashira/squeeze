module ExSeq = struct
  let ( <$> ) = Seq.map

  let ( <*> ) f x = Result.bind f (fun f -> Result.map f x)

  let apply = ( <*> )

  let rec foldr f xx b =
    Seq.uncons xx
    |> Option.fold ~none:b ~some:(fun (x, xx) -> foldr f xx b |> f x)

  let traverse f xx =
    foldr ( <*> )
      (Seq.map (Fun.compose (Result.map Seq.cons) f) xx)
      (Ok Seq.empty)

  let sequence xx = traverse Fun.id xx
end

module Name : sig
  type t

  type e = Empty

  val make : string -> (t, e) Result.t

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

  val make : string -> (t, e) Result.t

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

  val make : int -> (t, e) Result.t

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

module DayMap = Map.Make (Day)

module Time : sig
  type t

  type e = OffRange of int

  val min : t

  val max : t

  val make : int -> (t, e) Result.t

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

  val make : Time.t -> Time.t -> (t, e) Result.t

  val disjoint : t -> t -> bool

  val inter : t -> t -> (t, e) Result.t

  val values : t -> int List.t
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
    make lo hi

  let values {lo; hi} = List.init (1 + hi - lo) (fun i -> lo + i)
end

module Times : sig
  type t

  type e = Collision

  val empty : t

  val of_seq : Span.t Seq.t -> (t, e) Result.t

  val add : Span.t -> t -> (t, e) Result.t

  val fuse : t -> t -> (t, e) Result.t
end = struct
  type t = Span.t List.t

  type e = Collision

  let empty = []

  let add x xx =
    let split_by p xx =
      let map_fst f (a, b) = (f a, b) in
      let map_snd f (a, b) = (a, f b) in
      let f x = List.cons x |> if p x then map_fst else map_snd in
      List.fold_right f xx ([], [])
    in
    let aa, bb = split_by Span.(fun a -> a.lo < x.lo) xx in
    if
      bb
      |> List.exists (fun a ->
             Span.inter a x
             |> Result.fold ~ok:(Fun.const true) ~error:(Fun.const false) )
    then Error Collision
    else List.fold_left List.append [] [aa; [x]; bb] |> Result.ok

  let of_seq xx = Seq.fold_left Result.bind (Ok empty) (Seq.map add xx)

  let fuse xx =
    let f acc x = Result.bind acc (add x) in
    List.fold_left f (Ok xx)
end

type sched = (Name.t * Times.t DayMap.t) IdMap.t

module Schema : sig
  type t

  type e =
    | BadId of Id.e
    | BadName of Name.e
    | BadDay of Day.e
    | BadTime of Time.e
    | BadSpan of Span.e
    | TimesCollision of Times.e
    | NameCollision of string * string

  val to_sched : t -> (sched, e) Result.t

  val t_of_sexp : Sexplib0.Sexp.t -> t

  val sexp_of_t : t -> Sexplib0.Sexp.t
end = struct
  open Sexplib.Std

  type bounds = int * int [@@deriving sexp]

  type entry = {id: string; name: string; table: bounds array array}
  [@@deriving sexp]

  type t = entry array [@@deriving sexp]

  type e =
    | BadId of Id.e
    | BadName of Name.e
    | BadDay of Day.e
    | BadTime of Time.e
    | BadSpan of Span.e
    | TimesCollision of Times.e
    | NameCollision of string * string

  let to_sched schema =
    let ( let* ) = Result.bind in
    let make_pair {id; name; table} =
      let* id = Id.make id |> Result.map_error (fun e -> BadId e) in
      let* name = Name.make name |> Result.map_error (fun e -> BadName e) in
      let* times =
        let* span_matrix =
          let span_of_bounds (lo, hi) =
            let time t = Time.make t |> Result.map_error (fun e -> BadTime e) in
            let* lo = time lo in
            let* hi = time hi in
            Span.make lo hi |> Result.map_error (fun e -> BadSpan e)
          in
          Seq.init (Seq.length Day.values) (fun i ->
              try Array.to_seq table.(i) with Invalid_argument _ -> Seq.empty )
          |> ExSeq.traverse (ExSeq.traverse span_of_bounds)
        in
        span_matrix
        |> ExSeq.traverse Times.of_seq
        |> Result.map_error (fun e -> TimesCollision e)
      in
      let* table =
        let* dtts =
          times
          |> Seq.mapi (fun i tt ->
                 let* d = Day.(i + to_int min |> make) in
                 Ok (d, tt) )
          |> ExSeq.sequence
          |> Result.map_error (fun e -> BadDay e)
        in
        Seq.fold_left Result.bind (Ok DayMap.empty)
          (Seq.map (fun (d, tt) acc -> DayMap.add d tt acc |> Result.ok) dtts)
      in
      Ok (id, (name, table))
    in
    let add_pair (id, (name, table)) acc =
      match IdMap.find_opt id acc with
      | None ->
          IdMap.add id (name, table) acc |> Result.ok
      | Some (o_name, o_table) ->
          if not (Name.equal o_name name) then
            NameCollision (Name.to_string o_name, Name.to_string name)
            |> Result.error
          else
            let f d o_tt table =
              let* table = table in
              let tt = DayMap.find d table in
              Times.fuse tt o_tt
              |> Result.map (fun tt ->
                     DayMap.update d (Fun.const (Some tt)) table )
            in
            DayMap.fold f o_table (Ok table)
            |> Result.map (fun table ->
                   IdMap.update id (Some (name, table) |> Fun.const) acc )
            |> Result.map_error (fun e -> TimesCollision e)
    in
    let* pairs = Array.to_seq schema |> ExSeq.traverse make_pair in
    pairs |> Seq.map add_pair |> Seq.fold_left Result.bind (Ok IdMap.empty)
end

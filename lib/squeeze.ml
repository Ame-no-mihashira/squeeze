open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module type YojsonDerived = sig
  type t

  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module type Debug = sig
  type t

  val dbg : t -> string
end

module Day : sig
  type t = Mon | Tue | Wed | Thu | Fri | Sat

  val values : t list

  exception BadDay of string

  val of_int : int -> t
  val to_int : t -> int

  include Debug with type t := t
end = struct
  type t = Mon | Tue | Wed | Thu | Fri | Sat [@@deriving enum]

  let dbg = function
    | Mon -> "Monday"
    | Tue -> "Tuesday"
    | Wed -> "Wednesday"
    | Thu -> "Thursday"
    | Fri -> "Friday"
    | Sat -> "Saturday"

  let values = List.init (1 + max) (fun i -> of_enum i |> Option.get)

  exception BadDay of string

  let of_int n =
    if min <= n && n < max then of_enum n |> Option.get
    else raise (BadDay "out of range")

  let to_int = to_enum
end

module Time : sig
  type t

  exception BadTime of string

  val of_int : int -> t
  val to_int : t -> int
  val compare : t -> t -> int

  include Debug with type t := t
end = struct
  type t = int

  let dbg = Printf.sprintf "%d"

  exception BadTime of string

  let of_int n = if 7 <= n && n < 22 then n else raise (BadTime "out of range")
  let to_int = Fun.id
  let compare = Int.compare
end

module Segment : sig
  type t = { day : Day.t; bot : Time.t; top : Time.t }

  exception BadSegment of string

  val of_parts : Day.t * Time.t * Time.t -> t

  include Debug with type t := t
end = struct
  type t = { day : Day.t; bot : Time.t; top : Time.t }

  let dbg { day; bot; top } =
    Printf.sprintf "%s %d:00 - %d:00" (Day.dbg day) (Time.to_int bot)
      (Time.to_int top)

  exception BadSegment of string

  let of_parts = function
    | day, bot, top ->
        if Time.compare bot top < 0 then { day; bot; top }
        else
          raise
            (BadSegment
               (Printf.sprintf "invalid segment: %d:00 - %d:00"
                  (Time.to_int bot) (Time.to_int top)))
end

module Ident : sig
  type t

  exception BadIdentifier of string

  val of_string : string -> t
  val to_string : t -> string

  include Debug with type t := t
end = struct
  type t = string

  let dbg = Fun.id

  exception BadIdentifier of string

  let of_string s =
    let s = String.trim s in
    if String.length s > 0 then s else raise (BadIdentifier "empty identifier")

  let to_string = Fun.id
end

module Id : sig
  type t

  exception BadId of string

  val of_string : string -> t
  val to_string : t -> string

  include Debug with type t := t
end = struct
  type t = string

  let dbg = Fun.id

  exception BadId of string

  let of_string s =
    let s = Ident.(of_string s |> to_string) in
    if String.exists (fun c -> Uchar.of_char c |> Uucp.White.is_white_space) s
    then raise (BadId "short code with spaces")
    else s

  let to_string = Fun.id
end

module Course : sig
  type t = { id : Id.t; name : Ident.t; segments : Segment.t list }

  val of_parts : string * string * Segment.t list -> t

  include Debug with type t := t
end = struct
  type t = { id : Id.t; name : Ident.t; segments : Segment.t list }

  let dbg { id; name; segments } =
    List.map Segment.dbg segments
    |> List.cons (Printf.sprintf "%s: %s" (Id.dbg id) (Ident.dbg name))
    |> String.concat "\n"

  let of_parts = function
    | id, name, segments ->
        let id = Id.of_string id in
        let name = Ident.of_string name in
        { id; name; segments }
end

module Schema : sig
  type t

  val to_courses : t -> Course.t list

  include Debug with type t := t
  include YojsonDerived with type t := t
end = struct
  type segment = { day : int; bot : int; top : int } [@@deriving yojson]

  type course = { id : string; name : string; segments : segment list }
  [@@deriving yojson]

  type t = course list [@@deriving yojson]

  let sdbg { day; bot; top } = Printf.sprintf "Day %d: %d - %d" day bot top

  let cdbg { id; name; segments } =
    List.map sdbg segments
    |> List.cons (Printf.sprintf "%s: %s" id name)
    |> String.concat "\n"

  let dbg x = List.map cdbg x |> String.concat "\n\n"

  let to_courses =
    let to_course { id; name; segments = ss } =
      let segments =
        let convert { day; bot; top } =
          let day = Day.of_int day in
          let bot = Time.of_int bot in
          let top = Time.of_int top in
          Segment.of_parts (day, bot, top)
        in
        List.map convert ss
      in
      Course.of_parts (id, name, segments)
    in
    List.map to_course
end

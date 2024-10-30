(* Module type signature for Point *)
module type Point = sig
  type t
  (** The type of a point. *)

  val create : float list -> t
  (** [create lst] creates a point with the coordinates in [lst]. Requires:
      [lst] is non-empty. *)

  val get_coordinate : t -> float list
  (** [get_coordinate p] is the coordinate of the point [p]. *)

  val to_string : t -> string
  (** [to_string p] is the string representation of [p]. Requires: [p] contains
      only digits. *)

  val euclidean_distance : t -> t -> float
  (** [euclidean_distance p1 p2] is the Euclidean distance between points [p1]
      and [p2]. Requires: [p1] and [p2] have the same dimensions. *)

  val manhattan_distance : t -> t -> float
  (** [manahattan_distance p1 p2] is the Manhattan distance between points [p1]
      and [p2]. Requires: [p1] and [p2] have the same dimensions. *)
end

(** Module containing helper methods for the Point implementation modules*)
module PointHelpers = struct
  let no_decimal_places f = int_of_float (f *. 10.) mod 10 = 0

  let str f =
    if no_decimal_places f then string_of_float f ^ "0" else string_of_float f
end

(* AF: A 1D point represented as a list containing a single float, where the
   value is the point's position on a number line. Ex.: [(3.0)] is represented
   by [[3.0]] *)
(* RI: The list passed for creation of a point must contain exactly one element.
   The coordinate of t must be finite. *)

module Point1D : Point = struct
  include PointHelpers

  type t = float

  let create p =
    match p with
    | [ x ] -> x
    | _ -> failwith "1D"

  let get_coordinate (p : t) = [ p ]
  let euclidean_distance (p1 : t) (p2 : t) : float = abs_float (p1 -. p2)
  let to_string p = Printf.sprintf "(%s)" (str p)
  let manhattan_distance (p1 : t) (p2 : t) : float = abs_float (p1 -. p2)
end

(* AF: A 2D point represented as a list containing a pair of floats (x, y),
   where x and y are the point's coordinates in the Cartesian plane. Ex.: [(3.0,
   4.0)] is represented by [[3.0; 4.0]] *)
(* RI: The list passed for creation of a point must contain exactly two
   elements. Both x and y must be finite. *)

module Point2D : Point = struct
  include PointHelpers

  type t = float * float

  let create lst =
    match lst with
    | [ x; y ] -> (x, y)
    | _ -> failwith "2D"

  let get_coordinate (x, y) = [ x; y ]

  let euclidean_distance (p1 : t) (p2 : t) : float =
    let x1, y1 = p1 in
    let x2, y2 = p2 in
    sqrt (((x2 -. x1) ** 2.0) +. ((y2 -. y1) ** 2.0))

  let to_string (x, y) = Printf.sprintf "(%s, %s)" (str x) (str y)

  let manhattan_distance (p1 : t) (p2 : t) : float =
    let x1, y1 = p1 in
    let x2, y2 = p2 in
    abs_float (x2 -. x1) +. abs_float (y2 -. y1)
end

(* Point3D Module *)
(* AF: A 3D point represented as a list containing a triple of floats (x, y, z),
   where x, y, and z are the point's coordinates in the 3D space. Ex.: [(3.0,
   4.0, 5.0)] is represented by [[3.0; 4.0; 5.0]] *)
(* RI: The list passed for creation of a point must contain exactly three
   elements. All of x, y, and z must be finite. *)

module Point3D : Point = struct
  include PointHelpers

  type t = float * float * float

  let create lst =
    match lst with
    | [ x; y; z ] -> (x, y, z)
    | _ -> failwith "3D"

  let get_coordinate (x, y, z) = [ x; y; z ]

  let euclidean_distance (p1 : t) (p2 : t) : float =
    let x1, y1, z1 = p1 in
    let x2, y2, z2 = p2 in
    sqrt (((x2 -. x1) ** 2.0) +. ((y2 -. y1) ** 2.0) +. ((z2 -. z1) ** 2.0))

  let to_string (x, y, z) =
    Printf.sprintf "(%s, %s, %s)" (str x) (str y) (str z)

  let manhattan_distance (p1 : t) (p2 : t) : float =
    let x1, y1, z1 = p1 in
    let x2, y2, z2 = p2 in
    abs_float (x2 -. x1) +. abs_float (y2 -. y1) +. abs_float (z2 -. z1)
end

(* Module type signature for Point *)
module type Point = sig
  type t
  (** The type of a point. *)

  val create : float list -> t
  (** [create lst] creates a point with the coordinates in [lst]. *)

  val get_coordinate : t -> float list
  (** [get_coordinate p] is the coordinate of the point [p]. *)

  val euclidean_distance : t -> t -> float
  (** [euclidean_distance p1 p2] is the Euclidean distance between points [p1]
      and [p2]. *)

  val manahattan_distance : t -> t -> float
  (** [manahattan_distance p1 p2] is the Manhattan distance between points [p1]
      and [p2]. *)
end

(* Implementation for 1 Dimensional Point *)
module Point1D : Point = struct
  type t = float

  let create p =
    match p with
    | [ x ] -> x
    | _ -> failwith "1D"

  let get_coordinate (p : t) = [ p ]
  let euclidean_distance (p1 : t) (p2 : t) : float = abs_float (p1 -. p2)
  let manahattan_distance (p1 : t) (p2 : t) : float = abs_float (p1 -. p2)
end

module Point2D : Point = struct
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

  let manahattan_distance (p1 : t) (p2 : t) : float =
    let x1, y1 = p1 in
    let x2, y2 = p2 in
    abs_float (x2 -. x1) +. abs_float (y2 -. y1)
end

module Point3D : Point = struct
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

  let manahattan_distance (p1 : t) (p2 : t) : float =
    let x1, y1, z1 = p1 in
    let x2, y2, z2 = p2 in
    abs_float (x2 -. x1) +. abs_float (y2 -. y1) +. abs_float (z2 -. z1)
end

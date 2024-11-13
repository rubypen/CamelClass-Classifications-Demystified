(** Module containing helper methods for the Point implementation modules *)
module PointHelpers = struct
  let no_decimal_places f = int_of_float (f *. 10.) mod 10 = 0

  let str f =
    if no_decimal_places f then string_of_float f ^ "0" else string_of_float f
end

include PointHelpers

type t = {
  size : int;
  coordinates : float list;
}

(* AF: the reference [{n; [a1, a2, ..., an]}] represents a point in n
   dimensional space located in a point in space where we have x1, x2, ... xn
   axis and the following condition is satisfied [x1=a1 && x2=a2 && ... xn=an].
   Ex.: a point with coordinates [x=3.0, y=4.0, z=5.0] is represented by [3;
   [3.0, 4.0, 5.0]] *)
(* RI: The list passed for creation of a point must contain exactly [n]
   elements. These elements must be finite floats. *)

let create size lst =
  if size != List.length lst then failwith "ND" else { size; coordinates = lst }

let get_coordinates (p : t) = p.coordinates

let euclidean_distance (p1 : t) (p2 : t) : float =
  let n = p1.size in
  let sum = ref 0.0 in
  for i = 0 to n - 1 do
    sum :=
      ((List.nth p1.coordinates i -. List.nth p2.coordinates i) ** 2.0) +. !sum
  done;
  let distance = sqrt !sum in
  distance

let to_string p =
  let lst = get_coordinates p in
  let str_of_lst = ref "(" in
  for i = 0 to List.length lst - 2 do
    str_of_lst := !str_of_lst ^ str (List.nth lst i) ^ ", "
  done;
  str_of_lst := !str_of_lst ^ str (List.nth lst (List.length lst - 1)) ^ ")";
  Printf.sprintf "%s" !str_of_lst

let manhattan_distance (p1 : t) (p2 : t) : float =
  let n = p1.size in
  let distance = ref 0.0 in
  for i = 0 to n - 1 do
    distance :=
      abs_float (List.nth p1.coordinates i -. List.nth p2.coordinates i)
      +. !distance
  done;
  !distance
=======
module Point1D : Point = struct
  include PointHelpers

<<<<<<< HEAD
  (* AF: The float [t] represents a point in 1 dimensional space located in a
     point in space where we have x axis and the following condition is
     satisfied [x=a&]. Ex.: point on x axis at value 3.0 is represented by
     [3.0] *)
  (* RI: The list passed for creation of a point must contain exactly one
     element. The coordinate of t must be finite float. *)
=======
  (* AF: The float [t] represents a point in 1 dimensional space located in a 
   point in space where we have x axis and the following condition is satisfied 
   [x=a]. Ex.: point on x axis at value 3.0 is represented by [3.0] *)
  (* RI: The list passed for creation of a point must contain exactly one element.
     The coordinate of t must be a finite float. *)
>>>>>>> main
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

module Point2D : Point = struct
  include PointHelpers

  (* AF: the tuple [{a,b}] represents a point in 2 dimensional space located in
     a point in space where we have x and y axis and the following condition is
     satisfied [x=a && y=b]. Ex.: a point with coordinates [x=3.0, y=4.0] is
     represented by [(3.0, 4.0)] *)
  (* RI: The list passed for creation of a point must contain exactly three
     elements. These elements must be finite floats. *)
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

module Point3D : Point = struct
  include PointHelpers

  (* AF: the tuple [{a,b,c}] represents a point in 3 dimensional space located
     in a point in space where we have x, y, and z axis and the following
     condition is satisfied [x=a && y=b && z=c]. Ex.: a point with coordinates
     [x=3.0, y=4.0, z=5.0] is represented by [(3.0, 4.0, 5.0)] *)
  (* RI: The list passed for creation of a point must contain exactly three
     elements. These elements must be finite floats. *)
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

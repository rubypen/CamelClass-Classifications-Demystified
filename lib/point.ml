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
   Ex.: a point with coordinates [x=3.0, y=4.0, z=5.0] is represented by [{3;
   [3.0, 4.0, 5.0]}] *)
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

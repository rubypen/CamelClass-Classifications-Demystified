type t
(** The type of a point. *)

val create : int -> float list -> t
(** [create n lst] creates a point with the number of coordinates [n] and the
    coordinates in [lst]. Requires: [lst] is non-empty. *)

val get_coordinates : t -> float list
(** [get_coordinates p] is the list of coordinates of the point [p]. *)

val to_string : t -> string
(** [to_string p] is the string representation of [p]. Requires: [p] contains
    only digits. *)

val euclidean_distance : t -> t -> float
(** [euclidean_distance p1 p2] is the Euclidean distance between points [p1] and
    [p2]. Requires: [p1] and [p2] have the same dimensions. *)

val manhattan_distance : t -> t -> float
(** [manahattan_distance p1 p2] is the Manhattan distance between points [p1]
    and [p2]. Requires: [p1] and [p2] have the same dimensions. *)

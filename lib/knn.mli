val sort_by_distance :
  Point.t -> (Point.t * string) list -> (Point.t * string) list
(** [sort_by_distance point points_list] is the list of labeled points
    [(p, label)] where [p] is the point and [label] is the classification it
    belongs to, in ascending order of their Euclidean distance to [point]. *)

val take : int -> 'a list -> 'a list
(** [take n lst] is a list containing the first [n] elements of the list [lst].
    If [n <= 0] or [lst] is empty, an empty list is returned. *)

val k_nearest_neighbors :
  int -> Point.t -> (Point.t * string) list -> (Point.t * string) list
(** [k_nearest_neighbors k point points_list] are the [k] closest labeled points
    to [point] based on Euclidean distance. Raises [Invalid_argument] if
    [k <= 0] or [labeled_points] is empty. *)

val classify : int -> Point.t -> (Point.t * string) list -> string
(** [classify k point points_list] is the label of [query_point] by considering
    the majority label among its [k] nearest neighbors. Raises
    [Invalid_argument] if [k <= 0] or [labeled_points] is empty. *)

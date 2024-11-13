open Point

val initialize_clusters : int -> Point.t list -> Point.t list
(** [initialize_clusters k points] randomly selects [k] points from [points] as
    initial centroids. Raise *)

val assign_points :
  Point.t list -> Point.t list -> (Point.t * Point.t list) list
(** [assign_points points clusters] assigns each point in [points] to the
    nearest cluster in [clusters]. *)

val update_centroids : (Point.t * Point.t list) list -> Point.t list
(** [update_centroids assignments] calculates the mean of the points in each
    cluster and returns the new centroids. *)

val has_converged : Point.t list -> Point.t list -> float -> bool
(** [has_converged old_clusters new_clusters threshold] checks if the centroids
    have changed position by less than threshold. *)

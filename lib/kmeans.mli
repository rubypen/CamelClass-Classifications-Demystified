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

val run_range_kmeans : Point.t list -> Point.t list list
(** [run_range_kmeans points] runs kmeans on [points] for a range of k values
    from 1-10. *)

val run_custom_kmeans : int -> Point.t list -> Point.t list
(** [run_custom_kmeans k points] runs kmeans on [points] for a custom k value
    [k]. *)

val total_variation : Point.t list -> Point.t list -> float
(** [total_variation points centroids] calculates the total varation from
    [points] to [centroids]. *)

val find_best_set : Point.t list list -> Point.t list -> Point.t list
(** [find_best_set clusters_sets points] is the set of centroids in
    [cluster_sets] with the least total variation from [points]. *)

val find_best_k : Point.t list list -> Point.t list -> int
(** [find_best_k clusters_sets points] is the value of k that produces the least
    variation from [points]. *)

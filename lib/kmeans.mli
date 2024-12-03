open Point

type distance_metric = Point.t -> Point.t -> float

val check_same_dimension : Point.t list -> bool
(** [check_same_dimension points] is whether all points in the list [points]
    have the same number of dimensions, or if the list is empty. *)

val initialize_clusters : int -> Point.t list -> Point.t list
(** [initialize_clusters k points] randomly selects [k] points from [points] as
    initial centroids. Raise *)

val assign_points :
  Point.t list ->
  Point.t list ->
  distance_metric ->
  (Point.t * Point.t list) list
(** [assign_points points clusters distance_fn] assigns each point in [points]
    to the nearest cluster in [clusters] using the provided distance metric
    [distance_fn]. *)

val update_centroids : (Point.t * Point.t list) list -> Point.t list
(** [update_centroids assignments] calculates the mean of the points in each
    cluster and returns the new centroids. *)

val has_converged :
  Point.t list -> Point.t list -> distance_metric -> float -> bool
(** [has_converged old_clusters new_clusters distance_fn threshold] checks if
    the centroids have changed position by less than threshold using the
    provided distance metric [distance_fn]. *)

val run_kmeans : int -> Point.t list -> distance_metric -> Point.t list
(** [run_kmeans k points distance_fn] performs the k-means clustering algorithm
    on the given list of points [points], with [k] classifications using the
    provided distance metric [distance_fn]. *)

val run_range_kmeans : Point.t list -> distance_metric -> Point.t list list
(** [run_range_kmeans points distance_fn] runs kmeans on [points] for a range of
    k values from 1-10 using the provided distance metric [distance_fn]. *)

val run_custom_kmeans : int -> Point.t list -> distance_metric -> Point.t list
(** [run_custom_kmeans k points distance_fn] runs kmeans on [points] for a
    custom k value [k] using the provided distance metric [distance_fn]. *)

val total_variation : Point.t list -> Point.t list -> distance_metric -> float
(** [total_variation points centroids distance_fn] calculates the total
    variation from [points] to [centroids] using the provided distance metric
    [distance_fn]. *)

val find_best_set :
  Point.t list list -> Point.t list -> distance_metric -> Point.t list
(** [find_best_set clusters_sets points distance_fn] is the set of centroids in
    [cluster_sets] with the least total variation from [points] using the
    provided distance metric [distance_fn]. *)

val find_best_k : Point.t list list -> Point.t list -> distance_metric -> int
(** [find_best_k clusters_sets points distance_fn] is the value of k that
    produces the least variation from [points] using the provided distance
    metric [distance_fn]. *)

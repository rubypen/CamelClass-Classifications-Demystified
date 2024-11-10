open Point

module type KMeans = sig
  type point
  type cluster

  val initialize_clusters : int -> point list -> cluster list
  (** [initialize_clusters k points] randomly selects [k] points from [points]
      as initial centroids. Raises [Invalid_argument] if k <= 0 or k > length of
      points. *)

  val assign_points : point list -> cluster list -> (cluster * point list) list
  (** [assign_points points clusters] assigns each point in [points] to the
      nearest cluster in [clusters]. Returns empty list for empty inputs. *)

  val update_centroids : (cluster * point list) list -> cluster list
  (** [update_centroids assignments] calculates the mean of the points in each
      cluster and returns the new centroids. Empty clusters retain their
      previous centroid. *)

  val has_converged : cluster list -> cluster list -> float -> bool
  (** [has_converged old_clusters new_clusters threshold] checks if the
      centroids have changed position by less than threshold. Raises
      [Invalid_argument] if cluster lists have different lengths. *)

  val kmeans :
    int ->
    point list ->
    float ->
    int ->
    cluster list * (cluster * point list) list
  (** [kmeans k points threshold max_iter] performs k-means clustering. Returns
      (final_centroids, final_assignments). Raises [Invalid_argument] if k <= 0
      or max_iter <= 0. *)
end

module MakeKMeans (P : sig
  type t = Point.t

  val euclidean_distance : t -> t -> float
  val get_coordinates : t -> float list
  val create : int -> float list -> t
end) : KMeans with type point = P.t and type cluster = P.t = struct
  type point = P.t
  type cluster = P.t

  let initialize_clusters k points =
    if k <= 0 then invalid_arg "k must be positive"
    else if k > List.length points then
      invalid_arg "k cannot be larger than number of points"
    else begin
      Random.self_init ();
      (* Use Fisher-Yates shuffle for unbiased sampling *)
      let arr = Array.of_list points in
      let n = Array.length arr in
      for i = n - 1 downto n - k do
        let j = Random.int (i + 1) in
        let temp = arr.(i) in
        arr.(i) <- arr.(j);
        arr.(j) <- temp
      done;
      Array.to_list (Array.sub arr (n - k) k)
    end

  let assign_points points clusters =
    if points = [] || clusters = [] then []
    else
      List.map
        (fun c ->
          let assigned_points =
            List.filter
              (fun p ->
                let dist_to_c = P.euclidean_distance p c in
                List.for_all
                  (fun other_c ->
                    if other_c == c then true
                    else dist_to_c <= P.euclidean_distance p other_c)
                  clusters)
              points
          in
          (c, assigned_points))
        clusters

  let update_centroids assignments =
    List.map
      (fun (cluster, points) ->
        match points with
        | [] -> cluster (* Keep old centroid for empty clusters *)
        | first :: rest ->
            let dim = List.length (P.get_coordinates first) in
            let zeros = List.init dim (fun _ -> 0.0) in
            let sum_coords =
              List.fold_left
                (fun acc p -> List.map2 ( +. ) acc (P.get_coordinates p))
                zeros points
            in
            let mean_coords =
              List.map
                (fun sum -> sum /. float_of_int (List.length points))
                sum_coords
            in
            P.create dim mean_coords)
      assignments

  let has_converged old_clusters new_clusters threshold =
    if threshold < 0.0 then invalid_arg "threshold must be non-negative"
    else if List.length old_clusters <> List.length new_clusters then
      invalid_arg "cluster lists must have same length"
    else
      List.for_all2
        (fun old_c new_c -> P.euclidean_distance old_c new_c < threshold)
        old_clusters new_clusters

  let kmeans k points threshold max_iter =
    if k <= 0 then invalid_arg "k must be positive"
    else if max_iter <= 0 then invalid_arg "max_iter must be positive"
    else if points = [] then invalid_arg "points list cannot be empty"
    else
      let rec iterate clusters iter =
        if iter >= max_iter then (clusters, assign_points points clusters)
        else
          let assignments = assign_points points clusters in
          let new_clusters = update_centroids assignments in
          if has_converged clusters new_clusters threshold then
            (new_clusters, assignments)
          else iterate new_clusters (iter + 1)
      in
      let initial_clusters = initialize_clusters k points in
      iterate initial_clusters 0
end

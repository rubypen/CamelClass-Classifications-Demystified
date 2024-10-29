open Point

module type KMeans = sig
  type point
  type cluster

  val initialize_clusters : int -> point list -> cluster list
  (** [initialize_clusters k points] randomly takes [k] points in the [points]
      as initial centorids. *)

  val assign_points : point list -> cluster list -> (cluster * point list) list
  (** [assign_points points clusters] assigns each point in [points] to the
      nearest cluster from the [clusters]. *)

  val update_centroids : (cluster * point list) list -> cluster list
  (** [update_centroids assignments] calculates the mean of the points in each
      cluster and returns the new centroids of type [Point] in a list. *)

  val has_converged : cluster list -> cluster list -> float -> bool
  (** [has_converged old_clusters new_clusters threshold] checks if the
      centroids have changed their position by a value larger than the threshold
      in euclidean distance between old iteration [old_clusters] and new
      iteration [new_clusters]. If, none of the centoids have moved by that
      much, the function returns [true] *)
end

module MakeKMeans (P : Point) :
  KMeans with type point = P.t and type cluster = P.t = struct
  type point = P.t
  type cluster = P.t

  let initialize_clusters k points =
    Random.self_init ();
    let n = List.length points in
    let rec pick_random_points k selected_points =
      if k = 0 then selected_points
      else
        let rand_index = Random.int n in
        let point = List.nth points rand_index in
        if List.mem point selected_points then
          pick_random_points k selected_points
        else pick_random_points (k - 1) (point :: selected_points)
    in
    pick_random_points k []

  let assign_points points clusters =
    List.map
      (fun c ->
        let assigned_points =
          List.filter
            (fun p ->
              List.for_all
                (fun other_c ->
                  P.euclidean_distance p c <= P.euclidean_distance p other_c)
                clusters)
            points
        in
        (c, assigned_points))
      clusters

  let update_centroids assignments =
    List.map
      (fun (cluster, points) ->
        let coord_lists = List.map P.get_coordinate points in
        let coord_sums =
          List.fold_left (List.map2 ( +. )) (List.hd coord_lists) coord_lists
        in
        let mean_coords =
          List.map (fun s -> s /. float_of_int (List.length points)) coord_sums
        in
        P.create mean_coords)
      assignments

  let has_converged old_clusters new_clusters threshold =
    List.for_all2
      (fun old_c new_c -> P.euclidean_distance old_c new_c < threshold)
      old_clusters new_clusters
end

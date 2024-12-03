open Point

let check_same_dimension points =
  match points with
  | [] -> true
  | first :: rest ->
      let dim = List.length (get_coordinates first) in
      List.for_all (fun p -> List.length (get_coordinates p) = dim) rest

let initialize_clusters k points =
  if k <= 0 then invalid_arg "k must be positive"
  else if points = [] then invalid_arg "Points list is empty"
  else if k > List.length points then
    invalid_arg "k cannot be larger than number of points"
  else if not (check_same_dimension points) then
    invalid_arg "All points must have the same dimension"
  else begin
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

let assign_points points clusters distance_fn =
  if not (check_same_dimension points) then
    invalid_arg "All points must have the same dimension";
  List.map
    (fun c ->
      let assigned_points =
        List.filter
          (fun p ->
            List.for_all
              (fun other_c -> distance_fn p c <= distance_fn p other_c)
              clusters)
          points
      in
      (c, assigned_points))
    clusters

let update_centroids assignments =
  List.map
    (fun (cluster, points) ->
      if points = [] then cluster
      else if not (check_same_dimension (cluster :: points)) then
        invalid_arg "All points in cluster must have the same dimension"
      else
        let coord_lists = List.map get_coordinates points in
        let coord_sums =
          List.fold_left (List.map2 ( +. )) (List.hd coord_lists)
            (List.tl coord_lists)
        in
        let mean_coords =
          List.map (fun s -> s /. float_of_int (List.length points)) coord_sums
        in
        create (List.length mean_coords) mean_coords)
    assignments

let has_converged old_clusters new_clusters threshold =
  List.for_all2
    (fun old_c new_c -> euclidean_distance old_c new_c < threshold)
    old_clusters new_clusters

let run_kmeans k points distance_fn =
  if points = [] then invalid_arg "Points list cannot be empty."
  else if k > List.length points then
    invalid_arg "k cannot be larger than the number of points."
  else if not (check_same_dimension points) then
    invalid_arg "All points must have the same dimension."
  else
    let rec run old_clusters new_clusters i =
      let assignments = assign_points points new_clusters distance_fn in
      let centroid_updated = update_centroids assignments in
      if has_converged old_clusters centroid_updated 0.0001 then
        centroid_updated
      else run new_clusters centroid_updated (i + 1)
    in
    let clusters_initial = initialize_clusters k points in
    run clusters_initial clusters_initial 0

let run_range_kmeans points distance_fn =
  List.init 10 (fun x -> run_kmeans (x + 1) points distance_fn)

let run_custom_kmeans k points distance_fn = run_kmeans k points distance_fn

let total_variation points centroids distance_fn =
  let assignments = assign_points points centroids distance_fn in
  List.fold_left
    (fun total_variation (centroid, assigned_points) ->
      total_variation
      +. List.fold_left
           (fun acc point ->
             let dist = distance_fn point centroid in
             acc +. (dist *. dist))
           0.0 assigned_points)
    0.0 assignments

let find_best_set clusters_sets points distance_fn =
  let variations =
    List.map
      (fun centroids -> total_variation points centroids distance_fn)
      clusters_sets
  in
  let best_set_index =
    List.fold_left
      (fun (best_index, best_variation) variation ->
        if variation < best_variation then (best_index + 1, variation)
        else (best_index + 1, best_variation))
      (0, max_float) variations
  in
  List.nth clusters_sets (fst best_set_index - 1)

let find_best_k clusters_sets points distance_fn =
  let best_set = find_best_set clusters_sets points distance_fn in
  List.length best_set

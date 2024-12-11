open OUnit2
open GroupProject.Point
open GroupProject.Csvreader
open GroupProject.Kmeans
open GroupProject.Knn

(** [list_to_string lst] creates a string with the elements of [lst] separated
    by semicolon. *)
let rec list_to_string lst =
  match lst with
  | [] -> ""
  | [ h ] -> h
  | h :: t -> h ^ "; " ^ list_to_string t

let test_distance _ =
  (* 1D tests *)
  let p1 = GroupProject.Point.create 1 [ 3.0 ] in
  let p2 = GroupProject.Point.create 1 [ 7.5 ] in
  let dist_euc = GroupProject.Point.euclidean_distance p1 p2 in
  let dist_man = GroupProject.Point.manhattan_distance p1 p2 in
  assert_equal [ 3.0 ] (GroupProject.Point.get_coordinates p1)
    ~printer:(fun x -> "[" ^ list_to_string (List.map string_of_float x) ^ "]");
  assert_equal "(3.0)" (GroupProject.Point.to_string p1) ~printer:(fun x -> x);
  assert_equal dist_euc 4.5 ~printer:string_of_float;
  assert_equal dist_man 4.5 ~printer:string_of_float;

  (* 2D tests *)
  let p1 = GroupProject.Point.create 2 [ 1.0; 2.0 ] in
  let p2 = GroupProject.Point.create 2 [ 4.0; 6.0 ] in
  let dist_euc = GroupProject.Point.euclidean_distance p1 p2 in
  let dist_man = GroupProject.Point.manhattan_distance p1 p2 in
  assert_equal [ 1.0; 2.0 ] (GroupProject.Point.get_coordinates p1)
    ~printer:(fun x -> "[" ^ list_to_string (List.map string_of_float x) ^ "]");
  assert_equal "(1.0, 2.0)" (GroupProject.Point.to_string p1) ~printer:(fun x ->
      x);
  assert_equal dist_euc 5.0 ~printer:string_of_float;
  assert_equal dist_man 7.0 ~printer:string_of_float;

  (* 3D tests *)
  let p1 = GroupProject.Point.create 3 [ 6.0; 15.0; 7.0 ] in
  let p2 = GroupProject.Point.create 3 [ 2.0; 3.0; 1.0 ] in
  let dist_euc = GroupProject.Point.euclidean_distance p1 p2 in
  let dist_man = GroupProject.Point.manhattan_distance p1 p2 in
  assert_equal [ 6.0; 15.0; 7.0 ] (GroupProject.Point.get_coordinates p1)
    ~printer:(fun x -> "[" ^ list_to_string (List.map string_of_float x) ^ "]");
  assert_equal "(6.0, 15.0, 7.0)" (GroupProject.Point.to_string p1)
    ~printer:(fun x -> x);
  assert_equal dist_euc 14.0 ~printer:string_of_float;
  assert_equal dist_man 22.0 ~printer:string_of_float

let test_read_points _ =
  let points = CsvReaderImpl.read_points 1 "../data/test_data.csv" in
  assert_equal (List.length points) 3;
  assert_equal
    [
      GroupProject.Point.create 1 [ 5.4 ];
      GroupProject.Point.create 1 [ 3.7 ];
      GroupProject.Point.create 1 [ 1.1 ];
    ]
    points
    ~printer:(fun x ->
      "[" ^ list_to_string (List.map GroupProject.Point.to_string x) ^ "]");
  let points = CsvReaderImpl.read_points 2 "../data/test_data_2d.csv" in
  assert_equal (List.length points) 2;
  assert_equal
    [
      GroupProject.Point.create 2 [ 3.2; 2.5 ];
      GroupProject.Point.create 2 [ 4.6; 7.1 ];
    ]
    points
    ~printer:(fun x ->
      "[" ^ list_to_string (List.map GroupProject.Point.to_string x) ^ "]");
  let points = CsvReaderImpl.read_points 3 "../data/test_data_3d.csv" in
  assert_equal (List.length points) 9;
  assert_equal
    [
      GroupProject.Point.create 3 [ 1.0; 1.0; 1.0 ];
      GroupProject.Point.create 3 [ 2.0; 2.0; 2.0 ];
      GroupProject.Point.create 3 [ 3.0; 3.0; 3.0 ];
      GroupProject.Point.create 3 [ 4.0; 4.0; 4.0 ];
      GroupProject.Point.create 3 [ 5.0; 5.0; 5.0 ];
      GroupProject.Point.create 3 [ 6.0; 6.0; 6.0 ];
      GroupProject.Point.create 3 [ 7.0; 7.0; 7.0 ];
      GroupProject.Point.create 3 [ 8.0; 8.0; 8.0 ];
      GroupProject.Point.create 3 [ 9.0; 9.0; 9.0 ];
    ]
    points
    ~printer:(fun x ->
      "[" ^ list_to_string (List.map GroupProject.Point.to_string x) ^ "]")

let test_kmeans_initialization _ =
  let points =
    GroupProject.
      [
        Point.create 2 [ 1.0; 1.0 ];
        Point.create 2 [ 2.0; 2.0 ];
        Point.create 2 [ 3.0; 3.0 ];
        Point.create 2 [ 10.0; 10.0 ];
      ]
  in

  let clusters = initialize_clusters 2 points in
  assert_equal 2 (List.length clusters);

  assert_raises (Invalid_argument "k cannot be larger than number of points")
    (fun () -> initialize_clusters 5 points);

  let invalid_points =
    GroupProject.Point.create 3 [ 1.0; 1.0; 1.0 ] :: points
  in
  assert_raises (Invalid_argument "All points must have the same dimension")
    (fun () -> initialize_clusters 2 invalid_points)

let test_check_same_dimension _ =
  let points =
    GroupProject.Point.[ create 2 [ 1.0; 1.0 ]; create 2 [ 2.0; 2.0 ] ]
  in
  assert_bool "All points same dimension" (check_same_dimension points);
  let points_mismatch =
    GroupProject.Point.[ create 2 [ 1.0; 1.0 ]; create 1 [ 2.0 ] ]
  in
  assert_bool "Points with different dimensions"
    (not (check_same_dimension points_mismatch))

let test_initialize_clusters _ =
  let points =
    GroupProject.Point.
      [
        create 2 [ 1.0; 1.0 ];
        create 2 [ 2.0; 2.0 ];
        create 2 [ 3.0; 3.0 ];
        create 2 [ 10.0; 10.0 ];
      ]
  in
  let clusters = initialize_clusters 2 points in
  assert_equal 2 (List.length clusters);
  assert_raises (Invalid_argument "k cannot be larger than number of points")
    (fun () -> initialize_clusters 5 points)

let test_assign_points _ =
  let points =
    GroupProject.Point.
      [
        create 2 [ 1.0; 1.0 ];
        create 2 [ 2.0; 2.0 ];
        create 2 [ 9.0; 9.0 ];
        create 2 [ 10.0; 10.0 ];
      ]
  in
  let clusters = initialize_clusters 2 points in
  let assignments = assign_points points clusters euclidean_distance in
  assert_equal 2 (List.length assignments);
  let _, assigned_to_first = List.hd assignments in
  assert_equal 2 (List.length assigned_to_first)

let test_update_centroids _ =
  let points =
    [ create 2 [ 0.0; 0.0 ]; create 2 [ 1.0; 1.0 ]; create 2 [ 2.0; 2.0 ] ]
  in
  let assignments = [ (create 2 [ 0.0; 0.0 ], points) ] in
  let centroids = update_centroids assignments in
  assert_equal 1 (List.length centroids);
  let updated_centroid = List.hd centroids |> get_coordinates in
  assert_equal [ 1.0; 1.0 ] updated_centroid

let test_has_converged _ =
  let clusters1 = [ create 2 [ 0.0; 0.0 ]; create 2 [ 2.0; 2.0 ] ] in
  let clusters2 = [ create 2 [ 0.01; 0.01 ]; create 2 [ 2.0; 2.0 ] ] in
  let result = has_converged clusters1 clusters2 euclidean_distance 0.02 in
  assert_bool "Clusters have converged" result

let test_run_kmeans _ =
  let points =
    [
      create 2 [ 0.0; 0.0 ];
      create 2 [ 1.0; 1.0 ];
      create 2 [ 2.0; 2.0 ];
      create 2 [ 3.0; 3.0 ];
    ]
  in
  let clusters = run_kmeans 2 points euclidean_distance in
  assert_equal 2 (List.length clusters)

let test_total_variation _ =
  let points =
    [ create 2 [ 0.0; 0.0 ]; create 2 [ 1.0; 1.0 ]; create 2 [ 2.0; 2.0 ] ]
  in
  let centroids = [ create 2 [ 0.0; 0.0 ]; create 2 [ 2.0; 2.0 ] ] in
  let variation = total_variation points centroids euclidean_distance in
  assert_bool "Variation is positive" (variation > 0.0)

let test_find_best_set _ =
  let points =
    [
      create 2 [ 0.0; 0.0 ];
      create 2 [ 1.0; 1.0 ];
      create 2 [ 2.0; 2.0 ];
      create 2 [ 3.0; 3.0 ];
    ]
  in
  let clusters_sets =
    [
      run_kmeans 2 points euclidean_distance;
      run_kmeans 3 points euclidean_distance;
    ]
  in
  let best_set = find_best_set clusters_sets points euclidean_distance in
  assert_equal true (List.length best_set > 0)

let test_find_best_k _ =
  let points =
    [
      create 2 [ 0.0; 0.0 ];
      create 2 [ 1.0; 1.0 ];
      create 2 [ 2.0; 2.0 ];
      create 2 [ 3.0; 3.0 ];
    ]
  in
  let clusters_sets =
    List.map (fun k -> run_kmeans k points euclidean_distance) [ 2; 3; 4 ]
  in
  let best_k = find_best_k clusters_sets points euclidean_distance in
  assert_bool "Best k is positive" (best_k > 0)

let create_labeled_points points labels =
  List.map2 (fun p l -> (p, l)) points labels

let test_sort_by_distance _ =
  let query_point = create 2 [ 0.0; 0.0 ] in
  let points =
    [ create 2 [ 1.0; 1.0 ]; create 2 [ 2.0; 2.0 ]; create 2 [ 3.0; 3.0 ] ]
  in
  let labeled_points = create_labeled_points points [ "A"; "B"; "C" ] in
  let sorted_points = sort_by_distance query_point labeled_points in
  let actual_sorted_labels = List.map snd sorted_points in
  let expected_sorted_labels = [ "A"; "B"; "C" ] in

  assert_bool "Lengths differ"
    (List.length actual_sorted_labels = List.length expected_sorted_labels);

  List.iter2
    (fun actual expected -> assert_equal actual expected)
    actual_sorted_labels expected_sorted_labels

let test_k_nearest_neighbors _ =
  let query_point = create 2 [ 0.0; 0.0 ] in
  let points =
    [ create 2 [ 1.0; 1.0 ]; create 2 [ 2.0; 2.0 ]; create 2 [ 3.0; 3.0 ] ]
  in
  let labeled_points = create_labeled_points points [ "A"; "B"; "C" ] in
  let k = 2 in
  let neighbors = k_nearest_neighbors k query_point labeled_points in
  assert_equal k (List.length neighbors)

let test_classify _ =
  let points =
    [
      (create 2 [ 1.0; 1.0 ], "A");
      (create 2 [ 2.0; 2.0 ], "B");
      (create 2 [ 3.0; 3.0 ], "B");
      (create 2 [ 4.0; 4.0 ], "C");
    ]
  in
  let query_point = create 2 [ 0.0; 0.0 ] in

  let classification_k1 = classify 1 query_point points in
  assert_equal "A" classification_k1 ~msg:"k=1: Should classify as 'A'";

  let classification_k2 = classify 2 query_point points in
  assert_equal "A" classification_k2 ~msg:"k=2: Should classify as 'A'";

  let classification_k3 = classify 3 query_point points in
  assert_equal "B" classification_k3 ~msg:"k=3: Should classify as 'B'";

  let classification_k4 = classify 4 query_point points in
  assert_equal "B" classification_k4 ~msg:"k=4: Should classify as 'B'"

let rec test_cases =
  [
    ("Test Points" >:: fun _ -> test_distance ());
    ("Test CsvReader" >:: fun _ -> test_read_points ());
    ("Test Kmeans" >:: fun _ -> test_kmeans_initialization ());
    "test_check_same_dimension" >:: test_check_same_dimension;
    "test_initialize_clusters" >:: test_initialize_clusters;
    "test_assign_points" >:: test_assign_points;
    "test_update_centroids" >:: test_update_centroids;
    "test_has_converged" >:: test_has_converged;
    "test_run_kmeans" >:: test_run_kmeans;
    "test_total_variation" >:: test_total_variation;
    "test_find_best_set" >:: test_find_best_set;
    "test_find_best_k" >:: test_find_best_k;
    "test_sort_by_distance" >:: test_sort_by_distance;
    "test_k_nearest_neighbors" >:: test_k_nearest_neighbors;
    "test_classify" >:: test_classify;
  ]

let () = run_test_tt_main ("Point Tests" >::: test_cases)

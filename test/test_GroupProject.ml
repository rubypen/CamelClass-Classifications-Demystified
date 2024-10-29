open OUnit2
open GroupProject.Point
open GroupProject.Csvreader
open GroupProject.Kmeans

let test_distance _ =
  (* 1D tests*)
  let p1 = Point1D.create [ 3.0 ] in
  let p2 = Point1D.create [ 7.5 ] in
  let dist_euc = Point1D.euclidean_distance p1 p2 in
  let dist_man = Point1D.manahattan_distance p1 p2 in
  assert_equal dist_euc 4.5 ~printer:string_of_float;
  assert_equal dist_man 4.5 ~printer:string_of_float;

  (* 2D tests *)

  (* 2D test *)
  let p1 = Point2D.create [ 1.0; 2.0 ] in
  let p2 = Point2D.create [ 4.0; 6.0 ] in
  let dist_euc = Point2D.euclidean_distance p1 p2 in
  let dist_man = Point2D.manahattan_distance p1 p2 in
  assert_equal dist_euc 5.0 ~printer:string_of_float;
  assert_equal dist_man 7.0 ~printer:string_of_float;

  (* 3D test *)
  let p1 = Point3D.create [ 6.0; 15.0; 7.0 ] in
  let p2 = Point3D.create [ 2.0; 3.0; 1.0 ] in
  let dist_euc = Point3D.euclidean_distance p1 p2 in
  let dist_man = Point3D.manahattan_distance p1 p2 in
  assert_equal dist_euc 14.0 ~printer:string_of_float;
  assert_equal dist_man 22.0 ~printer:string_of_float

let test_read_points _ =
  let points = CsvReaderImpl.read_points_1d "../data/test_data.csv" in
  assert_equal (List.length points) 3;
  let points = CsvReaderImpl.read_points_2d "../data/test_data_2d.csv" in
  assert_equal (List.length points) 2;
  let points = CsvReaderImpl.read_points_3d "../data/test_data_3d.csv" in
  assert_equal (List.length points) 5

module KMeans1D = MakeKMeans (Point1D)

let test_kmeans_1d _ =
  let points =
    [ Point1D.create [ 1.0 ]; Point1D.create [ 2.0 ]; Point1D.create [ 9.0 ] ]
  in
  let clusters = KMeans1D.initialize_clusters 2 points in
  assert_equal (List.length clusters) 2

let rec test_cases =
  [
    ("Test Points" >:: fun _ -> test_distance ());
    ("Test CsvReader" >:: fun _ -> test_read_points ());
    ("Test KMeans 1D" >:: fun _ -> test_kmeans_1d ());
  ]

let () = run_test_tt_main ("Point Tests" >::: test_cases)

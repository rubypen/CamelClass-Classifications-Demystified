open OUnit2
open GroupProject.Point
open GroupProject.Csvreader

let test_euclidean_distance _ =
  (* 1D tests*)
  let p1 = Point1D.create [ 3.0 ] in
  let p2 = Point1D.create [ 7.5 ] in
  let dist = Point1D.euclidean_distance p1 p2 in
  assert_equal dist 4.5 ~printer:string_of_float;

  (* 2D test *)
  let p1 = Point2D.create [ 1.0; 2.0 ] in
  let p2 = Point2D.create [ 4.0; 6.0 ] in
  let dist = Point2D.euclidean_distance p1 p2 in
  assert_equal dist 5.0 ~printer:string_of_float;

  (* 3D test *)
  let p1 = Point3D.create [ 6.0; 15.0; 7.0 ] in
  let p2 = Point3D.create [ 2.0; 3.0; 1.0 ] in
  let dist = Point3D.euclidean_distance p1 p2 in
  assert_equal dist 14.0 ~printer:string_of_float

let test_read_points _ =
  let points = CsvReaderImpl.read_points "../data/test_data.csv" in
  assert_equal (List.length points) 3

let rec test_cases =
  [
    ("Test Points" >:: fun _ -> test_euclidean_distance ());
    ("Test CsvReader" >:: fun _ -> test_read_points ());
  ]

let () = run_test_tt_main ("Point Tests" >::: test_cases)

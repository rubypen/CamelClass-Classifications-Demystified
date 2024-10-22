open OUnit2
open GroupProject.Point
open GroupProject.Csvreader

let test_euclidean_distance _ =
  let p1 = Point1D.create 3.0 in
  let p2 = Point1D.create 7.5 in
  let dist = Point1D.euclidean_distance p1 p2 in
  assert_equal dist 4.5 ~printer:string_of_float

let test_read_points _ =
  let points = CsvReaderImpl.read_points "../data/test_data.csv" in
  assert_equal (List.length points) 3

let rec test_cases =
  [
    ("Test Point1D" >:: fun _ -> test_euclidean_distance ());
    ("Test CsvReader" >:: fun _ -> test_read_points ());
  ]

let () = run_test_tt_main ("priority queue tests" >::: test_cases)

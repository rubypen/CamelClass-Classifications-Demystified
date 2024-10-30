open OUnit2
open GroupProject.Point
open GroupProject.Csvreader

(** [list_to_string lst] creates a string with the elements of [lst] separated
    by semicolon. *)
let rec list_to_string lst =
  match lst with
  | [] -> ""
  | [ h ] -> h
  | h :: t -> h ^ "; " ^ list_to_string t

let test_distance _ =
  (* 1D tests *)
  let p1 = Point1D.create [ 3.0 ] in
  let p2 = Point1D.create [ 7.5 ] in
  let dist_euc = Point1D.euclidean_distance p1 p2 in
  let dist_man = Point1D.manhattan_distance p1 p2 in
  assert_equal [ 3.0 ] (Point1D.get_coordinate p1) ~printer:(fun x ->
      "[" ^ list_to_string (List.map string_of_float x) ^ "]");
  assert_equal "(3.0)" (Point1D.to_string p1) ~printer:(fun x -> x);
  assert_equal dist_euc 4.5 ~printer:string_of_float;
  assert_equal dist_man 4.5 ~printer:string_of_float;

  (* 2D tests *)
  let p1 = Point2D.create [ 1.0; 2.0 ] in
  let p2 = Point2D.create [ 4.0; 6.0 ] in
  let dist_euc = Point2D.euclidean_distance p1 p2 in
  let dist_man = Point2D.manhattan_distance p1 p2 in
  assert_equal [ 1.0; 2.0 ] (Point2D.get_coordinate p1) ~printer:(fun x ->
      "[" ^ list_to_string (List.map string_of_float x) ^ "]");
  assert_equal "(1.0, 2.0)" (Point2D.to_string p1) ~printer:(fun x -> x);
  assert_equal dist_euc 5.0 ~printer:string_of_float;
  assert_equal dist_man 7.0 ~printer:string_of_float;

  (* 3D tests *)
  let p1 = Point3D.create [ 6.0; 15.0; 7.0 ] in
  let p2 = Point3D.create [ 2.0; 3.0; 1.0 ] in
  let dist_euc = Point3D.euclidean_distance p1 p2 in
  let dist_man = Point3D.manhattan_distance p1 p2 in
  assert_equal [ 6.0; 15.0; 7.0 ] (Point3D.get_coordinate p1) ~printer:(fun x ->
      "[" ^ list_to_string (List.map string_of_float x) ^ "]");
  assert_equal "(6.0, 15.0, 7.0)" (Point3D.to_string p1) ~printer:(fun x -> x);
  assert_equal dist_euc 14.0 ~printer:string_of_float;
  assert_equal dist_man 22.0 ~printer:string_of_float

let test_read_points _ =
  let points = CsvReaderImpl.read_points_1d "../data/test_data.csv" in
  assert_equal (List.length points) 3;
  assert_equal
    [ Point1D.create [ 5.4 ]; Point1D.create [ 3.7 ]; Point1D.create [ 1.1 ] ]
    points
    ~printer:(fun x ->
      "[" ^ list_to_string (List.map PointD.to_string x) ^ "]");
  let points = CsvReaderImpl.read_points_2d "../data/test_data_2d.csv" in
  assert_equal (List.length points) 2;
  assert_equal
    [ Point2D.create [ 3.2; 2.5 ]; Point2D.create [ 4.6; 7.1 ] ]
    points
    ~printer:(fun x ->
      "[" ^ list_to_string (List.map Point2D.to_string x) ^ "]");
  let points = CsvReaderImpl.read_points_3d "../data/test_data_3d.csv" in
  assert_equal (List.length points) 5;
  assert_equal
    [
      Point3D.create [ 1.0; 1.0; 1.0 ];
      Point3D.create [ 2.0; 2.0; 2.0 ];
      Point3D.create [ 3.0; 3.0; 3.0 ];
      Point3D.create [ 4.0; 4.0; 4.0 ];
      Point3D.create [ 5.0; 5.0; 5.0 ];
    ]
    points
    ~printer:(fun x ->
      "[" ^ list_to_string (List.map Point3D.to_string x) ^ "]")

let rec test_cases =
  [
    ("Test Points" >:: fun _ -> test_distance ());
    ("Test CsvReader" >:: fun _ -> test_read_points ());
  ]

let () = run_test_tt_main ("Point Tests" >::: test_cases)

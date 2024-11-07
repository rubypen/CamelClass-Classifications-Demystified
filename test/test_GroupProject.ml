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
  assert_equal (List.length points) 5;
  assert_equal
    [
      GroupProject.Point.create 3 [ 1.0; 1.0; 1.0 ];
      GroupProject.Point.create 3 [ 2.0; 2.0; 2.0 ];
      GroupProject.Point.create 3 [ 3.0; 3.0; 3.0 ];
      GroupProject.Point.create 3 [ 4.0; 4.0; 4.0 ];
      GroupProject.Point.create 3 [ 5.0; 5.0; 5.0 ];
    ]
    points
    ~printer:(fun x ->
      "[" ^ list_to_string (List.map GroupProject.Point.to_string x) ^ "]")

let rec test_cases =
  [
    ("Test Points" >:: fun _ -> test_distance ());
    ("Test CsvReader" >:: fun _ -> test_read_points ());
  ]

let () = run_test_tt_main ("Point Tests" >::: test_cases)

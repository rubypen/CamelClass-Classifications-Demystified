open GroupProject.Point
open GroupProject.Csvreader

(** [is_csv c] is whether or not [c] is a csv file *)
let is_csv c =
  let len = String.length c in
  if len < 4 || String.sub c (len - 4) 4 <> ".csv" then begin
    Printf.printf "\nThis is not a valid csv file";
    false
  end
  else true

(** [is_dimension d] is whether or not [d] is a valid dimension *)
let is_dimension d =
  match String.uppercase_ascii d with
  | "1D" | "2D" | "3D" -> true
  | _ ->
      Printf.printf "\nThis is an invalid coordinate: Try [1D] [2D] or [3D]";
      false

(** [print_points p d] prints the points of *)
let print_points p d =
  match String.uppercase_ascii d with
  | "1D" -> begin
      let p_list =
        List.map Point1D.to_string (CsvReaderImpl.read_points_1d p)
      in
      List.iter (fun x -> Printf.printf "%s\n" x) p_list
    end
  | "2D" -> begin
      let p_list =
        List.map Point2D.to_string (CsvReaderImpl.read_points_2d p)
      in
      List.iter (fun x -> Printf.printf "%s\n" x) p_list
    end
  | "3D" -> begin
      let p_list =
        List.map Point3D.to_string (CsvReaderImpl.read_points_3d p)
      in
      List.iter (fun x -> Printf.printf "%s\n" x) p_list
    end
  | _ -> failwith "Bad Points CSV"

module MakeDummyPt (P : Point) = struct
  let dummy_pt dim =
    Printf.printf
      "\n\
       Now you will specify a point to calculate distances from each point in \
       your CSV file.\n";

    let prompt_coordinate name =
      Printf.printf "\nPlease specify the %s coordinate as a decimal number: "
        name;
      float_of_string (read_line ())
    in

    try
      let point =
        match String.uppercase_ascii dim with
        | "1D" -> P.create [ prompt_coordinate "X" ]
        | "2D" -> P.create [ prompt_coordinate "X"; prompt_coordinate "Y" ]
        | "3D" ->
            P.create
              [
                prompt_coordinate "X";
                prompt_coordinate "Y";
                prompt_coordinate "Z";
              ]
        | _ -> failwith "Invalid dimension"
      in
      point
    with Failure _ -> (
      Printf.printf "\nInvalid input: A default dummy input will be used\n\n";
      match String.uppercase_ascii dim with
      | "1D" -> P.create [ 1.0 ]
      | "2D" -> P.create [ 1.0; 2.0 ]
      | "3D" -> P.create [ 1.0; 2.0; 3.0 ]
      | _ -> failwith "Invalid dimension")
end

module Dummy1D = MakeDummyPt (Point1D)
module Dummy2D = MakeDummyPt (Point2D)
module Dummy3D = MakeDummyPt (Point3D)

(** [distances p_list dim] is the list of distance(s) between the points [p] in
    csv and a dummy point *)
let distances p dim dist_metric =
  match String.uppercase_ascii dim with
  | "1D" -> begin
      let p_list = CsvReaderImpl.read_points_1d p in
      let dp1 = Dummy1D.dummy_pt dim in
      if dist_metric = "euclidean" then
        List.iter
          (fun x ->
            Printf.printf "The euclidean distance between %s and %s is: %5f\n"
              (Point1D.to_string x) (Point1D.to_string dp1)
              (Point1D.euclidean_distance x dp1))
          p_list
      else if dist_metric = "manhattan" then
        List.iter
          (fun x ->
            Printf.printf "The manhattan distance between %s and %s is: %5f\n"
              (Point1D.to_string x) (Point1D.to_string dp1)
              (Point1D.manhattan_distance x dp1))
          p_list
    end
  | "2D" -> begin
      let p_list = CsvReaderImpl.read_points_2d p in
      let dp2 = Dummy2D.dummy_pt dim in
      if dist_metric = "euclidean" then
        List.iter
          (fun x ->
            Printf.printf "The euclidean distance between %s and %s is: %5f\n"
              (Point2D.to_string x) (Point2D.to_string dp2)
              (Point2D.euclidean_distance x dp2))
          p_list
      else if dist_metric = "manhattan" then
        List.iter
          (fun x ->
            Printf.printf "The manhattan distance between %s and %s is: %5f\n"
              (Point2D.to_string x) (Point2D.to_string dp2)
              (Point2D.manhattan_distance x dp2))
          p_list
    end
  | "3D" -> begin
      let p_list = CsvReaderImpl.read_points_3d p in
      let dp3 = Dummy3D.dummy_pt dim in
      if dist_metric = "euclidean" then
        List.iter
          (fun x ->
            Printf.printf "The euclidean distance between %s and %s is: %5f\n"
              (Point3D.to_string x) (Point3D.to_string dp3)
              (Point3D.euclidean_distance x dp3))
          p_list
      else if dist_metric = "manhattan" then
        List.iter
          (fun x ->
            Printf.printf "The manhattan distance between %s and %s is: %5f\n"
              (Point3D.to_string x) (Point3D.to_string dp3)
              (Point3D.manhattan_distance x dp3))
          p_list
    end
  | _ -> failwith "Bad Points CSV"

(** [print_distance] prints the distance(s) between all of the points i in i = 1
    ... n and a dummy point based on a distance metric the user chooses *)
let print_distances points dim =
  Printf.printf
    "What distance metric would you like to use: [Euclidean] or [Manhattan]>> ";
  let distance_metric = String.lowercase_ascii (read_line ()) in
  match distance_metric with
  | "euclidean" -> distances points dim "euclidean"
  | "manhattan" -> distances points dim "manhattan"
  | _ -> Printf.printf "The metric you have provided is invalid\n"

(** [analyze_args input len] is the Printf statement corresponding to different
    aspects of [input] *)
let analyze_args input len =
  if len = 1 then begin
    Printf.printf
      "You have not provided a csv file with points, so a default csv is being \
       used with the following points: ";
    print_points "./data/test_data_2d.csv" "2D";
    print_distances "./data/test_data_2d.csv" "2D"
  end
  else begin
    let arg1 = input.(1) in
    let arg2 = input.(2) in
    if is_csv arg1 && is_dimension arg2 then
      Printf.printf "The points in your csv are the following:\n ";
    print_points arg1 arg2;
    print_distances arg1 arg2
  end

(** [_] is the Printf statement corresponding to different aspects of user
    input. If a user provided a csv file, we print a user-friendly output
    describing the distance between all of the points i in i = 1 ... n in their
    file and a dummy point. If a csv is not provided, a default csv file is
    evaluated. *)
let _ =
  let num_of_args = 2 in
  let input = Sys.argv in
  let len = Array.length input in
  if len > num_of_args + 1 then
    Printf.printf "You have provided too many command-line arguments"
  else if len = 2 then
    Printf.printf
      "You have not provided enough arguments:\n\
       Hint: You might be missing the dimensional indicator: [1D] [2D] [3D]"
  else
    try analyze_args input len
    with Sys_error _ ->
      Printf.printf "\nMake sure your argument(s) are valid file path(s)"

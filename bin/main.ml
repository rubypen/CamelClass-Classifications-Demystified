open GroupProject.Csvreader
open GroupProject.Point

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
  try d > 0
  with _ ->
    Printf.printf
      "\n\
       This is an invalid coordinate: Try [1] [2] or  [N] where N is a \
       positive integer";
    false

(** [print_points file d] prints the points of dimension [d] in [file]. *)
let print_points file d =
  try
    let p_list = List.map to_string (CsvReaderImpl.read_points d file) in
    List.iter (fun x -> Printf.printf "%s\n" x) p_list
  with _ -> failwith "Bad Points CSV"

(* Create a dummy point based on user input or use default values *)

(** [dummy_pt dim] is a dummy point created by the user or a default dummy point
    if the user does not provide one. *)
let dummy_pt dim =
  Printf.printf
    "\n\
     Now you will specify a point to calculate distances from each point in \
     your CSV file.\n";
  let prompt_coordinate name =
    Printf.printf "\nPlease specify the %s coordinate as a number: " name;
    match read_line () with
    | input -> ( try float_of_string input with Failure _ -> 1.0)
    | exception End_of_file -> 1.0
  in
  let coords =
    List.init dim (fun i -> prompt_coordinate ("X" ^ string_of_int (i + 1)))
  in
  create dim coords

(** [distances p dim dist_metric] is the list of distance(s) between the points
    [p] in csv and a dummy point *)
let distances p dim dist_metric =
  let p_list = CsvReaderImpl.read_points dim p in
  let dp = dummy_pt dim in
  List.iter
    (fun p ->
      let distance =
        match dist_metric with
        | "euclidian" -> euclidean_distance p dp
        | "manhattan" -> manhattan_distance p dp
        | _ -> failwith "Invalid distance metric"
      in
      Printf.printf "The %s distance between %s and %s is: %5.2f\n" dist_metric
        (to_string p) (to_string dp) distance)
    p_list

(** [print_distance] prints the distance(s) between all of the points i in i = 1
    ... n and a dummy point based on a distance metric the user chooses *)

let print_distances points dim =
  Printf.printf
    "What distance metric would\n\
    \ you like to use: [Euclidian] or [Manhattan]>> ";
  let distance_metric = String.lowercase_ascii (read_line ()) in
  match distance_metric with
  | "euclidian" -> distances points dim "euclidian"
  | "manhattan" -> distances points dim "manhattan"
  | _ -> Printf.printf "The metric you have provided is invalid\n"

(** [analyze_args input len] is the Printf statement corresponding to different
    aspects of [input] *)
let analyze_args input len =
  if len = 1 then begin
    Printf.printf
      "You have not provided a csv file with points, so a\n\
      \ default csv is being used with the following points: \n";
    print_points "./data/test_data_2d.csv" 2;
    print_distances "./data/test_data_2d.csv" 2
  end
  else begin
    let arg1 = input.(1) in
    let arg2 = input.(2) in
    if is_csv arg1 && is_dimension (int_of_string arg2) then
      Printf.printf "The points in your csv are the following:\n ";
    print_points arg1 (int_of_string arg2);
    print_distances arg1 (int_of_string arg2)
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
    Printf.printf "You\n   have\n provided too many command-line arguments"
  else if len = 2 then
    Printf.printf
      "You have not provided enough arguments:\n\
      \  Hint: You might\n\
      \   be missing the\n\
      \  dimensional indicator: [1] [2] [...] [N] where N is a positive \
       integer."
  else
    try analyze_args input len
    with Sys_error _ ->
      Printf.printf "\nMake sure your\n   argument(s) are valid file\n path(s)"

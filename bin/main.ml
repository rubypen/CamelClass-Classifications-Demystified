open GroupProject.Point
open GroupProject.Csvreader
open GroupProject.Kmeans
open GroupProject.Extensions
open GMain

(** ============================= GUI FUNCTIONALITY ========================= *)

let initialize_gui () =
  (* Initialize the GUI *)
  let init = GMain.init () in
  ignore init;

  (* Create the window *)
  let w = GWindow.window ~title:"CamelClass" ~show:true () in

  (* Create a vertical box to organize layout *)
  let vbox = GPack.vbox ~packing:w#add () in

  (* Event handler for the button click *)
  let open_file () =
    (* Create a file chooser dialog *)
    let dialog =
      GWindow.file_chooser_dialog ~action:`OPEN ~title:"Select a File" ~parent:w
        ~position:`CENTER_ON_PARENT ()
    in

    (* Create buttons to open or cancel the file selection *)
    let _ = dialog#add_button_stock `OPEN `OPEN in
    let _ = dialog#add_button_stock `CANCEL `CANCEL in

    (* Run the file chooser *)
    let _ =
      match dialog#run () with
      | `OPEN -> (
          match dialog#filename with
          | Some file -> Printf.printf "Selected file: %s\n%!" file
          | None ->
              Printf.printf
                "No file selected. Please make sure you provide a file if you \
                 wish to proceed.\n\
                 %!")
      | `CANCEL | `DELETE_EVENT ->
          Printf.printf "You cancelled the file selection.\n%!"
    in
    dialog#destroy ()
  in

  (* Add text message to the window *)
  let _indent =
    GMisc.label ~markup:"<span size='50000'><b></b></span>" ~selectable:true
      ~yalign:0.0 ~height:50
      ~packing:(vbox#pack ~expand:true ~fill:true)
      ()
  in
  let _project_title =
    GMisc.label ~markup:"<span size='50000'><b>Project Title</b></span>"
      ~selectable:true ~yalign:0.0 ~height:50
      ~packing:(vbox#pack ~expand:true ~fill:true)
      ()
  in
  let _project_subtitle =
    GMisc.label ~markup:"<span size='25000'>K-means</span>" ~selectable:true
      ~yalign:0.5 ~height:50
      ~packing:(vbox#pack ~expand:true ~fill:true)
      ()
  in

  (* Add an alignment for the button *)
  let align =
    GBin.alignment ~xalign:0.5 ~yalign:0.5 ~height:50 ~width:100
      ~packing:vbox#pack ()
  in

  (* Add a button and connect the event handler *)
  (* let button = GButton.button ~label:"Open File" ~packing:vbox#pack () in
     ignore (button#connect#clicked ~callback:open_file); *)
  let start_button = GButton.button ~label:"Start" ~packing:align#add () in
  ignore (start_button#connect#clicked ~callback:open_file);
  (* Stop the program when the window is closed *)
  ignore
    (w#connect#destroy ~callback:(fun () ->
         GMain.quit ();
         exit 0));

  (* Show the GUI and start running it *)
  GMain.main ()

(** ======================= I/0 FUNCTIONALITY =============================== *)

(* !! ASK PROFS ABT. WHAT TYPE OF COMMENTS ARE BETTER FOR ORGANIZATION !! *)

(* MARK: - Properties (Data) *)
let default_files = Hashtbl.create 10;;

Hashtbl.add default_files "./data/test_data.csv" 1;;
Hashtbl.add default_files "./data/test_data_2d.csv" 2;;
Hashtbl.add default_files "./data/test_data_3d.csv" 3

(* MARK: - Properties (Utilities) *)

(** [show_progress_bar task] declares the [task] being working on, displays a
    progress bar filled to 100% after 1 second, and lastly declares a success
    message. *)
let show_progress_bar task =
  Printf.printf "Working on: %s...\n%!" task;
  Unix.sleep 1;
  Printf.printf "%s 100%%\n" (clr_ Bold Blue "[##################]");
  Printf.printf "Task '%s' completed successfully!\n\n" task

(** [print_help ()] prints a list of actions the user can perform with the
    points from their CSV file. *)
let print_help () =
  let msg = clr_ Und Cyan "\nCommands that may be used:\n" in
  let display = clr_ Bold Ylw "points" in
  let distances = clr_ Bold Ylw "dists" in
  let kmeans = clr_ Bold Ylw "kmeans" in
  let knn = clr_ Bold Ylw "knn" in
  let help = clr_ Bold Ylw "help" in
  let exit = clr_ Bold Ylw "exit" in
  Printf.printf "%s" msg;
  Printf.printf "- %s : View all points from the CSV file.\n" display;
  Printf.printf
    "- %s :Compute distances between points using a selected metric.\n"
    distances;
  Printf.printf "- %s : Perform k-means. \n" kmeans;
  Printf.printf "- %s : Perform k-nearest neighbors. \n" knn;
  Printf.printf "- %s : Display HELP message. \n" help;
  Printf.printf "- %s :  Exit the program. \n" exit

(* MARK: - Properties (Assurance) *)

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

(* ======================== Point Display Logic ============================= *)

(** [print_points file d] prints the points of dimension [d] in [file]. *)
let print_points file d =
  try
    let p_list = List.map to_string (CsvReaderImpl.read_points d file) in
    List.iter (fun x -> Printf.printf "%s\n" x) p_list
  with _ -> failwith "Bad Points CSV"

(* ================ Distance Calculation and Display Logic ================== *)

(** [dummy_pt dim] is a dummy point created by the user or a default dummy point
    if the user does not provide one. *)
let dummy_pt dim =
  Printf.printf
    "\n\
     Now you will specify a point to calculate distances from each point in \
     your CSV file.\n";
  let prompt_coordinate name =
    let prompt =
      clr_ Und Ylw "Please specify the %s coordinate as a number:" name
    in
    Printf.printf "\n%s " prompt;
    let input = read_line () in
    Printf.printf "\n";
    match input with
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
  let prompt =
    clr_ Reg Ylw
      "What distance metric would you like to use ([Euclidian] or \
       [Manhattan]): "
  in
  let err_msg = clr_ Bold Red "The metric you have provided is invalid\n" in
  Printf.printf "%s" prompt;
  let distance_metric = String.lowercase_ascii (read_line ()) in
  match distance_metric with
  | "euclidian" -> distances points dim "euclidian"
  | "manhattan" -> distances points dim "manhattan"
  | _ -> Printf.printf "%s" err_msg

(* ======================= Classification(s) UI Logic ======================= *)
let run_kmeans_ui csv dim = ()
let run_knn_ui csv dim = ()

(* ============================= Input Handler(s) =========================== *)

(** [prompt_for_csv_file ()] is the csv file the user provided if provided with
    points in a valid format, otherwise they are assigned a random csv file with
    points. *)
let prompt_for_csv_file () =
  let prompt_msg =
    clr_ Reg Cyan
      "\n\
       Please provide the path to your CSV file (or press Enter to use the \
       default file): "
  in
  let no_file_msg = clr_ Reg Ylw "No file provided. " in
  Printf.printf "%s%!" prompt_msg;
  match read_line () with
  | "" -> begin
      Random.self_init ();
      let rand_index = Random.int 3 + 1 in
      let default_file =
        match rand_index with
        | 1 -> "./data/test_data.csv"
        | 2 -> "./data/test_data_2d.csv"
        | _ -> "./data/test_data_3d.csv"
      in
      Printf.printf "%sUsing default file: %s\n\n" no_file_msg default_file;
      default_file
    end
  | file ->
      if is_csv file then begin
        Printf.printf "\n";
        show_progress_bar "Loading csv";
        file
      end
      else (
        Printf.printf "Invalid file type. Defaulting to the default file.\n";
        "./data/test_data_2d.csv")

(** [prompt_dimension csv] asks the user to provide what the dimension of their
    points are if [csv] isn't a default file. *)
let rec prompt_dimension csv =
  let prompt_msg =
    clr_ Reg Cyan
      "Enter the dimension of your points (i.e., [1] [2] ... [N], where N is a \
       positive integer): "
  in
  let invalid_dim_msg = clr_ Reg Ylw "Invalid input. " in
  if Hashtbl.mem default_files csv then begin
    let dim = Hashtbl.find default_files csv in
    let dim_msg =
      clr_ Reg Cyan "The points in %s have the following dimension: " csv
    in
    Printf.printf "%s %d\n" dim_msg dim;
    dim
  end
  else begin
    Printf.printf "%s" prompt_msg;
    let input = read_line () in
    match input with
    | dim -> (
        try
          let dim = int_of_string dim in
          if is_dimension dim then dim
          else (
            Printf.printf "%sPlease enter a valid dimension.\n\n"
              invalid_dim_msg;
            prompt_dimension csv)
        with _ ->
          Printf.printf "%sPlease enter a valid dimension.\n\n" invalid_dim_msg;
          prompt_dimension csv)
  end

(* ============================= Execution Logic =========================== *)

(** [command_handler file dim] is the handler of the program based on the user's
    input. *)
let rec command_handler file dim =
  Printf.printf "%s" (clr_ Bold Cyan "\nEnter a command ('help' for options): ");
  match String.lowercase_ascii (read_line ()) with
  | "points" ->
      print_points file dim;
      command_handler file dim
  | "dists" ->
      print_distances file dim;
      command_handler file dim
  | "kmeans" ->
      run_kmeans_ui file dim;
      command_handler file dim
  | "knn" ->
      run_knn_ui file dim;
      command_handler file dim
  | "help" ->
      print_help ();
      command_handler file dim
  | "exit" -> Printf.printf "\n%s" (clr_ Bold Grn "Exiting program. Goodbye!\n")
  | _ ->
      Printf.printf "%s" (clr_ Bold Red "Invalid command. Try again.\n");
      command_handler file dim

(** [run_io_mode ()] deals with program logic. *)
let run_io_mode () =
  let welcome_to_io_msg =
    clr_ Reg Grn "\nYou are now in CamelClass I/O mode !!"
  in
  Printf.printf "%s\n" welcome_to_io_msg;
  let csv_file = prompt_for_csv_file () in
  let dimension = prompt_dimension csv_file in
  command_handler csv_file dimension

(** Main Function *)
let () =
  let len = Array.length Sys.argv in
  let title = clr_ Bold Ylw "CamelClass: Classifications Demystified" in
  let debrief =
    "is a classification tool designed to simplify working with datasets in \
     OCaml."
  in
  let error_msg =
    clr_ Reg Red
      "Error: You have provided too many arguments. Try running something \
       like: "
  in
  let usage_msg = clr_ Reg Grn "$ dune exec bin/main.exe\n" in
  let invld_choice_msg = clr_ Reg Ylw "Invalid Input. " in
  try
    if len > 1 then Printf.printf "%s\n%s" error_msg usage_msg
    else begin
      Printf.printf "%s\n" welcome_ascii;
      Printf.printf "%s %s\n\n" title debrief;
      Printf.printf "%s"
        (clr_ Reg Cyan "Would you like to use [GUI] or [I/O] mode? ");
      let input = String.lowercase_ascii (read_line ()) in
      match input with
      | "gui" -> initialize_gui ()
      | "i/o" | "io" -> run_io_mode ()
      | _ ->
          Printf.printf "%sDefaulting to I/O mode.\n\n" invld_choice_msg;
          run_io_mode ()
    end
  with Sys_error _ ->
    Printf.printf "%s"
      (clr_ Bold Red
         "That was incorrect/invalid input. Please rerun the program and \
          provide valid prompts.")

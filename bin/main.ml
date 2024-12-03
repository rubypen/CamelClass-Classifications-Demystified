open GroupProject.Point
open GroupProject.Csvreader
open GroupProject.Kmeans
open GroupProject.Extensions
open GMain

(* -------------------------------------------------------------------------- *)
(* GUI FUNCTIONALITY *)
(* -------------------------------------------------------------------------- *)
let initialize_gui () =
  (* Initialize the GUI *)
  let init = GMain.init () in
  ignore init;

  (* Create the window *)
  let window = GWindow.window ~title:"CamelClass" ~show:true () in

  (* Set the window to full-screen after creation *)
  window#fullscreen ();

  (* Create main vertical box for layout *)
  let vbox = GPack.vbox ~packing:window#add () in

  (* Add title labels *)
  let _indent =
    GMisc.label ~markup:"<span size='50000'><b></b></span>" ~selectable:true
      ~yalign:0.0 ~height:50
      ~packing:(vbox#pack ~expand:true ~fill:true)
      ()
  in
  let _project_title =
    GMisc.label ~markup:"<span size='50000'><b>CamelClass</b></span>"
      ~selectable:true ~yalign:0.0 ~height:50
      ~packing:(vbox#pack ~expand:true ~fill:true)
      ()
  in
  let _project_subtitle =
    GMisc.label ~markup:"<span size='25000'>K-means Clustering</span>"
      ~selectable:true ~yalign:0.5 ~height:50
      ~packing:(vbox#pack ~expand:true ~fill:true)
      ()
  in

  (* Cleaning the existing window*)
  let clean (window : GWindow.window) =
    match window#children with
    | [] -> ()
    | children -> List.iter (fun widget -> widget#destroy ()) children
  in

  (* Create drawing area *)
  let drawing_area = GMisc.drawing_area ~packing:vbox#pack () in
  let () = drawing_area#misc#set_size_request ~width:600 ~height:400 () in

  (* Create controls area *)
  let controls_box = GPack.hbox ~packing:vbox#pack () in

  (* Start button *)
  let start_button =
    GButton.button ~label:"Start" ~packing:controls_box#pack ()
  in

  let radio_euclidean = ref false in

  (* Run button *)
  let run_button =
    GButton.button ~label:"Run K-means" ~packing:controls_box#pack ()
  in

  (* Text view for messages *)
  let scrolled_window =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:vbox#pack ()
  in
  let text_view = GText.view ~packing:scrolled_window#add () in
  let () = text_view#misc#set_size_request ~width:600 ~height:250 () in
  let buffer = text_view#buffer in

  (* Store current points and dimension *)
  let current_points = ref [] in
  let current_dim = ref 0 in
  let current_k = ref 2 in
  let current_metric = ref "Euclidean" in

  (* File selection handler *)
  let open_file () =
    let dialog =
      GWindow.file_chooser_dialog ~action:`OPEN ~title:"Select CSV File"
        ~parent:window ~position:`CENTER_ON_PARENT ()
    in

    dialog#add_button_stock `OPEN `OPEN;
    dialog#add_button_stock `CANCEL `CANCEL;

    let filter = GFile.filter ~name:"CSV Files" () in
    filter#add_pattern "*.csv";
    dialog#add_filter filter;

    let result = dialog#run () in
    let filename = dialog#filename in
    dialog#destroy ();

    match result with
    | `OPEN -> (
        match filename with
        | Some file -> (
            buffer#set_text ("Loading file: " ^ file ^ "\n");
            try
              let csv = Csv.load file in
              let first_line = List.hd csv in
              let dim = List.length first_line in
              current_dim := dim;
              current_points := CsvReaderImpl.read_points dim file;

              buffer#insert
                ("Successfully loaded "
                ^ string_of_int (List.length !current_points)
                ^ " points of dimension " ^ string_of_int dim ^ "\n\n"
                ^ "Sample points:\n");

              let rec show_n_points points n =
                match (points, n) with
                | [], _ -> ()
                | _, 0 -> ()
                | p :: ps, n ->
                    buffer#insert (GroupProject.Point.to_string p ^ "\n");
                    show_n_points ps (n - 1)
              in
              show_n_points !current_points 5;

              if dim <> 2 then
                buffer#insert
                  "\n\
                   Note: Points are not 2D. Visualization will not be available.\n";

              (*Transition 2*)
              clean window;
              let your_points_box = GPack.vbox ~packing:window#add () in
              let _indent =
                GMisc.label ~markup:"<span size='50000'><b></b></span>"
                  ~selectable:true ~yalign:0.0 ~height:50
                  ~packing:(your_points_box#pack ~expand:true ~fill:true)
                  ()
              in
              let _your_points_title =
                GMisc.label
                  ~markup:"<span size='50000'><b>Your Points: </b></span>"
                  ~selectable:true ~yalign:0.0 ~height:50
                  ~packing:(your_points_box#pack ~expand:true ~fill:true)
                  ()
              in

              (* TODO : add points graph*)

              (* Next button *)
              let next_button =
                GButton.button ~label:"Next" ~packing:your_points_box#pack ()
              in

              (* Transition 3*)
              let transition3 () =
                clean window;
                let controls_box = GPack.vbox ~packing:window#add () in

                (* k selection *)
                let _choose_k_title =
                  GMisc.label
                    ~markup:"<span size='50000'><b>Choose k value: </b></span>"
                    ~selectable:true ~yalign:0.0 ~height:50
                    ~packing:(controls_box#pack ~expand:true ~fill:true)
                    ()
                in
                let _ =
                  GMisc.label ~text:"K value: " ~packing:controls_box#pack ()
                in
                let k_adj =
                  GData.adjustment ~lower:2. ~upper:10. ~step_incr:1. ~value:2.
                    ()
                in
                let k_spin =
                  GEdit.spin_button ~adjustment:k_adj ~packing:controls_box#pack
                    ()
                in
                (* K-value change handler *)
                let on_k_changed () =
                  current_k := int_of_float k_adj#value;
                  buffer#insert
                    ("\nK value changed to: " ^ string_of_int !current_k ^ "\n")
                in
                window#misc#show_all ();
                ignore (k_spin#connect#value_changed ~callback:on_k_changed);

                (* Distance metric selection *)
                let _choose_distance_title =
                  GMisc.label
                    ~markup:
                      "<span size='50000'><b>Choose distance metric: \
                       </b></span>"
                    ~selectable:true ~yalign:0.0 ~height:50
                    ~packing:(controls_box#pack ~expand:true ~fill:true)
                    ()
                in
                let _ =
                  GMisc.label ~text:"Distance: " ~packing:controls_box#pack ()
                in
                let radio_euclidean_button =
                  GButton.radio_button ~label:"Euclidean"
                    ~packing:controls_box#pack ()
                in
                let radio_manhattan =
                  GButton.radio_button ~group:radio_euclidean_button#group
                    ~label:"Manhattan" ~packing:controls_box#pack ()
                in

                (* Distance metric change handler *)
                let on_metric_changed () =
                  current_metric :=
                    if radio_euclidean_button#active then "Euclidean"
                    else "Manhattan";
                  if radio_euclidean_button#active then radio_euclidean := true
                in
                ignore
                  (radio_euclidean_button#connect#clicked
                     ~callback:on_metric_changed);
                ignore
                  (radio_manhattan#connect#clicked ~callback:on_metric_changed);
                let next_button =
                  GButton.button ~label:"Next" ~packing:controls_box#pack ()
                in

                (* Transition 4 *)
                let transition4 () =
                  clean window;
                  let colors_box = GPack.vbox ~packing:window#add () in
                  let _choose_colors_title =
                    GMisc.label
                      ~markup:
                        "<span size='50000'><b>Choose k colors: </b></span>"
                      ~selectable:true ~yalign:0.0 ~height:50
                      ~packing:(colors_box#pack ~expand:true ~fill:true)
                      ()
                  in

                  (* Transition 5 *)
                  let transition5 () =
                    clean window;
                    let results_box = GPack.vbox ~packing:window#add () in
                    let _choose_colors_title =
                      GMisc.label
                        ~markup:
                          "<span size='50000'><b>Your results: </b></span>"
                        ~selectable:true ~yalign:0.0 ~height:50
                        ~packing:(results_box#pack ~expand:true ~fill:true)
                        ()
                    in

                    (* TODO : show the results of Kmeans *)
                    window#misc#show_all ()
                  in
                  let run_button =
                    GButton.button ~label:"Next" ~packing:colors_box#pack ()
                  in
                  ignore (run_button#connect#clicked ~callback:transition5)
                in
                window#misc#show_all ();
                ignore (next_button#connect#clicked ~callback:transition4)
              in

              ignore (next_button#connect#clicked ~callback:transition3);

              run_button#misc#set_sensitive true
            with e ->
              buffer#set_text
                ("Error reading file: " ^ Printexc.to_string e ^ "\n");
              run_button#misc#set_sensitive false)
        | None ->
            buffer#set_text "No file selected.\n";
            run_button#misc#set_sensitive false)
    | `CANCEL | `DELETE_EVENT ->
        buffer#set_text "File selection cancelled.\n";
        run_button#misc#set_sensitive false
  in
  let start () =
    (* Transition 1*)
    clean window;
    let controls_box = GPack.vbox ~packing:window#add () in
    (* File selection buttons *)
    let choose_file_button =
      GButton.button ~label:"Choose file" ~packing:controls_box#pack ()
    in
    let _sample_points_button =
      GButton.button ~label:"Sample points" ~packing:controls_box#pack ()
    in
    let _random_points_button =
      GButton.button ~label:"Random points" ~packing:controls_box#pack ()
    in
    let _page1 = GMisc.label ~text:"File selection page" () in
    window#misc#show_all ();
    ignore (choose_file_button#connect#clicked ~callback:open_file)
  in

  let get_cluster_color i =
    let colors =
      [|
        (1.0, 0.0, 0.0);
        (* Red *)
        (0.0, 0.0, 1.0);
        (* Blue *)
        (0.0, 0.8, 0.0);
        (* Green *)
        (1.0, 0.6, 0.0);
        (* Orange *)
        (0.8, 0.0, 0.8);
        (* Purple *)
        (0.0, 0.8, 0.8);
        (* Cyan *)
        (1.0, 0.0, 0.5);
        (* Pink *)
        (0.5, 0.5, 0.0);
        (* Olive *)
      |]
    in
    Array.get colors (i mod Array.length colors)
  in

  (* Add your existing generate_svg_visualization function here *)
  let generate_svg_visualization points clusters =
    let width = 600 in
    let height = 400 in

    (* Scale points helper function *)
    let scale_points points width height =
      let padding = 40 in
      let max_x = ref (-.max_float) in
      let max_y = ref (-.max_float) in
      let min_x = ref max_float in
      let min_y = ref max_float in

      (* Find bounds *)
      List.iter
        (fun p ->
          let coords = get_coordinates p in
          let x = List.nth coords 0 in
          let y = List.nth coords 1 in
          max_x := max !max_x x;
          max_y := max !max_y y;
          min_x := min !min_x x;
          min_y := min !min_y y)
        points;

      let scale_x =
        float_of_int (width - (2 * padding)) /. (!max_x -. !min_x)
      in
      let scale_y =
        float_of_int (height - (2 * padding)) /. (!max_y -. !min_y)
      in

      fun (x, y) ->
        let x_scaled = padding + int_of_float ((x -. !min_x) *. scale_x) in
        let y_scaled =
          height - padding - int_of_float ((y -. !min_y) *. scale_y)
        in
        (x_scaled, y_scaled)
    in

    (* Helper function for indexed fold *)
    let list_fold_lefti f init l =
      let rec fold i acc = function
        | [] -> acc
        | x :: xs -> fold (i + 1) (f acc i x) xs
      in
      fold 0 init l
    in

    (* Get the scaling function *)
    let scale = scale_points points width height in

    (* Create SVG header *)
    let svg =
      Printf.sprintf
        "<svg width='%d' height='%d' xmlns='http://www.w3.org/2000/svg'>\n"
        width height
    in

    (* Add white background *)
    let svg =
      svg
      ^ Printf.sprintf "<rect width='%d' height='%d' fill='white'/>\n" width
          height
    in

    (* Draw points for each cluster *)
    let svg =
      list_fold_lefti
        (fun acc i cluster_point ->
          let cluster_color = get_cluster_color i in
          let r, g, b = cluster_color in

          (* Get points in this cluster *)
          let cluster_points =
            List.filter
              (fun p ->
                let curr_dist =
                  if !radio_euclidean then euclidean_distance p cluster_point
                  else manhattan_distance p cluster_point
                in
                List.for_all
                  (fun other_cluster ->
                    curr_dist
                    <=
                    if !radio_euclidean then euclidean_distance p other_cluster
                    else manhattan_distance p other_cluster)
                  clusters)
              points
          in

          (* Draw each point in the cluster *)
          let cluster_svg =
            List.fold_left
              (fun acc p ->
                let coords = get_coordinates p in
                let x, y = scale (List.nth coords 0, List.nth coords 1) in
                acc
                ^ Printf.sprintf
                    "<circle cx='%d' cy='%d' r='4' fill='rgb(%d,%d,%d)'/>\n" x y
                    (int_of_float (r *. 255.))
                    (int_of_float (g *. 255.))
                    (int_of_float (b *. 255.)))
              "" cluster_points
          in

          (* Draw cluster center *)
          let center_coords = get_coordinates cluster_point in
          let cx, cy =
            scale (List.nth center_coords 0, List.nth center_coords 1)
          in
          let center_svg =
            Printf.sprintf "<circle cx='%d' cy='%d' r='6' fill='black'/>\n" cx
              cy
          in

          acc ^ cluster_svg ^ center_svg)
        svg clusters
    in

    (* Close SVG *)
    svg ^ "</svg>"
  in

  (* Run k-means handler *)
  let run_kmeans () =
    match !current_points with
    | [] -> buffer#insert "\nNo points loaded. Please select a file first.\n"
    | points -> (
        try
          let distance_fn =
            if !radio_euclidean then euclidean_distance else manhattan_distance
          in
          buffer#insert ("Using " ^ !current_metric ^ " distance metric.\n");
          let clusters = run_custom_kmeans !current_k points in
          buffer#insert "Clustering completed.\n";

          if !current_dim = 2 then begin
            let svg =
              generate_svg_visualization points (clusters distance_fn)
            in
            let oc = open_out "clustering.svg" in
            Printf.fprintf oc "%s" svg;
            close_out oc;
            buffer#insert "Visualization saved to 'clustering.svg'\n"
          end
          else
            buffer#insert "Points are not 2D - visualization not available.\n";

          List.iteri
            (fun i cluster ->
              buffer#insert
                ("Cluster "
                ^ string_of_int (i + 1)
                ^ " center: "
                ^ GroupProject.Point.to_string cluster
                ^ "\n"))
            (clusters distance_fn)
        with e ->
          buffer#insert
            ("\nError during clustering: " ^ Printexc.to_string e ^ "\n"))
  in

  (* Connect signals *)
  ignore (start_button#connect#clicked ~callback:start);
  ignore (run_button#connect#clicked ~callback:run_kmeans);
  ignore (window#connect#destroy ~callback:Main.quit);

  (* Initialize state *)
  run_button#misc#set_sensitive false;
  buffer#set_text
    "Welcome to CamelClass K-means Clustering\n\
     Please select a data file to begin.\n";

  (* Show the window *)
  window#show ();
  Main.main ()

(* -------------------------------------------------------------------------- *)
(* I/0 FUNCTIONALITY *)
(* -------------------------------------------------------------------------- *)

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
  let reload = clr_ Bold Ylw "reload" in
  Printf.printf "%s" msg;
  Printf.printf "- %s : View all points from the CSV file.\n" display;
  Printf.printf
    "- %s : Compute distances between points using a selected metric.\n"
    distances;
  Printf.printf "- %s : Perform k-means. \n" kmeans;
  Printf.printf "- %s : Perform k-nearest neighbors. \n" knn;
  Printf.printf "- %s :  Load a new CSV file.\n" reload;
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

(* -------------------------------------------------------------------------- *)
(* Point Display Logic *)
(* -------------------------------------------------------------------------- *)

(** [print_points file d] prints the points of dimension [d] in [file]. *)
let print_points file d =
  try
    let p_list = List.map to_string (CsvReaderImpl.read_points d file) in
    List.iter (fun x -> Printf.printf "%s\n" x) p_list
  with _ -> failwith "Bad Points CSV"

(* -------------------------------------------------------------------------- *)
(* Distance Calculation and Display Logic *)
(* -------------------------------------------------------------------------- *)

(** [dummy_pt dim] is a dummy point created by the user or a default dummy point
    if the user does not provide one. *)
let dummy_pt dim =
  let err_msg = clr_ Reg Red "Invalid Input." in
  Printf.printf
    "\n\
     Now you will specify a point to calculate distances from each point in \
     your CSV file.\n\n";
  let prompt_coordinate name =
    let prompt =
      clr_ Und Ylw "Please specify the %s coordinate as a number:" name
    in
    Printf.printf "%s " prompt;
    let input = read_line () in
    Printf.printf "\n";
    match input with
    | input -> (
        try float_of_string input
        with Failure _ ->
          Printf.printf "%s Defaulting %s coordinate to 1.0\n\n" err_msg name;
          1.0)
    | exception End_of_file ->
        Printf.printf "%s Defaulting %s coordinate to 1.0\n" err_msg name;
        1.0
  in
  let coords =
    List.init dim (fun i -> prompt_coordinate ("X" ^ string_of_int (i + 1)))
  in
  create dim coords

(** [distances p dim dist_metric] is the list of tuples with distance(s)
    calculated under [dist_metric] between the points [p] in csv and a dummy
    point *)
let distances p dim dist_metric =
  let p_list = CsvReaderImpl.read_points dim p in
  let dp = dummy_pt dim in
  List.map
    (fun p ->
      let distance =
        match dist_metric with
        | "euclidean" -> euclidean_distance p dp
        | "manhattan" -> manhattan_distance p dp
        | _ -> failwith "Invalid distance metric"
      in
      (to_string p, to_string dp, distance))
    p_list

(** [save_dists_to_csv distances metric choice] saves the distance information
    calculated under [metric] in the format of the user's [choice]. *)
let save_dists_to_csv distances metric choice =
  let file_name = Printf.sprintf "./data/distance_btw_points_%s.csv" metric in
  let csv_data =
    match choice with
    | "1" ->
        List.map
          (fun (p1, p2, dist) ->
            Printf.sprintf "The %s distance between %s and %s is: %f" metric p1
              p2 dist)
          distances
    | "2" ->
        List.map
          (fun (p1, p2, dist) -> Printf.sprintf "%s, %s, %f" p1 p2 dist)
          distances
    | _ ->
        Printf.printf "%s\n"
          (clr_ Bold Red "\nInvalid format selection. Not saving file.");
        []
  in
  if csv_data <> [] then (
    let oc = open_out file_name in
    List.iter (fun line -> Printf.fprintf oc "%s\n" line) csv_data;
    close_out oc;
    Printf.printf "\nData saved to %s\n" (clr_ Reg Grn "%s" file_name))

(** [ask_to_save_dists distances metric] prompts the user to choose whether or
    not to save the data they retrieved from the command dists. *)
let ask_to_save_dists distances metric =
  let msg =
    clr_ Bold Ylw
      "\nDo you want to save these distances to a CSV file? (yes/no): "
  in
  let choose_ = clr_ Und Ylw "\nChoose the format:\n\n" in
  let options =
    clr_ Reg Wht
      "1 - The [%s] distance between (p1, ..., pk) and (s1, ..., sj) is \
       [distance]\n\n\
       2 - (p1, ..., pk), (s1, ..., sj), distance\n\n"
      metric
  in
  let prompt = clr_ Und Ylw "Enter 1 or 2:" in
  Printf.printf "%s" msg;
  let input = String.lowercase_ascii (read_line ()) in
  match input with
  | "yes" ->
      Printf.printf "%s%s%s " choose_ options prompt;
      let format_choice = read_line () in
      save_dists_to_csv distances metric format_choice
  | "no" | _ -> ()

(** [print_distance] prints the distance(s) between all of the points i in i = 1
    ... n and a dummy point based on a distance metric the user chooses *)
let print_distances points dim =
  let prompt_metric =
    clr_ Reg Ylw
      "What distance metric would you like to use ([E: Euclidean] or [M: \
       Manhattan]): "
  in
  Printf.printf "%s" prompt_metric;
  let dist_metric = String.lowercase_ascii (read_line ()) in
  let distance_metric =
    if dist_metric = "e" then "euclidean"
    else if dist_metric = "m" then "manhattan"
    else "invalid"
  in
  match distance_metric with
  | "euclidean" | "manhattan" -> begin
      let distance_results = distances points dim distance_metric in
      let prompt_display =
        clr_ Bold Ylw "Would you like to see the distances? (yes/no): "
      in
      Printf.printf "%s" prompt_display;
      let input = String.lowercase_ascii (read_line ()) in
      if input = "yes" then
        List.iter
          (fun (p1, p2, dist) ->
            Printf.printf "The %s distance between %s and %s is: %5.2f\n"
              distance_metric p1 p2 dist)
          distance_results;
      ask_to_save_dists distance_results distance_metric
    end
  | _ ->
      Printf.printf "%s"
        (clr_ Bold Red "The metric you have provided is invalid. Try again.\n")

(* -------------------------------------------------------------------------- *)
(* Classifications UI Logic *)
(* -------------------------------------------------------------------------- *)
let run_kmeans_ui csv dim = ()
let run_knn_ui csv dim = ()

(* -------------------------------------------------------------------------- *)
(* Input Handlers *)
(* -------------------------------------------------------------------------- *)

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

(** [get_dimension_from_csv csv] is the dimension of points in [csv]. *)
let get_dimension_from_csv csv =
  try
    let csv_data = Csv.load csv in
    match csv_data with
    | row :: _ ->
        let dim = List.length row in
        if dim > 0 then Some dim else None
    | [] -> None
  with _ -> None

(** [prompt_dimension csv] asks the user to provide what the dimension of their
    points are if [csv] isn't a default file. *)
let rec prompt_dimension () =
  let csv = prompt_for_csv_file () in
  let clr_csv = clr_ Reg Grn "%s" csv in
  let inform_msg_p1 = clr_ Reg Cyan "The points in" in
  let inform_msg_p2 = clr_ Reg Cyan "have the following dimension: " in
  let inform_msg =
    Printf.sprintf "%s [%s] %s" inform_msg_p1 clr_csv inform_msg_p2
  in
  let err_msg =
    clr_ Bold Red
      "Failed to determine a valid dimension from [%s]. Please check the CSV \
       format and upload again."
      csv
  in
  match get_dimension_from_csv csv with
  | Some dim ->
      Printf.printf "%s%d\n" inform_msg dim;
      (csv, dim)
  | None ->
      Printf.printf "%s\n" err_msg;
      prompt_dimension ()

(* -------------------------------------------------------------------------- *)
(* Execution Logic *)
(* -------------------------------------------------------------------------- *)

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
  | "reload" ->
      let new_csv, new_dimension = prompt_dimension () in
      command_handler new_csv new_dimension
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
  let csv_file, dimension = prompt_dimension () in
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

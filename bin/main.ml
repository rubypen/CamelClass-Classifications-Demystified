open GroupProject.Point
open GroupProject.Csvreader
open GroupProject.Kmeans

(* GUI setup *)
open GMain
open Gtk

(* Initialize the GUI *)
let init = GMain.init ()

(* Create main window *)
let window =
  GWindow.window ~title:"K-means Clustering" ~width:800 ~height:600
    ~position:`CENTER ()

(* Create main vertical box for layout *)
let vbox = GPack.vbox ~packing:window#add ()

(* Create drawing area *)
let drawing_area = GMisc.drawing_area ~packing:vbox#pack ()
let () = drawing_area#misc#set_size_request ~width:600 ~height:400 ()

(* Create controls area *)
let controls_box = GPack.hbox ~packing:vbox#pack ()

(* File selection button *)
let file_button =
  GButton.button ~label:"Open File" ~packing:controls_box#pack ()

(* K selection *)
let k_box = GPack.hbox ~packing:controls_box#pack ()
let _ = GMisc.label ~text:"K value: " ~packing:k_box#pack ()
let k_adj = GData.adjustment ~lower:2. ~upper:10. ~step_incr:1. ~value:2. ()
let k_spin = GEdit.spin_button ~adjustment:k_adj ~packing:k_box#pack ()

(* Distance metric selection *)
let metric_box = GPack.hbox ~packing:controls_box#pack ()
let _ = GMisc.label ~text:"Distance: " ~packing:metric_box#pack ()

let radio_euclidean =
  GButton.radio_button ~label:"Euclidean" ~packing:metric_box#pack ()

let radio_manhattan =
  GButton.radio_button ~group:radio_euclidean#group ~label:"Manhattan"
    ~packing:metric_box#pack ()

(* Run button *)
let run_button =
  GButton.button ~label:"Run K-means" ~packing:controls_box#pack ()

(* Text view for messages *)
let scrolled_window =
  GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:vbox#pack ()

let text_view = GText.view ~packing:scrolled_window#add ()
let () = text_view#misc#set_size_request ~width:600 ~height:150 ()
let buffer = text_view#buffer

(* Store current points and dimension *)
let current_points = ref []
let current_dim = ref 0

(* State tracking *)
let current_k = ref 2
let current_metric = ref "Euclidean"

(* K-value change handler *)
let on_k_changed () =
  current_k := int_of_float k_adj#value;
  buffer#insert ("\nK value changed to: " ^ string_of_int !current_k ^ "\n")

(* Distance metric change handler *)
let on_metric_changed () =
  current_metric := if radio_euclidean#active then "Euclidean" else "Manhattan";
  buffer#insert ("\nDistance metric changed to: " ^ !current_metric ^ "\n")

(* File selection handler *)
let open_file () =
  let dialog =
    GWindow.file_chooser_dialog ~action:`OPEN ~title:"Select CSV File"
      ~parent:window ~position:`CENTER_ON_PARENT ()
  in

  (* Add buttons *)
  dialog#add_button_stock `OPEN `OPEN;
  dialog#add_button_stock `CANCEL `CANCEL;

  (* Add file filters *)
  let filter = GFile.filter ~name:"CSV Files" () in
  filter#add_pattern "*.csv";
  dialog#add_filter filter;

  (* Run dialog *)
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

(* Run k-means handler *)
let run_kmeans () =
  match !current_points with
  | [] -> buffer#insert "\nNo data loaded. Please select a file first.\n"
  | points -> (
      buffer#insert
        ("\nRunning k-means with:\n" ^ "K = " ^ string_of_int !current_k ^ "\n"
       ^ "Distance metric: " ^ !current_metric ^ "\n" ^ "Number of points: "
        ^ string_of_int (List.length points)
        ^ "\n\n");

      try
        let distance_fn =
          if !current_metric = "Euclidean" then euclidean_distance
          else manhattan_distance
        in
        let clusters = run_custom_kmeans !current_k points distance_fn in

        buffer#insert "Clustering completed. Results:\n\n";
        List.iteri
          (fun i cluster ->
            buffer#insert
              ("Cluster "
              ^ string_of_int (i + 1)
              ^ " center: "
              ^ GroupProject.Point.to_string cluster
              ^ "\n"))
          clusters;

        (* If points are 2D, we'll add visualization later *)
        if !current_dim = 2 then
          buffer#insert "\nVisualization will be added in the next step!\n"
      with e ->
        buffer#insert
          ("\nError during clustering: " ^ Printexc.to_string e ^ "\n"))

let () =
  ignore (file_button#connect#clicked ~callback:open_file);
  ignore (k_spin#connect#value_changed ~callback:on_k_changed);
  ignore (radio_euclidean#connect#clicked ~callback:on_metric_changed);
  ignore (radio_manhattan#connect#clicked ~callback:on_metric_changed);
  ignore (run_button#connect#clicked ~callback:run_kmeans);
  ignore (window#connect#destroy ~callback:Main.quit);
  run_button#misc#set_sensitive false;
  buffer#set_text
    "Welcome to K-means Clustering\nPlease select a data file to begin.\n";
  window#show ();
  Main.main ()

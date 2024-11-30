open GroupProject.Point
open GroupProject.Csvreader
open GroupProject.Kmeans
open Cairo

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

(* Colors for different clusters *)
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

(* Point scaling helper *)
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

  let scale_x = float_of_int (width - (2 * padding)) /. (!max_x -. !min_x) in
  let scale_y = float_of_int (height - (2 * padding)) /. (!max_y -. !min_y) in

  fun (x, y) ->
    let x_scaled = padding + int_of_float ((x -. !min_x) *. scale_x) in
    let y_scaled = height - padding - int_of_float ((y -. !min_y) *. scale_y) in
    (x_scaled, y_scaled)

let list_fold_lefti f init l =
  let rec fold i acc = function
    | [] -> acc
    | x :: xs -> fold (i + 1) (f acc i x) xs
  in
  fold 0 init l

let generate_svg_visualization points clusters =
  let width = 600 in
  let height = 400 in

  (* Get the scaling function *)
  let scale = scale_points points width height in

  (* Create SVG header *)
  let svg =
    Printf.sprintf
      "<svg width='%d' height='%d' xmlns='http://www.w3.org/2000/svg'>\n" width
      height
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
              let curr_dist = euclidean_distance p cluster_point in
              List.for_all
                (fun other_cluster ->
                  curr_dist <= euclidean_distance p other_cluster)
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
          Printf.sprintf "<circle cx='%d' cy='%d' r='6' fill='black'/>\n" cx cy
        in

        acc ^ cluster_svg ^ center_svg)
      svg clusters
  in

  (* Close SVG *)
  svg ^ "</svg>"

(* Update run_kmeans to use this *)
let run_kmeans () =
  match !current_points with
  | [] -> buffer#insert "\nNo points loaded. Please select a file first.\n"
  | points -> (
      try
        let distance_fn =
          if radio_euclidean#active then euclidean_distance
          else manhattan_distance
        in
        let clusters = run_custom_kmeans !current_k points distance_fn in
        buffer#insert "Clustering completed.\n";

        if !current_dim = 2 then begin
          let svg = generate_svg_visualization points clusters in
          let oc = open_out "clustering.svg" in
          Printf.fprintf oc "%s" svg;
          close_out oc;
          buffer#insert "Visualization saved to 'clustering.svg'\n"
        end
        else buffer#insert "Points are not 2D - visualization not available.\n";

        List.iteri
          (fun i cluster ->
            buffer#insert
              ("Cluster "
              ^ string_of_int (i + 1)
              ^ " center: "
              ^ GroupProject.Point.to_string cluster
              ^ "\n"))
          clusters
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

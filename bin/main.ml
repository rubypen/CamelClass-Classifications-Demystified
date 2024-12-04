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
  let _init = GMain.init () in

  (* Create the window *)
  let window = GWindow.window ~title:"CamelClass" ~show:true () in

  (* Set the window to full-screen after creation *)
  window#fullscreen ();

  (* Create main vertical box for layout *)
  let vbox = GPack.vbox ~packing:window#add () in

  (* Add title labels *)
  let _project_title =
    GMisc.label ~markup:"<span size='50000'><b>CamelClass</b></span>"
      ~selectable:true ~yalign:0.0
      ~packing:(vbox#pack ~expand:true ~fill:true)
      ()
  in
  let _project_subtitle =
    GMisc.label ~markup:"<span size='25000'>K-means Clustering</span>"
      ~selectable:true ~yalign:0.5
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
  drawing_area#misc#set_size_request ~width:600 ~height:400 ();

  (* Create controls area *)
  let controls_box = GPack.hbox ~packing:vbox#pack () in

  (* Start button *)
  let start_button =
    GButton.button ~label:"Start" ~packing:controls_box#pack ()
  in

  let rec start () =
    (* Transition 1: File selection *)
    clean window;
    let controls_box = GPack.vbox ~packing:window#add () in
    let choose_file_button =
      GButton.button ~label:"Choose file" ~packing:controls_box#pack ()
    in
    let _sample_points_button =
      GButton.button ~label:"Sample points" ~packing:controls_box#pack ()
    in
    let _random_points_button =
      GButton.button ~label:"Random points" ~packing:controls_box#pack ()
    in
    window#misc#show_all ();
    ignore (choose_file_button#connect#clicked ~callback:transition3)
  and transition3 () =
    (* Transition 3: After file selection *)
    clean window;
    let vbox = GPack.vbox ~packing:window#add () in

    (* Get screen dimensions *)
    let screen_width = Gdk.Screen.width () in
    let screen_height = Gdk.Screen.height () in

    let title_height = int_of_float (0.05 *. float_of_int screen_height) in

    let title_box = GPack.vbox ~packing:vbox#add ~spacing:5 () in
    title_box#misc#set_size_request ~width:screen_width ~height:title_height ();

    let _divider =
      GMisc.separator `HORIZONTAL
        ~packing:(vbox#pack ~expand:false ~fill:true)
        ()
    in
    _divider#misc#set_size_request ~height:2 ();

    (* Set a small height for the divider *)
    let _top_indent =
      GMisc.label ~text:"" ~height:10
        ~packing:(title_box#pack ~expand:false ~fill:false)
        ()
    in

    (* Title Label *)
    let _project_title =
      GMisc.label ~markup:"<span size='50000'><b>CamelClass</b></span>"
        ~selectable:false ~xalign:0.5 ~yalign:0.5
        ~packing:(title_box#pack ~expand:false ~fill:false)
        ()
    in

    (* Subtitle Label *)
    let _project_subtitle =
      GMisc.label ~markup:"<span size='15000'>K-means Clustering</span>"
        ~selectable:false ~xalign:0.5 ~yalign:0.5
        ~packing:(title_box#pack ~expand:false ~fill:false)
        ()
    in

    let main_area = GPack.hbox ~packing:vbox#add () in
    let menu_area =
      GPack.vbox ~packing:main_area#add ~spacing:5 ~border_width:10 ()
    in
    let _menu_divider =
      GMisc.separator `VERTICAL
        ~packing:(main_area#pack ~expand:false ~fill:true)
        ()
    in
    _menu_divider#misc#set_size_request ~width:2 ();
    let plot_and_log_area =
      GPack.vbox ~packing:main_area#add ~spacing:5 ~border_width:10 ()
    in

    (* Create drawing area *)
    (*let drawing_area = GMisc.drawing_area ~packing:vbox#pack () in let () =
      drawing_area#misc#set_size_request ~width:600 ~height:400 () in*)
    let _logsubtitle =
      GMisc.label
        ~markup:
          "<span size='40000' weight='bold' underline='single'>Menu</span>"
        ~selectable:false ~xalign:0.5 ~yalign:0.5
        ~packing:(menu_area#pack ~expand:false ~fill:false)
        ()
    in

    let _logindent =
      GMisc.label
        ~markup:"<span size='15000' weight='bold' underline='single'> </span>"
        ~selectable:false ~xalign:0.5 ~yalign:0.5
        ~packing:(menu_area#pack ~expand:false ~fill:false)
        ()
    in

    (* Create controls area *)
    let controls_box = GPack.vbox ~packing:menu_area#pack ~spacing:10 () in

    let open_file_box = GPack.hbox ~packing:controls_box#pack ~spacing:5 () in

    (* File selection button *)
    let file_button =
      GButton.button ~label:"Open File" ~packing:open_file_box#pack ()
    in
    file_button#misc#set_size_request ~width:200 ();

    let file_label_box = GPack.vbox ~packing:open_file_box#pack ~spacing:5 () in

    (* Label for "Selected File" header *)
    let _selected_file_label =
      GMisc.label
        ~markup:
          "<span size='15000' weight='bold' underline='single'>Selected \
           File</span>"
        ~selectable:false ~xalign:0.0 ~yalign:0.0
        ~packing:(file_label_box#pack ~expand:false ~fill:false)
        ()
    in

    (* Label to display the selected filename *)
    let file_name_label =
      GMisc.label ~text:"None" ~xalign:0.0 ~yalign:0.0
        ~packing:(file_label_box#pack ~expand:false ~fill:false)
        ()
    in

    (* K selection *)
    let k_box = GPack.hbox ~packing:controls_box#pack () ~spacing:10 in
    let _ =
      GMisc.label ~markup:"<span size='15000' weight='bold'>K-Value:</span>"
        ~packing:k_box#pack ()
    in
    let k_spin =
      GEdit.spin_button ~packing:k_box#pack ~digits:0 ~numeric:true ~wrap:true
        ()
    in
    k_spin#adjustment#set_bounds ~lower:1. ~upper:10.0 ~step_incr:1. ();
    k_spin#set_value 3.;

    (* Distance metric selection *)
    let metric_box = GPack.hbox ~packing:controls_box#pack () in
    let _ =
      GMisc.label
        ~markup:"<span size='15000' weight='bold'>Distance Metric:</span>"
        ~packing:metric_box#pack ()
    in
    let radio_euclidean =
      GButton.radio_button ~label:"Euclidean" ~packing:metric_box#pack ()
    in
    let radio_manhattan =
      GButton.radio_button ~group:radio_euclidean#group ~label:"Manhattan"
        ~packing:metric_box#pack ()
    in

    let cluster_colors_box =
      GPack.vbox ~packing:controls_box#pack ~spacing:5 ()
    in

    let _cluster_colors_label =
      GMisc.label
        ~markup:"<span size='15000' weight='bold'>Cluster Colors:</span>"
        ~packing:cluster_colors_box#pack () ~selectable:false ~xalign:0.0
        ~yalign:0.0
    in

    (* Make a square using coordinates *)
    let polygon = [ (10, 10); (60, 10); (60, 60); (10, 60); (10, 10) ] in

    (* Define a color mapping for the polygons *)
    let colors =
      [|
        ("red", (1.0, 0.0, 0.0));
        ("orange", (1.0, 0.5, 0.0));
        ("gold", (1.0, 0.84, 0.0));
        ("yellow", (1.0, 1.0, 0.0));
        ("green", (0.0, 1.0, 0.0));
        ("cyan", (0.0, 1.0, 1.0));
        ("blue", (0.0, 0.0, 1.0));
        ("purple", (0.5, 0.0, 0.5));
        ("pink", (1.0, 0.0, 1.0));
        ("brown", (0.65, 0.16, 0.16));
        ("gray", (0.5, 0.5, 0.5));
        ("silver", (0.75, 0.75, 0.75));
      |]
    in

    (* Define a hashtable to check which colors were clicked *)
    let selected_squares = Hashtbl.create 10 in

    (* [draw_square] draws the square on the window by using the provided
       points *)
    let draw_square cr points (r, g, b) =
      match points with
      | [] -> ()
      | (x, y) :: tl ->
          Cairo.move_to cr (float x) (float y);
          List.iter (fun (x, y) -> Cairo.line_to cr (float x) (float y)) tl;
          Cairo.set_source_rgb cr r g b;
          Cairo.fill cr
    in

    (* [draw] draws the square on the canvas *)
    let draw square (name, (r, g, b)) (cr : Cairo.context) =
      draw_square cr polygon (r, g, b);
      (* Check if this square is selected. If it is, add an overlay that says
         "selected"*)
      if Hashtbl.mem selected_squares name then (
        Cairo.set_source_rgba cr 1.0 1.0 1.0 0.6;
        Cairo.rectangle cr 10.0 10.0 ~w:50.0 ~h:50.0;
        Cairo.fill cr;

        Cairo.set_source_rgb cr 0.0 0.0 0.0;
        Cairo.select_font_face cr "Sans" ~weight:Bold;
        Cairo.set_font_size cr 10.0;
        Cairo.move_to cr 16.0 40.0;
        Cairo.show_text cr "Selected";
        Cairo.stroke cr);
      true
    in

    (* [square_selected] marks a square as selected when it is clicked on *)
    let square_selected name square =
      if Hashtbl.mem selected_squares name then
        Hashtbl.remove selected_squares name
      else Hashtbl.add selected_squares name ();

      (* Draw an overlay that says "selected" onto the square *)
      square#misc#queue_draw ();
      false
    in

    (* Create the grid (table) *)
    let grid =
      GPack.table ~rows:2 ~columns:6 ~homogeneous:true
        ~packing:cluster_colors_box#pack ()
    in

    (* Go through all the colors in the array to make the grid *)
    Array.iteri
      (fun idx (name, color) ->
        let row = idx / 6 in
        let col = idx mod 6 in

        let square =
          GMisc.drawing_area ~packing:(grid#attach ~left:col ~top:row) ()
        in
        square#misc#set_size_request ~width:70 ~height:70 ();

        (* Make the squares clickable *)
        square#event#add [ `BUTTON_PRESS ];

        (* Make a square with the next color in the array *)
        ignore (square#misc#connect#draw ~callback:(draw square (name, color)));

        (* Make the squares selectable *)
        ignore
          (square#event#connect#button_press ~callback:(fun _ ->
               square_selected name square)))
      colors;

    let _divider =
      GMisc.separator `HORIZONTAL
        ~packing:(menu_area#pack ~expand:false ~fill:true)
        ()
    in
    _divider#misc#set_size_request ~height:2 ();

    let run_kmeans_button_area =
      GPack.vbox ~packing:menu_area#pack ~spacing:10 ()
    in

    (* Run button *)
    let run_button =
      GButton.button ~label:"Run K-means" ~packing:run_kmeans_button_area#pack
        ()
    in

    let graph_box_height = int_of_float (0.6 *. float_of_int screen_height) in
    let graph_box_width = int_of_float (0.6 *. float_of_int screen_width) in
    (* Create a box for the graph*)
    let graph_box = GPack.box `HORIZONTAL ~packing:plot_and_log_area#pack () in
    let () =
      graph_box#misc#set_size_request ~width:graph_box_width
        ~height:graph_box_height ()
    in
    (* Put the PNG file in the GUI *)
    let graph_image =
      GMisc.image ~file:"no_graph.png" ~packing:graph_box#add ()
    in

    let _divider =
      GMisc.separator `HORIZONTAL
        ~packing:(plot_and_log_area#pack ~expand:false ~fill:true)
        ()
    in
    _divider#misc#set_size_request ~height:2 ();

    let log_area = GPack.vbox ~packing:plot_and_log_area#add ~spacing:5 () in

    let _logsubtitle =
      GMisc.label
        ~markup:"<span size='30000' weight='bold' underline='single'>Log</span>"
        ~selectable:false ~xalign:0.0 ~yalign:0.5
        ~packing:(log_area#pack ~expand:false ~fill:false)
        ()
    in

    let scroll_view_height = int_of_float (0.4 *. float_of_int screen_height) in
    let scroll_view_width = int_of_float (0.6 *. float_of_int screen_width) in

    (* Text view for messages *)
    let scrolled_window =
      GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
        ~packing:(log_area#pack ~expand:true ~fill:true)
        ()
    in
    let text_view = GText.view ~packing:scrolled_window#add () in
    let () =
      text_view#misc#set_size_request ~width:scroll_view_width
        ~height:scroll_view_height ()
    in
    let buffer = text_view#buffer in

    (* Store current points and dimension *)
    let current_points = ref [] in
    let current_dim = ref 0 in
    let current_k = ref 2 in
    let current_metric = ref "Euclidean" in

    (* Distance metric change handler *)
    let on_metric_changed () =
      current_metric :=
        if radio_euclidean#active then "Euclidean" else "Manhattan";
      buffer#insert ("\nDistance metric changed to: " ^ !current_metric ^ "\n")
    in

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
              let file_basename = Filename.basename file in
              buffer#set_text ("Loading file: " ^ file ^ "\n");
              file_name_label#set_text file_basename;
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
                     Note: Points are not 2D. Visualization will not be \
                     available.\n";

                run_button#misc#set_sensitive true
              with e ->
                buffer#set_text
                  ("Error reading file: " ^ Printexc.to_string e ^ "\n");
                run_button#misc#set_sensitive false)
          | None ->
              buffer#set_text "No file selected.\n";
              file_name_label#set_text "None";
              run_button#misc#set_sensitive false)
      | `CANCEL | `DELETE_EVENT ->
          buffer#set_text "File selection cancelled.\n";
          run_button#misc#set_sensitive false
    in

    let predefined_colors = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12 |] in

    let create_2d_graph filename (points : t list) clusters =
      let max_x = ref (-.max_float) in
      let min_x = ref max_float in
      let max_y = ref (-.max_float) in
      let min_y = ref max_float in

      let x = Array.make (List.length points) 0.0 in
      let y = Array.make (List.length points) 0.0 in

      for i = 0 to List.length points - 1 do
        let coords = get_coordinates (List.nth points i) in
        let curr_x = List.nth coords 0 in
        let curr_y = List.nth coords 1 in
        x.(i) <- curr_x;
        y.(i) <- curr_y;
        min_x := min !min_x curr_x;
        max_x := max !max_x curr_x;
        min_y := min !min_y curr_y;
        max_y := max !max_y curr_y
      done;

      let open Plplot in
      plsdev "pngcairo";
      plsfnam filename;
      plinit ();

      let x_range = !max_x -. !min_x in
      let y_range = !max_y -. !min_y in

      (* Initialize PLplot *)
      plenv
        (!min_x -. (0.1 *. x_range))
        (!max_x +. (0.1 *. x_range))
        (!min_y -. (0.1 *. y_range))
        (!max_y +. (0.1 *. y_range))
        0 0;
      pllab "X-axis" "Y-axis" "2D Graph";

      (* Plot points for each cluster *)
      List.iteri
        (fun i cluster_point ->
          let color_index =
            predefined_colors.(i mod Array.length predefined_colors)
          in
          plcol0 color_index;

          (* Get the points by clusters! *)
          let cluster_points =
            List.filter
              (fun p ->
                let curr_dist =
                  if radio_euclidean#active then
                    euclidean_distance p cluster_point
                  else manhattan_distance p cluster_point
                in
                List.for_all
                  (fun other_cluster ->
                    curr_dist
                    <=
                    if radio_euclidean#active then
                      euclidean_distance p other_cluster
                    else manhattan_distance p other_cluster)
                  clusters)
              points
          in

          let x_cluster_points =
            Array.of_list
              (List.map
                 (fun p -> List.nth (get_coordinates p) 0)
                 cluster_points)
          in
          let y_cluster_points =
            Array.of_list
              (List.map
                 (fun p -> List.nth (get_coordinates p) 1)
                 cluster_points)
          in

          plpoin x_cluster_points y_cluster_points 9;

          let center_coords = get_coordinates cluster_point in
          let cx = List.nth center_coords 0 in
          let cy = List.nth center_coords 1 in
          plcol0 3;
          plpoin [| cx |] [| cy |] 5)
        clusters;

      plend ();
      Printf.printf "2D plot saved to %s\n" filename
    in

    let create_1d_graph filename (points : t list) clusters =
      let max_x = ref (-.max_float) in
      let min_x = ref max_float in

      (* Convert 1D points list to Array *)
      let x = Array.make (List.length points) 0.0 in

      (* Create an array from the existing points and find the max and min
         bounds*)
      for i = 0 to List.length points - 1 do
        let coords = get_coordinates (List.nth points i) in
        let curr_x = List.nth coords 0 in
        x.(i) <- curr_x;
        min_x := min !min_x curr_x;
        max_x := max !max_x curr_x
      done;

      let y = Array.make (Array.length x) 0.0 in

      let max_x_clusters = ref (-.max_float) in
      let min_x_clusters = ref max_float in

      (* Convert 1D points list to Array *)
      let x_clusters = Array.make (List.length points) 0.0 in

      (* Create an array from the existing points and find the max and min
         bounds*)
      for i = 0 to List.length clusters - 1 do
        let coords = get_coordinates (List.nth clusters i) in
        let curr_x = List.nth coords 0 in
        x_clusters.(i) <- curr_x;
        min_x_clusters := min !min_x_clusters curr_x;
        max_x_clusters := max !max_x_clusters curr_x
      done;

      let y_clusters = Array.make (Array.length x) 0.0 in

      let open Plplot in
      plsdev "pngcairo";
      plsfnam filename;
      plinit ();

      let range = !max_x -. !min_x in

      (* Initialize PLplot *)
      plenv (!min_x -. (0.1 *. range)) (!max_x +. (0.1 *. range)) (-0.1) 0.1 0 0;

      pllab "X-axis" "" "1D Graph";

      (*let n_points = 100 in let x = Array.init n_points (fun _ -> Random.float
        10.0) in let y = Array.make n_points 0.0 in*)
      (* Plot normal points *)
      plcol0 2;
      plschr 0.0 1.0;
      plpoin x y 9;

      (* Plot cluster points *)
      plcol0 3;
      plpoin x_clusters y_clusters 5;

      plend ();
      Printf.printf "1D plot saved to %s\n" filename
    in

    let create_3d_graph filename (points : t list) clusters =
      let max_x = ref (-.max_float) in
      let min_x = ref max_float in
      let max_y = ref (-.max_float) in
      let min_y = ref max_float in
      let max_z = ref (-.max_float) in
      let min_z = ref max_float in

      (* Convert 3D points list to Array *)
      let x = Array.make (List.length points) 0.0 in
      let y = Array.make (List.length points) 0.0 in
      let z = Array.make (List.length points) 0.0 in

      for i = 0 to List.length points - 1 do
        let coords = get_coordinates (List.nth points i) in
        let curr_x = List.nth coords 0 in
        let curr_y = List.nth coords 1 in
        let curr_z = List.nth coords 2 in
        x.(i) <- curr_x;
        y.(i) <- curr_y;
        z.(i) <- curr_z;
        min_x := min !min_x curr_x;
        max_x := max !max_x curr_x;
        min_y := min !min_y curr_y;
        max_y := max !max_y curr_y;
        min_z := min !min_z curr_z;
        max_z := max !max_z curr_z
      done;

      let max_x_clusters = ref (-.max_float) in
      let min_x_clusters = ref max_float in
      let max_y_clusters = ref (-.max_float) in
      let min_y_clusters = ref max_float in
      let max_z_clusters = ref (-.max_float) in
      let min_z_clusters = ref max_float in

      (* Convert 2D points list to Array *)
      let x_clusters = Array.make (List.length clusters) 0.0 in
      let y_clusters = Array.make (List.length clusters) 0.0 in
      let z_clusters = Array.make (List.length clusters) 0.0 in

      (* Create an array from the existing points and find the max and min
         bounds*)
      for i = 0 to List.length clusters - 1 do
        let coords = get_coordinates (List.nth clusters i) in
        let curr_x = List.nth coords 0 in
        let curr_y = List.nth coords 1 in
        let curr_z = List.nth coords 2 in
        x_clusters.(i) <- curr_x;
        y_clusters.(i) <- curr_y;
        z_clusters.(i) <- curr_z;
        min_x_clusters := min !min_x_clusters curr_x;
        max_x_clusters := max !max_x_clusters curr_x;
        min_y_clusters := min !min_y_clusters curr_y;
        max_y_clusters := max !max_y_clusters curr_y;
        min_z_clusters := min !min_z_clusters curr_z;
        max_z_clusters := max !max_z_clusters curr_z
      done;

      let open Plplot in
      plsdev "pngcairo";
      plsfnam filename;
      plinit ();

      let x_range = !max_x -. !min_x in
      let y_range = !max_y -. !min_y in
      let z_range = !max_z -. !min_z in

      let max_range = max x_range (max y_range z_range) in

      let scale_factor = 5.0 in

      plenv
        (-0.5 *. max_range *. scale_factor)
        (0.5 *. max_range *. scale_factor)
        (-0.5 *. max_range *. scale_factor)
        (0.5 *. max_range *. scale_factor)
        0 0;

      plw3d (1.0 *. scale_factor) (1.0 *. scale_factor) (1.0 *. scale_factor)
        (!min_x -. (0.1 *. x_range))
        (!max_x +. (0.1 *. x_range))
        (!min_y -. (0.1 *. y_range))
        (!max_y +. (0.1 *. y_range))
        (!min_z -. (0.1 *. z_range))
        (!max_z +. (0.1 *. z_range))
        30.0 30.0;

      plcol0 1;
      plbox3 "bnstu" "X-axis" 0.0 0 "bnstu" "Y-axis" 0.0 0 "bcdmnstuv" "Z-axis"
        0.0 0;

      (* let n_points = 100 in let x = Array.init n_points (fun _ ->
         Random.float 2.0 -. 1.0) in let y = Array.init n_points (fun _ ->
         Random.float 2.0 -. 1.0) in let z = Array.init n_points (fun _ ->
         Random.float 2.0 -. 1.0) in*)
      plcol0 2;
      plpoin3 x y z 9;

      (* Plot cluster points *)
      plcol0 3;
      plpoin3 x_clusters y_clusters z_clusters 5;

      plend ();
      Printf.printf "3D scatter plot saved to %s\n" filename
    in

    let plot_graph view (points : t list) clusters () =
      let filename = "graph.png" in
      if view = "1D" then create_1d_graph filename points clusters
      else if view = "2D" then create_2d_graph filename points clusters
      else create_3d_graph filename points clusters
    in

    (* Run k-means handler *)
    let run_kmeans () =
      match !current_points with
      | [] -> buffer#insert "\nNo points loaded. Please select a file first.\n"
      | points -> (
          try
            let distance_fn =
              if radio_euclidean#active then euclidean_distance
              else manhattan_distance
            in
            buffer#insert ("Using " ^ !current_metric ^ " distance metric.\n");
            let clusters = run_custom_kmeans !current_k points distance_fn in
            buffer#insert "Clustering completed.\n";

            (* let _ = generate_svg_visualization points clusters in *)
            if !current_dim == 1 then begin
              let _ = plot_graph "1D" points clusters () in
              buffer#insert "Visualization saved to 'graph.png'\n";
              graph_image#set_file "graph.png"
            end
            else if !current_dim == 2 then begin
              let _ = plot_graph "2D" points clusters () in
              buffer#insert "Visualization saved to 'graph.png'\n";
              graph_image#set_file "graph.png"
            end
            else if !current_dim == 3 then begin
              let _ = plot_graph "3D" points clusters () in
              buffer#insert "Visualization saved to 'graph.png'\n";
              graph_image#set_file "graph.png"
            end
            else
              buffer#insert
                "Only points in the 1D, 2D, and 3D spaces can be graphed. \n";

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
    in

    (* Connect signals *)
    ignore (file_button#connect#clicked ~callback:open_file);
    ignore (radio_euclidean#connect#clicked ~callback:on_metric_changed);
    ignore (radio_manhattan#connect#clicked ~callback:on_metric_changed);
    ignore (run_button#connect#clicked ~callback:run_kmeans);
    ignore (window#connect#destroy ~callback:Main.quit);

    (* Initialize state *)
    run_button#misc#set_sensitive false;
    buffer#set_text
      "Welcome to CamelClass K-means Clustering\n\
       Please select a data file to begin.\n";

    let next_button = GButton.button ~label:"Next" ~packing:vbox#pack () in
    window#misc#show_all ();
    ignore (next_button#connect#clicked ~callback:transition4)
  and transition4 () =
    (* Transition 6: Show statistics *)
    clean window;
    let stats_box = GPack.vbox ~packing:window#add () in
    let _statistics_title =
      GMisc.label ~markup:"<span size='50000'><b>Your statistics: </b></span>"
        ~selectable:true ~yalign:0.0 ~height:50
        ~packing:(stats_box#pack ~expand:true ~fill:true)
        ()
    in
    let next_button = GButton.button ~label:"Next" ~packing:stats_box#pack () in
    window#misc#show_all ();
    ignore (next_button#connect#clicked ~callback:transition5)
  and transition5 () =
    (* Transition 7: Plotting *)
    clean window;
    let plot_box = GPack.vbox ~packing:window#add () in
    let _plot_title =
      GMisc.label ~markup:"<span size='50000'><b>Elbow plot: </b></span>"
        ~selectable:true ~yalign:0.0 ~height:50
        ~packing:(plot_box#pack ~expand:true ~fill:true)
        ()
    in
    let next_button = GButton.button ~label:"Next" ~packing:plot_box#pack () in
    window#misc#show_all ();
    ignore (next_button#connect#clicked ~callback:transition6)
  and transition6 () =
    (* Transition 8: End Screen *)
    clean window;
    let controls_box = GPack.vbox ~packing:window#add () in
    (* Start/Quit buttons *)
    let start_over_button =
      GButton.button ~label:"Start over" ~packing:controls_box#pack ()
    in
    let quit_button =
      GButton.button ~label:"Quit" ~packing:controls_box#pack ()
    in
    let start_over () = start () in

    let quit () =
      start ();
      clean window;
      let thanks_box = GPack.vbox ~packing:window#add () in
      let _thanks_title =
        GMisc.label
          ~markup:
            "<span size='80000'><b>Thank you for your attention! </b></span>"
          ~selectable:true ~yalign:0.0
          ~packing:(thanks_box#pack ~expand:true ~fill:true)
          ()
      in
      let _authors_title =
        GMisc.label
          ~markup:
            "<span size='30000'><b>By: Keti Sulamanidze, Neha Naveen, Ruby \
             Penafiel-Gutierrez, Samantha Vaca, Varvara Babii </b></span>"
          ~selectable:true ~yalign:0.0 ~height:50
          ~packing:(thanks_box#pack ~expand:true ~fill:true)
          ()
      in
      let final_quit_button =
        GButton.button ~label:"Quit" ~packing:thanks_box#pack ()
      in
      window#misc#show_all ();
      ignore
        (final_quit_button#connect#clicked ~callback:(fun () ->
             window#destroy ()))
    in
    window#misc#show_all ();
    ignore (start_over_button#connect#clicked ~callback:start_over);
    ignore (quit_button#connect#clicked ~callback:quit)
  in

  (* Connect signals *)
  ignore (start_button#connect#clicked ~callback:start);
  ignore (window#connect#destroy ~callback:GMain.quit);

  (* Show the window *)
  window#show ();
  GMain.main ()

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

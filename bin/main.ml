open GroupProject.Point
open GroupProject.Csvreader
open GroupProject.Kmeans
open GroupProject.Knn
open GroupProject.Extensions
open GroupProject.Visualizations
open GMain
open Gtk

(* Random points file generator *)
let generate () =
  Random.self_init ();
  let random_a_b a b = a + Random.int b in
  let dim = random_a_b 1 3 in
  let n = random_a_b 1 100 in
  let points_lst = ref [] in
  for i = 0 to n - 1 do
    let line = ref "" in
    for j = 0 to dim - 1 do
      let coordinate = random_a_b 1 100 in
      let coordinate_str = string_of_float (float_of_int coordinate /. 10.) in
      if !line == "" then line := coordinate_str
      else line := !line ^ ", " ^ coordinate_str
    done;
    points_lst := !line :: !points_lst
  done;
  BatFile.write_lines "data/random.csv" (BatList.enum !points_lst)

(* Find a widget by name *)
let find_widget_by_name parent name widget_type =
  let rec find_in_container container =
    let children = container#children in
    List.find_opt
      (fun child ->
        try
          match child#misc#get with
          | Some w -> w#name = name
          | None -> false
        with _ -> false)
      children
  in
  match find_in_container parent with
  | Some w -> Some (widget_type w)
  | None -> None

(* -------------------------------------------------------------------------- *)
(* GUI FUNCTIONALITY *)
(* -------------------------------------------------------------------------- *)
let initialize_gui () =
  (* Initialize the GUI *)
  let init = GMain.init () in
  ignore init;

  (* Create the window *)
  let window = GWindow.window ~title:"CamelClass" ~show:true () in
  window#maximize ();

  (* Create choice reference *)
  let choice = ref "" in

  (* Create main vertical box for layout *)
  let vbox = GPack.vbox ~packing:window#add () in

  (* Add title labels *)
  let _project_title =
    GMisc.label ~markup:"<span size='100000'><b>CamelClass</b></span>"
      ~selectable:true ~yalign:0.0
      ~packing:(vbox#pack ~expand:true ~fill:true)
      ()
  in
  let _project_subtitle =
    GMisc.label ~markup:"<span size='35000'>K-means Clustering</span>"
      ~selectable:true ~yalign:0.0
      ~packing:(vbox#pack ~expand:true ~fill:true)
      ()
  in

  (* Create drawing area *)
  (* let drawing_area = GMisc.drawing_area ~packing:vbox#pack () in
  drawing_area#misc#set_size_request ~width:600 ~height:400 (); *)

  (* Create controls area *)
  let controls_box = GPack.hbox ~spacing:5 ~packing:vbox#pack () in

  (* Start button *)
  let start_button =
    GButton.button ~label:"Start"
      ~packing:(controls_box#pack ~expand:true ~fill:false)
      ()
  in
  let font = GPango.font_description_from_string "Arial 20" in
  start_button#misc#modify_font font;
  start_button#misc#set_size_request ~height:80 ~width:200 ();

  (* Picture *)
  let cwd = Sys.getcwd () in
  let font_file = Filename.concat cwd "data/pictures/background.jpeg" in
  let pixbuf = GdkPixbuf.from_file_at_size font_file ~width:1000 ~height:600 in

  let _font_picture =
    GMisc.image ~pixbuf ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in

  (* Cleaning the existing window*)
  let clean (window : GWindow.window) =
    match window#children with
    | [] -> ()
    | children -> List.iter (fun widget -> widget#destroy ()) children
  in

  let rec start () =
    (* Clean existing window *)
    clean window;
    let controls_box =
      GPack.vbox ~width:60 ~height:400 ~packing:window#add ~spacing:20
        ~border_width:400 ()
    in
    controls_box#set_homogeneous false;

    (* Setting the font *)
    let font = GPango.font_description_from_string "Arial 20" in
    let choose_file_button =
      GButton.button ~label:"Choose file"
        ~packing:(controls_box#pack ~expand:true ~fill:true)
        ()
    in
    choose_file_button#misc#set_size_request ~height:50 ~width:50 ();
    choose_file_button#misc#modify_font font;

    let sample_points_button =
      GButton.button ~label:"Sample points"
        ~packing:(controls_box#pack ~expand:true ~fill:true)
        ()
    in
    sample_points_button#misc#set_size_request ~height:50 ~width:50 ();
    sample_points_button#misc#modify_font font;

    let random_points_button =
      GButton.button ~label:"Random points"
        ~packing:(controls_box#pack ~expand:true ~fill:true)
        ()
    in
    random_points_button#misc#set_size_request ~height:50 ~width:50 ();
    random_points_button#misc#modify_font font;

    window#misc#show_all ();
    ignore
      (choose_file_button#connect#clicked ~callback:(fun () ->
           choice := "file";
           transition3 ()));
    ignore
      (sample_points_button#connect#clicked ~callback:(fun () ->
           choice := "sample";
           transition3 ()));
    ignore
      (random_points_button#connect#clicked ~callback:(fun () ->
           choice := "random";
           generate ();
           transition3 ()))
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

    (* Store current points and dimension *)
    let current_points = ref [] in
    let current_dim = ref 0 in
    let current_k = ref 3 in
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
    let next_button = GButton.button ~label:"Next" ~packing:vbox#pack () in

    (* Disable the next button initially *)
    next_button#misc#set_sensitive false;

    let _divider =
      GMisc.separator `HORIZONTAL
        ~packing:(plot_and_log_area#pack ~expand:false ~fill:true)
        ()
    in
    _divider#misc#set_size_request ~height:2 ();

    let log_area = GPack.vbox ~packing:plot_and_log_area#add ~spacing:5 () in

    let _logsubtitle =
      GMisc.label
        ~markup:
          "<span size='30000' weight='bold' underline='single'>Log \
           (scrollable)</span>"
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
    (* Define buffer *)
    let buffer = text_view#buffer in
    let auto_scroll () =
      let end_iter = buffer#end_iter in
      ignore
        (text_view#scroll_to_iter end_iter ~within_margin:0.0 ~use_align:true
           ~xalign:0.0 ~yalign:0.0);
      buffer#place_cursor ~where:end_iter
    in
    (* Update current_k when k_spin value changes *)
    ignore
      (k_spin#connect#value_changed ~callback:(fun () ->
           current_k := int_of_float k_spin#value));
    let current_metric = ref "Euclidean" in
    (* Add Optimize K button *)
    let optimize_k_box = GPack.hbox ~packing:controls_box#pack ~spacing:10 () in
    let optimize_k_button =
      GButton.button ~label:"Optimize K" ~packing:optimize_k_box#pack ()
    in
    let optimize_k_handler () =
      if List.length !current_points = 0 then begin
        buffer#insert "\nNo points loaded. Please select a file first.\n";
        auto_scroll ()
      end
      else
        let points = !current_points in
        let num_points = List.length points in
        let dist_fn =
          if radio_euclidean#active then euclidean_distance
          else manhattan_distance
        in

        buffer#insert "\nCalculating optimal K value...\n";
        auto_scroll ();

        (* Run k-means for a range up to min(6, num_points) *)
        let max_k = min 6 num_points in
        buffer#insert
          (Printf.sprintf "Testing different k values (2 to %d)...\n" max_k);
        auto_scroll ();
        let cluster_sets = ref [] in

        (* Try each k value individually and update progress *)
        for k = 2 to max_k do
          buffer#insert (Printf.sprintf "Testing k=%d...\n" k);
          auto_scroll ();
          try
            let clusters = run_kmeans k points dist_fn in
            cluster_sets := clusters :: !cluster_sets;
            buffer#insert (Printf.sprintf "Completed k=%d\n" k);
            auto_scroll ()
          with Invalid_argument _ ->
            buffer#insert
              (Printf.sprintf "Skipped k=%d (not enough points)\n" k);
            auto_scroll ()
        done;

        if List.length !cluster_sets = 0 then
          buffer#insert
            "\nCould not find optimal k value. Try with more data points.\n"
        else begin
          buffer#insert "Calculating best k value...\n";
          auto_scroll ();
          let best_k = find_best_k (List.rev !cluster_sets) points dist_fn in

          (* Display final result *)
          buffer#insert (Printf.sprintf "\nOptimal K value found: %d\n" best_k);
          auto_scroll ();

          (* Update the k-spin value *)
          k_spin#set_value (float_of_int best_k);

          buffer#insert "Done! The k-value has been updated.\n";
          auto_scroll ()
        end
    in
    (* Connect the handler *)
    ignore (optimize_k_button#connect#clicked ~callback:optimize_k_handler);
    (* Distance metric change handler *)
    let on_metric_changed () =
      current_metric :=
        if radio_euclidean#active then "Euclidean" else "Manhattan";
      buffer#insert ("\nDistance metric changed to: " ^ !current_metric ^ "\n");
      auto_scroll ()
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
              auto_scroll ();
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
                auto_scroll ();

                let rec show_n_points points n =
                  match (points, n) with
                  | [], _ -> ()
                  | _, 0 -> ()
                  | p :: ps, n ->
                      buffer#insert (GroupProject.Point.to_string p ^ "\n");
                      auto_scroll ();
                      show_n_points ps (n - 1)
                in
                show_n_points !current_points 5;

                if dim > 3 then
                  buffer#insert
                    "\n\
                     Note: Points are greater than 3D. Visualization will not \
                     be available.\n";
                auto_scroll ();

                run_button#misc#set_sensitive true
              with e ->
                buffer#set_text
                  ("Error reading file: " ^ Printexc.to_string e ^ "\n");
                auto_scroll ();
                run_button#misc#set_sensitive false)
          | None ->
              buffer#set_text "No file selected.\n";
              auto_scroll ();
              file_name_label#set_text "None";
              run_button#misc#set_sensitive false)
      | `CANCEL | `DELETE_EVENT ->
          buffer#set_text "File selection cancelled.\n";
          auto_scroll ();
          run_button#misc#set_sensitive false
    in

    let open_sample_file () =
      buffer#set_text "You selected points from a sample file.\n";
      auto_scroll ();
      let cwd = Sys.getcwd () in
      let sample_filename = Filename.concat cwd "data/test_data_2d.csv" in

      let file_basename = Filename.basename sample_filename in
      buffer#insert ("Loading file: " ^ sample_filename ^ "\n");
      auto_scroll ();
      file_name_label#set_text file_basename;
      let csv = Csv.load sample_filename in
      let first_line = List.hd csv in
      let dim = List.length first_line in
      current_dim := dim;
      current_points := CsvReaderImpl.read_points dim sample_filename;

      buffer#insert
        ("Successfully loaded "
        ^ string_of_int (List.length !current_points)
        ^ " points of dimension " ^ string_of_int dim ^ "\n\n"
        ^ "Sample points:\n");
      auto_scroll ();

      let rec show_n_points points n =
        match (points, n) with
        | [], _ -> ()
        | _, 0 -> ()
        | p :: ps, n ->
            buffer#insert (GroupProject.Point.to_string p ^ "\n");
            auto_scroll ();
            show_n_points ps (n - 1)
      in
      show_n_points !current_points 5;
      run_button#misc#set_sensitive true
    in

    let open_random_file () =
      buffer#set_text "You selected points from a random points generator.\n";
      auto_scroll ();

      let cwd = Sys.getcwd () in
      let random_filename = Filename.concat cwd "data/random.csv" in

      let file_basename = Filename.basename random_filename in
      buffer#insert ("Loading file: " ^ random_filename ^ "\n");
      auto_scroll ();
      file_name_label#set_text file_basename;
      let csv = Csv.load random_filename in
      let first_line = List.hd csv in
      let dim = List.length first_line in
      current_dim := dim;
      current_points := CsvReaderImpl.read_points dim random_filename;

      buffer#insert
        ("Successfully loaded "
        ^ string_of_int (List.length !current_points)
        ^ " points of dimension " ^ string_of_int dim ^ "\n\n"
        ^ "Sample points:\n");
      auto_scroll ();

      let rec show_n_points points n =
        match (points, n) with
        | [], _ -> ()
        | _, 0 -> ()
        | p :: ps, n ->
            buffer#insert (GroupProject.Point.to_string p ^ "\n");
            auto_scroll ();
            show_n_points ps (n - 1)
      in
      show_n_points !current_points 5;
      run_button#misc#set_sensitive true
    in

    let create_2d_graph filename (points : t list) clusters =
      GroupProject.Visualizations.create_2d_graph filename points clusters
    in

    let create_1d_graph filename (points : t list) clusters =
      GroupProject.Visualizations.create_1d_graph filename points clusters
    in

    let create_3d_graph filename (points : t list) clusters =
      GroupProject.Visualizations.create_3d_graph filename points clusters
    in

    let plot_graph view points clusters () =
      let filename = "graph.png" in
      if view = "1D" then create_1d_graph filename points clusters
      else if view = "2D" then create_2d_graph filename points clusters
      else create_3d_graph filename points clusters
    in

    (* Run k-means handler *)
    let run_kmeans () =
      Printf.printf "Starting run_kmeans...\n%!";
      (* Debug print *)
      match !current_points with
      | [] ->
          Printf.printf "No points loaded\n%!";
          (* Debug print *)
          buffer#insert "\nNo points loaded. Please select a file first.\n";
          auto_scroll ()
      | points ->
          (try
             Printf.printf "Points loaded, running clustering...\n%!";
             (* Debug print *)
             let dist_fn =
               if radio_euclidean#active then euclidean_distance
               else manhattan_distance
             in
             buffer#insert ("Using " ^ !current_metric ^ " distance metric.\n");
             auto_scroll ();
             Printf.printf "Running k-means with k=%d...\n%!" !current_k;
             (* Debug print *)
             let clusters = run_custom_kmeans !current_k points dist_fn in
             buffer#insert "Clustering completed.\n";
             auto_scroll ();
             Printf.printf "Clustering completed, creating visualization...\n%!";

             (* Debug print *)
             if !current_dim == 1 then begin
               Printf.printf "Creating 1D visualization...\n%!";
               (* Debug print *)
               let _ = plot_graph "1D" points clusters () in
               buffer#insert "Visualization saved to 'graph.png'\n";
               auto_scroll ();
               graph_image#set_file "graph.png"
             end
             else if !current_dim == 2 then begin
               Printf.printf "Creating 2D visualization...\n%!";
               (* Debug print *)
               let _ = plot_graph "2D" points clusters () in
               buffer#insert "Visualization saved to 'graph.png'\n";
               auto_scroll ();
               graph_image#set_file "graph.png"
             end
             else if !current_dim == 3 then begin
               Printf.printf "Creating 3D visualization...\n%!";
               (* Debug print *)
               let _ = plot_graph "3D" points clusters () in
               buffer#insert "Visualization saved to 'graph.png'\n";
               auto_scroll ();
               graph_image#set_file "graph.png"
             end
             else
               buffer#insert
                 "Only points in the 1D, 2D, and 3D spaces can be graphed. \n";
             auto_scroll ();

             Printf.printf "Visualization completed\n%!";
             (* Make Next button sensitive *)
             next_button#misc#set_sensitive true;
             (* Debug print *)
             List.iteri
               (fun i cluster ->
                 buffer#insert
                   ("Cluster "
                   ^ string_of_int (i + 1)
                   ^ " center: "
                   ^ GroupProject.Point.to_string cluster
                   ^ "\n");
                 auto_scroll ())
               clusters
           with e ->
             Printf.printf "Error occurred: %s\n%!" (Printexc.to_string e);
             (* Debug print *)
             buffer#insert
               ("\nError during clustering: " ^ Printexc.to_string e ^ "\n"));
          auto_scroll ()
    in

    (* Connect signals *)
    if !choice == "sample" then begin
      run_button#misc#set_sensitive true;
      file_button#misc#set_sensitive false;
      open_sample_file () (* run_button#misc#set_sensitive false *)
    end
    else if !choice == "random" then begin
      run_button#misc#set_sensitive true;
      file_button#misc#set_sensitive false;
      open_random_file () (* run_button#misc#set_sensitive false *)
    end
    else ignore (file_button#connect#clicked ~callback:open_file);
    ignore (radio_euclidean#connect#clicked ~callback:on_metric_changed);
    ignore (radio_manhattan#connect#clicked ~callback:on_metric_changed);
    ignore (run_button#connect#clicked ~callback:run_kmeans);
    ignore (window#connect#destroy ~callback:Main.quit);

    (* Initialize state *)
    (* run_button#misc#set_sensitive false; *)
    buffer#set_text
      "Welcome to CamelClass K-means Clustering\n\
       Please select a data file to begin.\n";
    auto_scroll ();
    ignore
      (next_button#connect#clicked ~callback:(fun () ->
           transition4 !current_k !current_points))
  and transition4 current_k current_points =
    (* Transition 6: Show statistics *)
    clean window;
    let stats_box = GPack.vbox ~packing:window#add () in
    let _statistics_title =
      GMisc.label
        ~markup:"<span size='50000'><b>K-Means Cluster Statistics</b></span>"
        ~selectable:true ~xalign:0.5 ~yalign:0.0 ~height:50
        ~packing:(stats_box#pack ~expand:true ~fill:true)
        ()
    in

    let clusters =
      run_custom_kmeans current_k current_points euclidean_distance
    in
    let cluster_stats_box = GPack.vbox ~packing:stats_box#add ~spacing:20 () in

    let total_points = List.length current_points in
    let _total_points_label =
      GMisc.label
        ~markup:
          ("<span size='20000'>Total Points: " ^ string_of_int total_points
         ^ "</span>")
        ~selectable:false ~xalign:0.5 ~yalign:0.5
        ~packing:(cluster_stats_box#pack ~expand:false ~fill:false)
        ()
    in

    let _cluster_count_label =
      GMisc.label
        ~markup:
          ("<span size='20000'>Number of Clusters: "
          ^ string_of_int (List.length clusters)
          ^ "</span>")
        ~selectable:false ~xalign:0.5 ~yalign:0.5
        ~packing:(cluster_stats_box#pack ~expand:false ~fill:false)
        ()
    in

    let total_variance =
      total_variation current_points clusters euclidean_distance
    in
    let _total_variance_label =
      GMisc.label
        ~markup:
          ("<span size='20000'>Total Variance: "
          ^ string_of_float total_variance
          ^ "</span>")
        ~selectable:false ~xalign:0.5 ~yalign:0.5
        ~packing:(cluster_stats_box#pack ~expand:false ~fill:false)
        ()
    in

    let _divider =
      GMisc.separator `HORIZONTAL
        ~packing:(cluster_stats_box#pack ~expand:false ~fill:true)
        ()
    in
    _divider#misc#set_size_request ~height:2 ();

    List.iteri
      (fun i cluster ->
        let cluster_points =
          let pts_in_cluster cluster clusters points dist_fn =
            List.filter
              (fun point ->
                List.for_all
                  (fun other_cluster ->
                    dist_fn point cluster <= dist_fn point other_cluster)
                  clusters)
              points
          in
          pts_in_cluster cluster clusters current_points euclidean_distance
        in
        let _cluster_label =
          GMisc.label
            ~markup:
              ("<span size='20000'><b>Cluster "
              ^ string_of_int (i + 1)
              ^ ":</b></span>")
            ~selectable:false ~xalign:0.5 ~yalign:0.5
            ~packing:(cluster_stats_box#pack ~expand:false ~fill:false)
            ()
        in
        let _cluster_size_label =
          GMisc.label
            ~markup:("Size: " ^ string_of_int (List.length cluster_points))
            ~selectable:false ~xalign:0.5 ~yalign:0.5
            ~packing:(cluster_stats_box#pack ~expand:false ~fill:false)
            ()
        in
        let _cluster_centroid_label =
          GMisc.label
            ~markup:("Centroid: " ^ GroupProject.Point.to_string cluster)
            ~selectable:false ~xalign:0.5 ~yalign:0.5
            ~packing:(cluster_stats_box#pack ~expand:false ~fill:false)
            ()
        in
        let cluster_points =
          let pts_in_cluster cluster clusters points dist_fn =
            List.filter
              (fun point ->
                List.for_all
                  (fun other_cluster ->
                    dist_fn point cluster <= dist_fn point other_cluster)
                  clusters)
              points
          in
          pts_in_cluster cluster clusters current_points euclidean_distance
        in
        let cluster_variance =
          List.fold_left
            (fun acc point ->
              let dist = euclidean_distance point cluster in
              acc +. (dist *. dist))
            0.0 cluster_points
        in
        let _cluster_variance_label =
          GMisc.label
            ~markup:("Variance: " ^ string_of_float cluster_variance)
            ~selectable:false ~xalign:0.5 ~yalign:0.5
            ~packing:(cluster_stats_box#pack ~expand:false ~fill:false)
            ()
        in
        ())
      clusters;

    let back_button = GButton.button ~label:"Back" ~packing:stats_box#pack () in
    ignore (back_button#connect#clicked ~callback:(fun () -> transition3 ()));
    let next_button = GButton.button ~label:"Next" ~packing:stats_box#pack () in
    ignore (next_button#connect#clicked ~callback:(fun () -> transition6 ()));
    window#misc#show_all ()
  and transition6 () =
    (* Transition 8: End Screen *)
    clean window;
    let controls_box =
      GPack.vbox ~width:60 ~height:400 ~packing:window#add ~spacing:20
        ~border_width:400 ()
    in

    (* Setting the font *)
    let font = GPango.font_description_from_string "Arial 20" in
    (* Start/Quit buttons *)
    let start_over_button =
      GButton.button ~label:"Start over" ~packing:controls_box#pack ()
    in
    start_over_button#misc#set_size_request ~height:70 ~width:50 ();
    start_over_button#misc#modify_font font;

    let quit_button =
      GButton.button ~label:"Quit" ~packing:controls_box#pack ()
    in
    quit_button#misc#set_size_request ~height:70 ~width:50 ();
    quit_button#misc#modify_font font;

    let start_over () =
      choice := "file";
      start ()
    in

    let quit () =
      start ();
      clean window;
      let controls_box =
        GPack.vbox ~height:400 ~width:600 ~spacing:5 ~border_width:50
          ~packing:window#add ()
      in
      controls_box#set_homogeneous true;
      let _thanks_title =
        GMisc.label
          ~markup:
            "<span size='80000'><b>Thank you for your attention! </b></span>"
          ~selectable:true ~xalign:0.5 ~yalign:0.5
          ~packing:(controls_box#pack ~expand:true ~fill:true)
          ()
      in
      let _authors_title =
        GMisc.label ~markup:"<span size='30000'><b>\n\nAuthors: </b></span>"
          ~selectable:true ~xalign:0.5 ~yalign:1.0
          ~packing:(controls_box#pack ~expand:true ~fill:true)
          ()
      in
      let thanks_box =
        GPack.hbox ~height:200 ~width:600 ~spacing:20 ~border_width:50
          ~packing:controls_box#add ()
      in

      let add_name name picture_file =
        let column =
          GPack.vbox ~spacing:5
            ~packing:(thanks_box#pack ~expand:true ~fill:true)
            ()
        in
        ignore (GMisc.label ~markup:name ~xalign:0.5 ~packing:column#add ());
        let pixbuf =
          GdkPixbuf.from_file_at_size picture_file ~width:200 ~height:200
        in
        ignore
          (GMisc.image ~pixbuf
             ~packing:(column#pack ~expand:true ~fill:true)
             ())
      in

      (* Adding names of authors *)
      let picture_file = Filename.concat cwd "data/pictures/camel1.jpeg" in
      add_name "<span size='30000'> Keti Sulamanidze </span>" picture_file;
      let divider =
        GMisc.separator `HORIZONTAL
          ~packing:(thanks_box#pack ~expand:false ~fill:true)
          ()
      in
      divider#misc#set_size_request ~height:4 ();

      let picture_file = Filename.concat cwd "data/pictures/camel2.jpeg" in
      add_name "<span size='30000'> Neha Naveen </span>" picture_file;
      let divider =
        GMisc.separator `HORIZONTAL
          ~packing:(thanks_box#pack ~expand:false ~fill:true)
          ()
      in
      divider#misc#set_size_request ~height:4 ();

      let picture_file = Filename.concat cwd "data/pictures/camel3.jpeg" in
      add_name "<span size='30000'> Ruby Penafiel-Gutierrez </span>"
        picture_file;
      let divider =
        GMisc.separator `HORIZONTAL
          ~packing:(thanks_box#pack ~expand:false ~fill:true)
          ()
      in
      divider#misc#set_size_request ~height:4 ();

      let picture_file = Filename.concat cwd "data/pictures/camel4.jpeg" in
      add_name "<span size='30000'> Samantha Vaca </span>" picture_file;
      let divider =
        GMisc.separator `HORIZONTAL
          ~packing:(thanks_box#pack ~expand:false ~fill:true)
          ()
      in
      divider#misc#set_size_request ~height:4 ();

      let picture_file = Filename.concat cwd "data/pictures/camel5.jpeg" in
      add_name "<span size='30000'> Varvara Babii </span>" picture_file;

      let quit_box =
        GPack.vbox ~height:10 ~width:10 ~spacing:5 ~border_width:150
          ~packing:controls_box#add ()
      in
      controls_box#set_homogeneous false;

      let final_quit_button =
        GButton.button ~label:"Quit"
          ~packing:(quit_box#pack ~expand:true ~fill:false)
          ()
      in
      final_quit_button#misc#set_size_request ~height:50 ~width:10 ();
      final_quit_button#misc#modify_font font;

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
  let help = clr_ Bold Ylw "help" in
  let exit = clr_ Bold Ylw "exit" in
  let reload = clr_ Bold Ylw "reload" in
  Printf.printf "%s" msg;
  Printf.printf "- %s : View all points from the CSV file.\n" display;
  Printf.printf
    "- %s : Compute distances between points using a selected metric.\n"
    distances;
  Printf.printf "- %s : Perform k-means. \n" kmeans;
  Printf.printf "- %s :  Load a new CSV file.\n" reload;
  Printf.printf "- %s : Display HELP message. \n" help;
  Printf.printf "- %s :  Exit the program. \n" exit

(* MARK: - Properties (Assurance) *)

(** [is_csv c] is whether or not [c] is a csv file. *)
let is_csv c =
  let len = String.length c in
  if len < 4 || String.sub c (len - 4) 4 <> ".csv" then begin
    Printf.printf "\nThis is not a valid csv file";
    false
  end
  else true

(** [is_dimension d] is whether or not [d] is a valid dimension. *)
let is_dimension d =
  try d > 0
  with _ ->
    Printf.printf
      "\n\
       This is an invalid coordinate: Try [1] [2] or  [N] where N is a \
       positive integer";
    false

(* MARK: - Properties (Metadata) *)

(** [get_dimension_from_csv csv] is the dimension of points in [csv]. *)
let get_dimension_from_csv csv =
  try
    let csv_data = Csv.load csv in
    let point_count = List.length csv_data in
    match csv_data with
    | row :: _ ->
        let dim = List.length row in
        if dim > 0 then Some (dim, point_count) else None
    | [] -> None
  with _ -> None

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

(** [prompt_for_distfn ()] is the distance function the user chooses. *)
let prompt_for_distfn () =
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
  distance_metric

(** [distances p dim dist_metric] is the list of tuples with distance(s)
    calculated under [dist_metric] between the points [p] in csv and a dummy
    point. *)
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
    ... n and a dummy point based on a distance metric the user chooses, as well
    as prompts the user to save the data in the data directory. *)
let print_distances points dim =
  let distance_metric = prompt_for_distfn () in
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
(* -------------------------------------------------------------------------- *)
(* KNN Display Logic *)
(* -------------------------------------------------------------------------- *)

(** [prompt_for_point dim] is the point the user wants to classify into a
    cluster using knn after running kmeans. *)
let prompt_for_point dim =
  let prompt =
    clr_ Und Wht
      "Enter the %dD coordinate you'd like to classify \
       (comma-separated-values; i.e. x1, ..., xk):"
      dim
  in
  let err_msg = clr_ Reg Red "Invalid input." in
  Printf.printf "\n%s " prompt;
  try
    let point =
      String.split_on_char ',' (read_line ()) |> List.map float_of_string
    in
    create dim point
  with _ ->
    Printf.printf "%s Defaulting to (1.0, ..., 1.0).\n" err_msg;
    create dim (List.init dim (fun _ -> 1.0))

(** [prompt_for_k_knn k_max] is the k-value from 1 to [k_max] that represents
    how many nearest neighbors will be considered in running knn. *)
let prompt_for_k_knn k_max =
  let prompt =
    clr_ Und Wht "Enter k, the number of neighbors to consider (1-%d):" k_max
  in
  let err_msg = clr_ Reg Red "Invalid input." in
  Printf.printf "\n%s " prompt;
  try
    let k_val = int_of_string (read_line ()) in
    k_val
  with _ ->
    Printf.printf "%s Defaulting to k = 1.\n" err_msg;
    1

(** [run_knn_ui clusters dim dist_fn] takes in user input for what point must be
    classified and runs knn to classify the point and inform the user of the
    conclusion, given the user provides valid data, otherwise default values
    shall be used. *)
let run_knn_ui clusters dim dist_fn =
  let point = prompt_for_point dim in
  let k = prompt_for_k_knn (List.length clusters) in
  let labeled_clusters =
    List.mapi (fun i cluster -> (cluster, string_of_int i)) clusters
  in
  let classification = classify k point labeled_clusters in
  let cluster_index = int_of_string classification + 1 in
  let class_msg =
    clr_ Reg Grn "The point %s belongs to Cluster %d.\n" (to_string point)
      cluster_index
  in
  if cluster_index >= 0 then Printf.printf "\n%s" class_msg
  else Printf.printf "\nERROR: Could not classify point.\n"

(* -------------------------------------------------------------------------- *)
(* Kmeans Display Logic *)
(* -------------------------------------------------------------------------- *)

(** [prompt_for_k point_ct] is the k value to use for running kmeans. *)
let prompt_for_k point_ct =
  let one_cluster_msg =
    clr_ Reg Grn
      "Your data only contains one point, so it will be clustered into one \
       cluster."
  in
  if point_ct = 1 then begin
    Printf.printf "%s" one_cluster_msg;
    1
  end
  else begin
    let inform_msg =
      clr_ Reg Wht
        "\n\
         Now you will specify a value for k, the number of clusters. Your file \
         contains %d points, so k must be between 1 and %d, inclusive.\n\n"
        point_ct point_ct
    in
    let prompt_msg = clr_ Und Ylw "Enter a k value for clustering:" in
    let err_msg = clr_ Reg Red "Invalid input." in
    Printf.printf "%s" inform_msg;
    Printf.printf "%s " prompt_msg;
    let input = read_line () in
    try
      begin
        let k = int_of_string input in
        if k > point_ct then failwith "Bad k value"
        else
          let success_msg =
            clr_ Reg Grn
              "\nGreat! Your data will be clustered into %d clusters.\n" k
          in
          Printf.printf "%s\n" success_msg;
          k
      end
    with Failure _ ->
      Printf.printf "%s Defaulting to k = 2\n\n" err_msg;
      2
  end

(** [pts_in_cluster cluster clusters points dist_fn] is the filtered [points]
    list containing points closer to [cluster] than to any other cluster in
    [clusters], as determined by [dist_fn]. *)
let pts_in_cluster cluster clusters points dist_fn =
  List.filter
    (fun point ->
      List.for_all
        (fun other_cluster ->
          dist_fn point cluster <= dist_fn point other_cluster)
        clusters)
    points

(** [display_clusters clusters points dist_fn] displays the [points] in
    [clusters]. *)
let display_clusters clusters points dist_fn =
  List.iteri
    (fun cluster_ind cluster ->
      Printf.printf "\nCluster %d:\n" (cluster_ind + 1);
      let cluster_points = pts_in_cluster cluster clusters points dist_fn in
      List.iter
        (fun point -> Printf.printf "- %s\n" (to_string point))
        cluster_points)
    clusters

(** [prompt_to_show_clusters clusters points dist_fn] asks the user if they want
    to see their clustered data. *)
let prompt_to_show_clusters clusters points dist_fn =
  let prompt_msg =
    clr_ Bold Ylw "\nWould you like to see the clustered data? (yes/no): "
  in
  Printf.printf "%s" prompt_msg;
  let input = String.lowercase_ascii (read_line ()) in
  if input = "yes" then display_clusters clusters points dist_fn else ()

(** [save_clusters_to_csv clusters points dist_fn k] saves cluster data to a csv
    file in the data directory. *)
let save_clusters_to_csv clusters points dist_fn k =
  let file_name = Printf.sprintf "./data/clusters_k%d.csv" k in
  let clr_file_name = clr_ Reg Grn "%s" file_name in
  let oc = open_out file_name in

  Printf.fprintf oc "Cluster,Point\n";
  List.iteri
    (fun cluster_index cluster ->
      let cluster_points = pts_in_cluster cluster clusters points dist_fn in
      List.iter
        (fun point ->
          Printf.fprintf oc "%d,%s\n" (cluster_index + 1) (to_string point))
        cluster_points)
    clusters;

  close_out oc;
  Printf.printf "Data saved to %s\n" clr_file_name

(** [ask_to_save_clusters clusters points dist_fn k] asks the user if they want
    to save their cluster data, and does so if they answer yes. *)
let ask_to_save_clusters clusters points dist_fn k =
  let prompt_msg =
    clr_ Bold Ylw
      "\nDo you want to save the cluster data to a CSV file? (yes/no): "
  in
  Printf.printf "%s" prompt_msg;
  let input = String.lowercase_ascii (read_line ()) in
  if input = "yes" then save_clusters_to_csv clusters points dist_fn k else ()

(** [prompt_to_classify clusters dim dist_fn] asks the user whether or not
    they'd like to classify a point using knn into a cluster resultant from
    running kmeans. *)
let prompt_to_classify clusters dim dist_fn =
  let prompt =
    clr_ Bold Ylw
      "\n\
       Would you like to classify a point into one of the clusters using kNN? \
       (yes/no): "
  in
  Printf.printf "%s" prompt;
  let classify_input = String.lowercase_ascii (read_line ()) in
  if classify_input = "yes" then run_knn_ui clusters dim dist_fn

(** [run_kmeans_ui csv dim point_count] performs k-means clustering on the
    dataset in [csv] and handles user interaction with processing and saving the
    data. *)
let run_kmeans_ui csv dim point_count =
  let points = CsvReaderImpl.read_points dim csv in
  let dist_fn =
    let err_msg = clr_ Reg Red "Invalid metric." in
    match prompt_for_distfn () with
    | "euclidean" -> euclidean_distance
    | "manhattan" -> manhattan_distance
    | _ ->
        Printf.printf "%s Defaulting to euclidean distance function.\n" err_msg;
        euclidean_distance
  in
  let k = prompt_for_k point_count in
  let progress_msg = Printf.sprintf "Running k-means with k = %d" k in
  show_progress_bar progress_msg;
  try
    let complete_msg = clr_ Und Ylw "Cluster centers:" in
    let clusters = run_custom_kmeans k points dist_fn in
    Printf.printf "%s\n" complete_msg;

    List.iteri
      (fun i cluster ->
        Printf.printf "Cluster %d : %s\n" (i + 1) (to_string cluster))
      clusters;

    prompt_to_show_clusters clusters points dist_fn;
    ask_to_save_clusters clusters points dist_fn k;
    prompt_to_classify clusters dim dist_fn
  with _ ->
    Printf.printf "%s unable to cluster [%s]\n" (clr_ Bold Red "Error:") csv

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

(** [attain_dimension csv] is the tuple containing the tuple with [csv] and its
    corresponding dimension. Informs the user of the dimension of the points in
    [csv]. *)
let rec attain_dimension () =
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
  | Some (dim, point_count) ->
      Printf.printf "%s%d\n" inform_msg dim;
      (csv, dim, point_count)
  | None ->
      Printf.printf "%s\n" err_msg;
      attain_dimension ()

(* -------------------------------------------------------------------------- *)
(* Execution Logic *)
(* -------------------------------------------------------------------------- *)

(** [command_handler csv dim] is the handler of the program based on the user's
    input. *)
let rec command_handler csv dim point_count =
  let ty_msg = clr_ Bold Magenta "Thank you for choosing CamelClass !!" in
  Printf.printf "%s" (clr_ Bold Cyan "\nEnter a command ('help' for options): ");
  match String.lowercase_ascii (read_line ()) with
  | "points" ->
      print_points csv dim;
      command_handler csv dim point_count
  | "dists" ->
      print_distances csv dim;
      command_handler csv dim point_count
  | "kmeans" ->
      run_kmeans_ui csv dim point_count;
      command_handler csv dim point_count
  | "reload" ->
      let (new_csv : string), (new_dimension : int), (new_point_count : int) =
        attain_dimension ()
      in
      command_handler new_csv new_dimension new_point_count
  | "help" ->
      print_help ();
      command_handler csv dim point_count
  | "exit" ->
      Printf.printf "\n%s\n\n%s" ty_msg
        (clr_ Bold Grn "Exiting program. Goodbye!\n")
  | _ ->
      Printf.printf "%s" (clr_ Bold Red "Invalid command. Try again.\n");
      command_handler csv dim point_count

(** [run_io_mode ()] deals with program logic. *)
let run_io_mode () =
  let welcome_to_io_msg =
    clr_ Reg Grn "\nYou are now in CamelClass I/O mode !!"
  in
  Printf.printf "%s\n" welcome_to_io_msg;
  let csv_file, dimension, point_count = attain_dimension () in
  command_handler csv_file dimension point_count

(** Main Function *)
let () =
  let len = Array.length Sys.argv in
  let title = clr_ Bold Ylw "CamelClass: Classifications Demystified" in
  let debrief =
    "is a classification tool designed to simplify working with datasets in \
     OCaml."
  in
  let authors_title =
    clr_ Und Grn "By: Keti S., Neha N., Ruby P.G, Samantha V., Varvara B."
  in
  let error_msg =
    clr_ Reg Red
      "Error: You have provided too many arguments. Try running something \
       like: "
  in
  let err_bad_csv_msg =
    clr_ Reg Red
      "Error: The CSV you've provided is in an invalid format. Please rerun \
       the program once you're file is in a valid format."
  in
  let usage_msg = clr_ Reg Grn "$ dune exec bin/main.exe\n" in
  let invld_choice_msg = clr_ Reg Ylw "Invalid Input. " in
  try
    if len > 1 then Printf.printf "%s\n%s" error_msg usage_msg
    else begin
      Printf.printf "%s\n" welcome_ascii;
      Printf.printf "%s\n\n" authors_title;
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
  with
  | Sys_error _ ->
      Printf.printf "%s"
        (clr_ Bold Red
           "That was incorrect/invalid input. Please rerun the program and \
            provide valid prompts.")
  | Failure e ->
      if e = "Bad Points CSV" then Printf.printf "\n%s" err_bad_csv_msg
      else failwith e

open Point
open GObj
open Gtk

(* Predefined colors for clusters *)
let predefined_colors = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12 |]

let create_1d_graph filename points clusters =
  let max_x = ref (-.max_float) in
  let min_x = ref max_float in

  (* Convert 1D points list to Array *)
  let x = Array.make (List.length points) 0.0 in

  (* Create array from existing points and find bounds *)
  for i = 0 to List.length points - 1 do
    let coords = Point.get_coordinates (List.nth points i) in
    let curr_x = List.nth coords 0 in
    x.(i) <- curr_x;
    min_x := min !min_x curr_x;
    max_x := max !max_x curr_x
  done;

  let y = Array.make (Array.length x) 0.0 in

  let x_clusters = Array.make (List.length clusters) 0.0 in
  for i = 0 to List.length clusters - 1 do
    let coords = Point.get_coordinates (List.nth clusters i) in
    x_clusters.(i) <- List.nth coords 0
  done;
  let y_clusters = Array.make (Array.length x_clusters) 0.0 in

  let open Plplot in
  plsdev "png";
  plsfnam filename;
  plinit ();

  plcol0 15;
  let range = !max_x -. !min_x in
  plenv (!min_x -. (0.1 *. range)) (!max_x +. (0.1 *. range)) (-0.1) 0.1 0 0;
  plcol0 15;
  pllab "X-axis" "" "1D Graph";

  (* Plot normal points *)
  plcol0 2;
  plschr 0.0 1.0;
  plpoin x y 9;

  (* Plot cluster points *)
  plcol0 3;
  plpoin x_clusters y_clusters 5;

  plcol0 15;
  plpoin x_clusters y_clusters 5;

  plend ()

let create_2d_graph filename points clusters =
  let max_x = ref (-.max_float) in
  let min_x = ref max_float in
  let max_y = ref (-.max_float) in
  let min_y = ref max_float in

  let x = Array.make (List.length points) 0.0 in
  let y = Array.make (List.length points) 0.0 in

  for i = 0 to List.length points - 1 do
    let coords = Point.get_coordinates (List.nth points i) in
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
  plsdev "png";
  plsfnam filename;
  plinit ();

  let x_range = !max_x -. !min_x in
  let y_range = !max_y -. !min_y in

  plcol0 15;
  plenv
    (!min_x -. (0.1 *. x_range))
    (!max_x +. (0.1 *. x_range))
    (!min_y -. (0.1 *. y_range))
    (!max_y +. (0.1 *. y_range))
    0 0;
  plcol0 15;
  pllab "X-axis" "Y-axis" "2D Graph";

  (* Plot points for each cluster *)
  List.iteri
    (fun i cluster_point ->
      let color_index =
        predefined_colors.(i mod Array.length predefined_colors)
      in
      plcol0 color_index;

      let cluster_points =
        List.filter
          (fun p ->
            let curr_dist = Point.euclidean_distance p cluster_point in
            List.for_all
              (fun other_cluster ->
                curr_dist <= Point.euclidean_distance p other_cluster)
              clusters)
          points
      in

      let x_cluster_points =
        Array.of_list
          (List.map
             (fun p -> List.nth (Point.get_coordinates p) 0)
             cluster_points)
      in
      let y_cluster_points =
        Array.of_list
          (List.map
             (fun p -> List.nth (Point.get_coordinates p) 1)
             cluster_points)
      in

      plpoin x_cluster_points y_cluster_points 9;

      (* Plot cluster centers *)
      let center_coords = Point.get_coordinates cluster_point in
      let cx = List.nth center_coords 0 in
      let cy = List.nth center_coords 1 in
      plcol0 15;
      plpoin [| cx |] [| cy |] 5)
    clusters;

  plend ()

let create_3d_graph filename points clusters =
  let max_x = ref (-.max_float) in
  let min_x = ref max_float in
  let max_y = ref (-.max_float) in
  let min_y = ref max_float in
  let max_z = ref (-.max_float) in
  let min_z = ref max_float in

  let x = Array.make (List.length points) 0.0 in
  let y = Array.make (List.length points) 0.0 in
  let z = Array.make (List.length points) 0.0 in

  for i = 0 to List.length points - 1 do
    let coords = Point.get_coordinates (List.nth points i) in
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

  let open Plplot in
  plsdev "png";
  plsfnam filename;
  plinit ();

  let x_range = !max_x -. !min_x in
  let y_range = !max_y -. !min_y in
  let z_range = !max_z -. !min_z in

  let max_range = max x_range (max y_range z_range) in
  let scale_factor = 5.0 in

  plcol0 0;
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

  plcol0 15;
  plbox3 "bnstu" "X-axis" 0.0 0 "bnstu" "Y-axis" 0.0 0 "bcdmnstuv" "Z-axis" 0.0
    0;

  plcol0 2;
  plpoin3 x y z 9;

  (* Plot cluster centers *)
  plcol0 15;
  let x_clusters = Array.make (List.length clusters) 0.0 in
  let y_clusters = Array.make (List.length clusters) 0.0 in
  let z_clusters = Array.make (List.length clusters) 0.0 in
  List.iteri
    (fun i cluster ->
      let coords = Point.get_coordinates cluster in
      x_clusters.(i) <- List.nth coords 0;
      y_clusters.(i) <- List.nth coords 1;
      z_clusters.(i) <- List.nth coords 2)
    clusters;
  plpoin3 x_clusters y_clusters z_clusters 5;

  plend ()

let plot_graph view points clusters () =
  let filename = "graph.png" in
  match view with
  | "1D" -> create_1d_graph filename points clusters
  | "2D" -> create_2d_graph filename points clusters
  | "3D" -> create_3d_graph filename points clusters
  | _ -> failwith "Unsupported visualization type"

let create_plot_window window graph_box image_path =
  GMisc.image ~file:image_path ~packing:graph_box#add ()

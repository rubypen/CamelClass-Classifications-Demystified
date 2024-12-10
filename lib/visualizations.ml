open Point
open GObj
open Gtk

type color_array = (string * (float * float * float)) array

let create_1d_graph filename points clusters colors =
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

  let x_clusters = Array.make (List.length clusters) 0.0 in
  for i = 0 to List.length clusters - 1 do
    let coords = Point.get_coordinates (List.nth clusters i) in
    x_clusters.(i) <- List.nth coords 0
  done;

  let open Plplot in
  plsdev "png";
  plsfnam filename;
  plinit ();

  (* Assign the custom colors to the plot *)
  Array.iteri
    (fun i (_, (r, g, b)) ->
      plscol0 (i + 1)
        (int_of_float (r *. 255.))
        (int_of_float (g *. 255.))
        (int_of_float (b *. 255.)))
    colors;

  (* Define a new color, white, after the custom colors *)
  plscol0 (Array.length colors + 1) 255 255 255;

  (* Make axis color white *)
  plcol0 (Array.length colors + 1);

  let range = !max_x -. !min_x in
  plenv (!min_x -. (0.1 *. range)) (!max_x +. (0.1 *. range)) (-0.1) 0.1 0 0;
  pllab "X-axis" "" "1D Graph";

  (* Plot points for each cluster *)
  List.iteri
    (fun i cluster_point ->
      let color_index = (i mod Array.length colors) + 1 in
      plcol0 color_index;

      (* Filter points belonging to the current cluster *)
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

      (* Plot cluster points *)
      let y_fixed = Array.make (Array.length x_cluster_points) 0.0 in
      plpoin x_cluster_points y_fixed 9;

      (* Plot cluster centers *)
      let center_coords = Point.get_coordinates cluster_point in
      let cx = List.nth center_coords 0 in
      plcol0 (Array.length colors + 1);
      plpoin [| cx |] [| 0.0 |] 5)
    clusters;

  plend ()

let create_2d_graph filename points clusters colors =
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

  (* Assign the custom colors to the plot *)
  Array.iteri
    (fun i (_, (r, g, b)) ->
      plscol0 (i + 1)
        (int_of_float (r *. 255.))
        (int_of_float (g *. 255.))
        (int_of_float (b *. 255.)))
    colors;

  (* Define a new color, white, after the custom colors *)
  plscol0 (Array.length colors + 1) 255 255 255;

  (* Make axis color white *)
  plcol0 (Array.length colors + 1);

  let x_range = !max_x -. !min_x in
  let y_range = !max_y -. !min_y in

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
      let color_index = (i mod Array.length colors) + 1 in
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
      plcol0 (Array.length colors + 1);
      plpoin [| cx |] [| cy |] 5)
    clusters;

  plend ()

let create_3d_graph filename points clusters colors =
  let max_x = ref (-.max_float) and min_x = ref max_float in
  let max_y = ref (-.max_float) and min_y = ref max_float in
  let max_z = ref (-.max_float) and min_z = ref max_float in

  (* Prepare points arrays *)
  let x = Array.make (List.length points) 0.0 in
  let y = Array.make (List.length points) 0.0 in
  let z = Array.make (List.length points) 0.0 in

  (* Fill points and update bounds *)
  List.iteri
    (fun i p ->
      let coords = Point.get_coordinates p in
      let curr_x, curr_y, curr_z =
        (List.nth coords 0, List.nth coords 1, List.nth coords 2)
      in
      x.(i) <- curr_x;
      y.(i) <- curr_y;
      z.(i) <- curr_z;
      min_x := min !min_x curr_x;
      max_x := max !max_x curr_x;
      min_y := min !min_y curr_y;
      max_y := max !max_y curr_y;
      min_z := min !min_z curr_z;
      max_z := max !max_z curr_z)
    points;

  let open Plplot in
  plsdev "png";
  plsfnam filename;
  plinit ();

  (* Assign custom colors to the plot *)
  Array.iteri
    (fun i (_, (r, g, b)) ->
      plscol0 (i + 1)
        (int_of_float (r *. 255.))
        (int_of_float (g *. 255.))
        (int_of_float (b *. 255.)))
    colors;

  let x_range = !max_x -. !min_x in
  let y_range = !max_y -. !min_y in
  let z_range = !max_z -. !min_z in
  (*let max_range = max x_range (max y_range z_range) in*)
  let scale_factor = 5.0 in

  (* Define a new color, black, for the 2D axes *)
  plscol0 (Array.length colors + 1) 0 0 0;
  (* Define a new color, white, for the 3D axes and clusters *)
  plscol0 (Array.length colors + 2) 255 255 255;

  plcol0 (Array.length colors + 1);

  (* 3D Plot setup *)
  plenv
    (-0.15 *. x_range *. scale_factor)
    (0.15 *. x_range *. scale_factor)
    (-0.15 *. y_range *. scale_factor)
    (0.15 *. y_range *. scale_factor)
    0 0;

  plw3d (1.0 *. scale_factor) (1.0 *. scale_factor) (1.0 *. scale_factor)
    (!min_x -. (0.15 *. x_range))
    (!max_x +. (0.15 *. x_range))
    (!min_y -. (0.15 *. y_range))
    (!max_y +. (0.15 *. y_range))
    (!min_z -. (0.15 *. z_range))
    (!max_z +. (0.15 *. z_range))
    30.0 30.0;

  plcol0 (Array.length colors + 2);
  plbox3 "bnstu" "X-axis" 0.0 0 "bnstu" "Y-axis" 0.0 0 "bcdmnstuv" "Z-axis" 0.0
    0;

  (* Plot points *)
  plpoin3 x y z 9;

  (* Plot clusters dynamically *)
  List.iteri
    (fun i cluster ->
      let color_index = (i mod Array.length colors) + 1 in
      plcol0 color_index;

      let cluster_points =
        List.filter
          (fun p ->
            let curr_dist = Point.euclidean_distance p cluster in
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
      let z_cluster_points =
        Array.of_list
          (List.map
             (fun p -> List.nth (Point.get_coordinates p) 2)
             cluster_points)
      in

      (* Plot cluster points *)
      plpoin3 x_cluster_points y_cluster_points z_cluster_points 9;

      (* Plot cluster centers *)
      let center_coords = Point.get_coordinates cluster in
      let cx = List.nth center_coords 0 in
      let cy = List.nth center_coords 1 in
      let cz = List.nth center_coords 2 in
      plcol0 (Array.length colors + 2);
      plpoin3 [| cx |] [| cy |] [| cz |] 5)
    clusters;

  plend ()

let plot_graph view points clusters colors () =
  let filename = "graph.png" in
  match view with
  | "1D" -> create_1d_graph filename points clusters colors
  | "2D" -> create_2d_graph filename points clusters colors
  | "3D" -> create_3d_graph filename points clusters colors
  | _ -> failwith "Unsupported visualization type"

let create_plot_window window graph_box image_path =
  GMisc.image ~file:image_path ~packing:graph_box#add ()

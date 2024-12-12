open Point
open Kmeans

let sort_by_distance point points_list =
  List.sort
    (fun (p1, _) (p2, _) ->
      compare (euclidean_distance point p1) (euclidean_distance point p2))
    points_list

let rec take n lst =
  match lst with
  | [] -> []
  | _ when n <= 0 -> []
  | x :: xs -> x :: take (n - 1) xs

let k_nearest_neighbors k point points_list =
  if k <= 0 then invalid_arg "k must be positive"
  else if points_list = [] then invalid_arg "No labeled points provided"
  else
    let sorted_points = sort_by_distance point points_list in
    take k sorted_points

let classify k point points_list =
  let neighbors = k_nearest_neighbors k point points_list in
  let labels = List.map snd neighbors in
  let counts =
    List.fold_left
      (fun acc label ->
        let count = try List.assoc label acc with Not_found -> 0 in
        (label, count + 1) :: List.remove_assoc label acc)
      [] labels
  in
  let counts =
    List.sort
      (fun (label1, count1) (label2, count2) -> compare count2 count1)
      counts
  in
  let max_count = snd (List.hd counts) in
  let tied_labels =
    List.filter (fun (_, count) -> count = max_count) counts |> List.map fst
  in
  let best_label =
    match tied_labels with
    | [ label ] -> label
    | _ ->
        let nearest_label = snd (List.hd neighbors) in
        if List.mem nearest_label tied_labels then nearest_label
        else List.hd tied_labels
  in
  best_label

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
  if k <= 0 then invalid_arg "k\n   must be positive"
  else if points_list = [] then invalid_arg "No labeled\n   points provided"
  else
    let sorted_points = sort_by_distance point points_list in
    take k sorted_points

(* let classify k point points_list = let neighbors = k_nearest_neighbors k
   point points_list in let labels = List.map snd neighbors in let counts =
   List.fold_left (fun acc label -> let count = try List.assoc label acc with
   Not_found -> 0 in (label, count + 1) :: List.remove_assoc label acc) []
   labels in List.fold_left (fun (best_label, best_count) (label, count) -> if
   count > best_count then (label, count) else (best_label, best_count)) ("", 0)
   counts |> fst *)

(* let classify k point points_list = let neighbors = k_nearest_neighbors k
   point points_list in print_endline "Neighbors used for classification:";
   List.iter (fun (_, label) -> print_endline label) neighbors;

   let labels = List.map snd neighbors in let counts = List.fold_left (fun acc
   label -> let count = try List.assoc label acc with Not_found -> 0 in (label,
   count + 1) :: List.remove_assoc label acc) [] labels in

   print_endline "Label counts:"; List.iter (fun (label, count) -> Printf.printf
   "%s: %d\n" label count) counts;

   let best_label, _ = List.fold_left (fun (best_label, best_count) (label,
   count) -> if count > best_count then (label, count) else (best_label,
   best_count)) ("", 0) counts in best_label *)

let classify k point points_list =
  let neighbors = k_nearest_neighbors k point points_list in
  let labels = List.map snd neighbors in

  (* Count occurrences of each label *)
  let counts =
    List.fold_left
      (fun acc label ->
        let count = try List.assoc label acc with Not_found -> 0 in
        (label, count + 1) :: List.remove_assoc label acc)
      [] labels
  in

  (* Sort counts by count (descending), break ties lexicographically *)
  let counts =
    List.sort
      (fun (label1, count1) (label2, count2) ->
        if count1 = count2 then String.compare label1 label2
        else compare count2 count1)
      counts
  in

  let best_label, _ = List.hd counts in
  best_label

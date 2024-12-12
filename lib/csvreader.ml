open Point

let read_points n (file_path : string) : Point.t list =
  let csv = Csv.load file_path in
  List.map
    (fun row ->
      let lst = ref [] in
      for i = 0 to List.length row - 1 do
        lst := float_of_string (List.nth row (List.length row - 1 - i)) :: !lst
      done;
      Point.create n !lst)
    csv

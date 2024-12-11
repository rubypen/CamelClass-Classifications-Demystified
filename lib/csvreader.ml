open Point
open Csv

(* Module type signature for CsvReader *)
module type CsvReader = sig
  val read_points : int -> string -> Point.t list
  (** [read_points n str] creates a list of points with [n] coordinates each
      from csv file with a name [str]. Requires: [lst] is a name of csv file. *)
end

module CsvReaderImpl : CsvReader = struct
  let read_points n (file_path : string) : Point.t list =
    let csv = Csv.load file_path in
    List.map
      (fun row ->
        let lst = ref [] in
        for i = 0 to List.length row - 1 do
          lst :=
            float_of_string (List.nth row (List.length row - 1 - i)) :: !lst
        done;
        Point.create n !lst)
      csv
end

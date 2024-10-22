open Point
open Csv

module type CsvReader = sig
  val read_points : string -> Point1D.t list
end

module CsvReaderImpl : CsvReader = struct
  let read_points (file_path : string) : Point1D.t list =
    let csv = Csv.load file_path in
    List.map
      (fun row ->
        match row with
        | [ coordinate_as_str ] ->
            let coord = float_of_string coordinate_as_str in
            Point1D.create coord
        | _ -> failwith "Invalid CSV format")
      csv
end

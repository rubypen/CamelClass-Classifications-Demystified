open Point
open Csv

(* Module type signature for CsvReader *)
module type CsvReader = sig
  val read_points_1d : string -> Point1D.t list
  (** [read_points_1d str] creates a list of points from csv file with a name
      [str]. Requires: [lst] is a name of csv file. *)

  val read_points_2d : string -> Point2D.t list
  (** [read_points_2d str] creates a list of points from csv file with a name
      [str]. Requires: [lst] is a name of csv file. *)

  val read_points_3d : string -> Point3D.t list
  (** [read_points_3d str] creates a list of points from csv file with a name
      [str]. Requires: [lst] is a name of csv file. *)
end

module CsvReaderImpl : CsvReader = struct
  let read_points_1d (file_path : string) : Point1D.t list =
    let csv = Csv.load file_path in
    List.map
      (fun row ->
        match row with
        | [ coordinate_as_str ] ->
            let coord = float_of_string coordinate_as_str in
            Point1D.create [ coord ]
        | _ -> failwith "Invalid CSV format")
      csv

  let read_points_2d (file_path : string) : Point2D.t list =
    let csv = Csv.load file_path in
    List.map
      (fun row ->
        match row with
        | [ x_str; y_str ] ->
            let x = float_of_string x_str in
            let y = float_of_string y_str in
            Point2D.create [ x; y ]
        | _ -> failwith "Invalid CSV format")
      csv

  let read_points_3d (file_path : string) : Point3D.t list =
    let csv = Csv.load file_path in
    List.map
      (fun row ->
        match row with
        | [ x_str; y_str; z_str ] ->
            let x = float_of_string x_str in
            let y = float_of_string y_str in
            let z = float_of_string z_str in
            Point3D.create [ x; y; z ]
        | _ -> failwith "Invalid CSV format")
      csv
end

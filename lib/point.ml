(* Module type signature for Point *)
module type Point = sig
  type t

  val create : float -> t
  val get_coordinate : t -> float
  val euclidean_distance : t -> t -> float
end

(* Implementation for 1 Dimensional Point *)
module Point1D : Point = struct
  type t = float

  let create (x : float) : t = x
  let get_coordinate (p : t) : float = p
  let euclidean_distance (p1 : t) (p2 : t) : float = abs_float (p1 -. p2)
end

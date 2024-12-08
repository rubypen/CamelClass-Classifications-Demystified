open GObj

val create_1d_graph : string -> Point.t list -> Point.t list -> unit
(** [create_1d_graph filename points clusters] creates a 1D visualization of
    points and their clusters, saving to filename *)

val create_2d_graph : string -> Point.t list -> Point.t list -> unit
(** [create_2d_graph filename points clusters] creates a 2D visualization of
    points and their clusters, saving to filename *)

val create_3d_graph : string -> Point.t list -> Point.t list -> unit
(** [create_3d_graph filename points clusters] creates a 3D visualization of
    points and their clusters, saving to filename *)

val plot_graph : string -> Point.t list -> Point.t list -> unit -> unit
(** [plot_graph view points clusters ()] creates a visualization based on the
    dimensionality specified by view ("1D", "2D", or "3D") *)

val create_plot_window : GWindow.window -> GPack.box -> string -> GMisc.image
(** [create_plot_window window graph_box image_path] creates and returns an
    image widget displaying the plot *)

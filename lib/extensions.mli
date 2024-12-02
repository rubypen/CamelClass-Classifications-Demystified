(** The type representing a color. *)
type color =
  | Red
  | Grn
  | Ylw
  | Blue
  | Magenta
  | Cyan
  | Wht

(** The type representing text styling options. *)
type style =
  | Bold
  | Und
  | Reg

val clr_ : style -> color -> ('a, unit, string) format -> 'a
(** [clr_ s c str] is the string [str] with style [s] and color [clr]. *)

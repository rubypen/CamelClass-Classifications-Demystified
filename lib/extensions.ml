type color =
  | Red
  | Grn
  | Ylw
  | Blue
  | Magenta
  | Cyan
  | Wht

type style =
  | Bold
  | Und
  | Reg

let clr_ s c str =
  let clr =
    match c with
    | Red -> ANSITerminal.red
    | Grn -> ANSITerminal.green
    | Ylw -> ANSITerminal.yellow
    | Blue -> ANSITerminal.blue
    | Magenta -> ANSITerminal.magenta
    | Cyan -> ANSITerminal.cyan
    | _ -> ANSITerminal.white
  in
  match s with
  | Bold -> ANSITerminal.sprintf [ Bold; clr; ANSITerminal.on_black ] str
  | Und -> ANSITerminal.sprintf [ Underlined; clr; ANSITerminal.on_black ] str
  | Reg -> ANSITerminal.sprintf [ clr; ANSITerminal.on_black ] str

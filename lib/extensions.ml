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

let welcome_ascii =
  clr_ Bold Grn
    "\n\
    \ __        _______ _     ____ ___  __  __ _____   _        \n\
    \ \\ \\      / / ____| |   / ___/ _ \\|  \\/  | ____| | |_ ___  \n\
    \  \\ \\ /\\ / /|  _| | |  | |  | | | | |\\/| |  _|   | __/ _ \\ \n\
    \   \\ V  V / | |___| |__| |__| |_| | |  | | |___  | || (_) |\n\
    \   _\\_/\\_/  |_____|_____\\____\\___/|_|  |_|_____|  \\__\\___/ \n\
    \  / ___|__ _ _ __ ___   ___| |/ ___| | __ _ ___ ___  | |   \n\
    \ | |   / _` | '_ ` _ \\ / _ \\ | |   | |/ _` / __/ __| | |   \n\
    \ | |__| (_| | | | | | |  __/ | |___| | (_| \\__ \\__ \\ |_|   \n\
    \  \\____\\__,_|_| |_| |_|\\___|_|\\____|_|\\__,_|___/___/ (_)   \n"

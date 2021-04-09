open Graphics

exception Entree_inconnue of string

let y_high_left_corner() = Graphics.size_y() - 20
let x_high_left_corner() = 10
let y_bottom_right_corner() = 20
let x_bottom_right_corner() = Graphics.size_x() - 10

let default_color = rgb 0 0 0

let black = Graphics.black
let white = Graphics.white
let red = Graphics.red
let green = Graphics.green
let blue = Graphics.blue
let yellow = Graphics.yellow
let cyan = Graphics.cyan
let magenta = Graphics.magenta

let rgb = Graphics.rgb

type position = int * int

(* Description du style d'un texte *)
(* Description du style d'un texte *)
type text_style =
  { color: Graphics.color;
    size:int; (* 1 = grand titre (H1), 5 = petit titre (H5), -1=texte normal.*)
    boldness : bool; (* gras ou non *)
    italicness : bool; (* italique ou non *)
    underlined : bool;(* souligné ou non ATTENTION NON PRIS EN COMPTE par [draw_action]*)
    centered : bool;(* centré ou non ATTENTION NON PRIS EN COMPTE par [draw_action]  *)
    interligne:int; (* nombre de pixels à sauter entre chaque ligne. *)
  }

type text_action =
  { texte: string; style: text_style; pos: position}

type line_action =
  { line_from: position; line_to: position; line_color:color}

(* Une action d'affichage décrit l'affichage d'un morceau de texte
   dans un certain style à partir d'une certaine position. Chaque
   action sera dessinée sans saut de ligne. Il faut donc découper un
   texte en plusieurs drawing_action pour l'afficher sur plusieurs
   lignes. *)
type drawing_action =
  | Text of text_action
  | Line of line_action

let pr_if_true s x = if x then (s ^" = true" ^"; ") else ""

let pr_style s = "{ " ^ (pr_if_true "boldness" s.boldness) ^ (pr_if_true "italicness" s.italicness)
               ^ (pr_if_true "underlined" s.underlined) ^ (pr_if_true "centered" s.centered)
               ^ Printf.sprintf "color= 0x%X" s.color ^ Printf.sprintf "; size = %d" s.size
               ^ " }"
let pr_drawing_action a =
match a with
| Text s -> "Text { " ^ "position = " ^ "("^ string_of_int (fst s.pos)
          ^ "," ^ string_of_int (snd s.pos) ^"); " ^ "texte = \"" ^ s.texte
          ^ "\"; style = "^ pr_style s.style ^ "}"
| Line l -> Printf.sprintf "Line { line_from = (%d,%d); line_to = (%d,%d); line_color = 0x%X }"
              (fst l.line_from) (snd l.line_from)
              (fst l.line_to) (snd l.line_to) l.line_color

let default_style =
  { size = -1; boldness= false; italicness=false; color=default_color;
    underlined = false; interligne=18; centered=false}

let size_to_pixels n =
  match n with
  | -1 -> 12 | 1 -> 24 | 2 -> 20 | 3 -> 18 | 4 -> 14 | 5 -> 12
  | _ -> failwith ("Taille de fonte invalide:"^ string_of_int n^".")

(** [build_font_names size bold italic] retourne un nom de font
    correspondant à la taille [size] l'epaisseur [bold] et
    l'inclinaison [italic]. Sous windows, la taille et l'italique ne
    marche pas pour l'instant. *)
let build_font_names_unix size bold italic =
  (** la font normal doit etre de largeur fixe *)
  let taille = string_of_int (size_to_pixels size * 10) in  (* 12 --> "120" *)
  let epai = if bold then "-bold" else "-medium" in
  let ital = if italic then "-i" else "-r" in
  ("-*-courier 10 pitch" ^ epai ^ ital ^ "-*-*-*-" ^ taille ^ "-*-*-*-*-*-*")

(* La taille des font ne marche pas sous windows. Ni l'italique. *)
let build_font_names_windows size bold italic =
  (** la font normal doit etre de largeur fixe *)
   "Verdana" ^ if bold then " gras" else "" ^ if italic then " italique" else ""

   
(** [set_font st] modifie la fonte d'affichage courante pour
    correspondre au style st. Sous windows, la taille et l'italique ne
    marche pas pour l'instant. Voir la documentation du module
    {!Graphics} pour la liste des couleurs possibles. *)
let set_current_font (st:text_style) =
  let bld_font =
    if Sys.os_type = "Unix" then build_font_names_unix else build_font_names_windows in
  set_font (bld_font st.size st.boldness st.italicness);
  set_text_size (size_to_pixels st.size);
  set_color st.color;;


(** Dessine une action. Sans saut de ligne, en partant de la position
    spécifiée. *)
let draw_action_no_sync action =
  match action with
  | Text action -> 
     set_current_font action.style;
     moveto (fst action.pos) (snd action.pos);
     draw_string action.texte
  | Line action ->
     set_color action.line_color;
     moveto (fst action.line_from) (snd action.line_from);
     lineto (fst action.line_to) (snd action.line_to)

let draw_action action =
  draw_action_no_sync action;
  Graphics.synchronize()

let draw_list_actions l =
  List.iter draw_action_no_sync l;
  Graphics.synchronize()


let text_width styl s =
  set_current_font styl;
  fst (Graphics.text_size s)

let text_height styl s =
  set_current_font styl;
  snd (Graphics.text_size s)



(** [draw_warning s] dessine la chaine [s] à la position par défaut  *)
let draw_warning s =
  begin	 
    set_color default_color;
    set_text_size 12;
    moveto (x_high_left_corner()) (y_high_left_corner());
    draw_string s;
  end



(** Exemple de fonction de détection d'événement. Consulter la
   documentation de la librairie Graphics pour plus de détails. *)
let wait_a_key_or_a_mouse () =	 
  (** Attend une touche ou un bouton de souris dans la fenêtre. *)
  let struc = wait_next_event [Button_down; Key_pressed] in
  (** Si on a tapé une touche, alors struc.key vaut le caractère tapé,
     sinon il vaut n'importe quoi. *)
  let x = struc.key in
  (** On effectue un test sur le caractère tapé et on retourne un
      entier different selon les cas. *)
  match x with
  | '.' -> 0
  | ',' -> 1
  | '+' -> 2
  | 'q' -> -1
  | c -> (** pour les caractères ne correspondant pas à un caractère ascii:*)
      match Char.code x with
      | 13 -> 3 (** Return *)
      | 8 -> 4 (** Backspace *)
      | 127 -> 5 (** Del *)
      | 27 -> 6 (** Escape *)
      | _ -> raise (Entree_inconnue (Char.escaped x))


(** Ouvre une fenêtre et règle les fonts *)
let start_graph() =
  (** Ceci ouvre une fenêtre graphique: *) 
  open_graph " 400x600"; (** laisser l'espace au début *)
  (** On teste si la font est bien reconnue. *)
  (try set_current_font default_style with _ -> print_string "probleme de font\n");
  set_window_title "Navigateur html"

;;




(** Ce module est celui où vous devez travailler; Le but du projet est
d'y définir les fonctions (mutuellement récursives) display_html et
display_html_list. *)

open Graph_utils
open Graphics
open Html_tree


exception Pas_encore_implante


(** Le type [display_state] représente l'entrée du moteur d'affichage:
    position courante dans la fenêtre, style du texte courant... *)
type display_state = 
  { start_pos: position; (** La position courante de l'afficheur. C'est là que
                             le texte doit continuer. *)
    style: text_style; (** Le style courant de l'afficheur. C'est comme ça que
                           doit être affiché du texte brut. *)
  }

(** Le type [return_state] représente les données de sortie de l'afficheur: La
    liste des actionns d'affichage à faire, la nouvelle position, les ancres
    détectées... *)
type return_state = 
  { list_actions: drawing_action list; (** La liste des drawing actions. Une par mot. *)
    last_position: position; (** Position situé à la fin de tous les affichages. *)
    (* ancres:(int*int*int*int*string) list;*) (*  Rectangles des positions dans les ancres. *)
  }
    

(** {1 LE MOTEUR D'AFFICHAGE}
    Le moteur d'affichage est une fonction récursive prenant en argument:
    - l'arbre html (de type html_tree) à afficher,
    - l'état du moteur d'affichage (de type display_state) et retourne le
      nouvel état de l'affichage (et aura pour effet d'afficher l'arbre dans
      la fenêtre graphique.)

   En fait c'est une fonction mutuellement récursive avec celle qui traite une
   liste d'arbres. En effet chaque noeud de l'arbre html contient une liste de
   sous-arbres. *)

(** Affichage d'un mot de texte brut.
    [display_word state w] retourne la liste des actions d'affichage
    nécessaires pour afficher (texte brut) le mot [w] dans l'état [state].
    C'est ici qu'on décide où on place le mot dans la fenêtre (doit-on passer
    à la ligne? *)
let rec display_word state wrd =
  let str = " " ^ wrd in
  let lineWidth = text_width state.style str in
  let (x, y) = state.start_pos in
  {list_actions=[Text {texte=wrd; style=state.style; pos=(x, y)}]; last_position=((x + lineWidth), (y))}

(** Affichage d'un arbre HTML.
    [display_html state tr] retourne la liste des actions d'affichage nécessaires
    pour afficher l'arbre [tr] à partir de l'état d'affichage [state]. *)
let rec display_html (state:display_state) (tr:html_tree) : return_state =
  let (x, y) = state.start_pos in
  match tr with
    | Empty           -> {list_actions=[]; last_position=state.start_pos}
    | Html   (a, ltr) -> display_html_list state ltr
    | Head   (a, ltr) -> display_html_list state ltr
    | Body   (a, ltr) -> display_html_list state ltr
    | P      (a, ltr) -> display_html_list {start_pos=(0, y - state.style.interligne); style=state.style} ltr
    | B      (a, ltr) -> let nStyle = {color=state.style.color; size=state.style.size; boldness=true; italicness=state.style.italicness; underlined=state.style.underlined; centered=state.style.centered; interligne=state.style.interligne} in
                         display_html_list {style=nStyle; start_pos=state.start_pos} ltr
    | I      (a, ltr) -> let nStyle = {color=state.style.color; size=state.style.size; boldness=state.style.boldness; italicness=true; underlined=state.style.underlined; centered=state.style.centered; interligne=state.style.interligne} in
                         display_html_list {style=nStyle; start_pos=state.start_pos} ltr
    | U      (a, ltr) -> let nStyle = {color=state.style.color; size=state.style.size; boldness=state.style.boldness; italicness=state.style.italicness; underlined=true; centered=state.style.centered; interligne=state.style.interligne} in
                         display_html_list {style=nStyle; start_pos=state.start_pos} ltr
    | Font   (a, ltr) -> display_html_list state ltr
    | A      (a, ltr) -> display_html_list state ltr
    | H1     (a, ltr) -> let nStyle = {color=state.style.color; size=1; boldness = state.style.boldness; italicness=state.style.italicness; underlined=state.style.underlined; centered=state.style.centered; interligne=state.style.interligne} in
                         let rState = display_html_list {start_pos=(0, y - (nStyle.interligne * 2)); style=nStyle} ltr in 
                         let (rx, ry) = rState.last_position in 
                         {list_actions=rState.list_actions; last_position=(0, ry - nStyle.interligne)}
    | H2     (a, ltr) -> let nStyle = {color=state.style.color; size=2; boldness = state.style.boldness; italicness=state.style.italicness; underlined=state.style.underlined; centered=state.style.centered; interligne=state.style.interligne} in
                         let rState = display_html_list {start_pos=(0, y - (nStyle.interligne * 2)); style=nStyle} ltr in 
                         let (rx, ry) = rState.last_position in 
                         {list_actions=rState.list_actions; last_position=(0, ry - nStyle.interligne)}
    | H3     (a, ltr) -> let nStyle = {color=state.style.color; size=3; boldness = state.style.boldness; italicness=state.style.italicness; underlined=state.style.underlined; centered=state.style.centered; interligne=state.style.interligne} in
                         let rState = display_html_list {start_pos=(0, y - (nStyle.interligne * 2)); style=nStyle} ltr in 
                         let (rx, ry) = rState.last_position in 
                         {list_actions=rState.list_actions; last_position=(0, ry - nStyle.interligne)}
    | H4     (a, ltr) -> let nStyle = {color=state.style.color; size=4; boldness = state.style.boldness; italicness=state.style.italicness; underlined=state.style.underlined; centered=state.style.centered; interligne=state.style.interligne} in
                         let rState = display_html_list {start_pos=(0, y - (nStyle.interligne * 2)); style=nStyle} ltr in 
                         let (rx, ry) = rState.last_position in 
                         {list_actions=rState.list_actions; last_position=(0, ry - nStyle.interligne)}
    | H5     (a, ltr) -> let nStyle = {color=state.style.color; size=5; boldness = state.style.boldness; italicness=state.style.italicness; underlined=state.style.underlined; centered=state.style.centered; interligne=state.style.interligne} in
                         let rState = display_html_list {start_pos=(0, y - (nStyle.interligne * 2)); style=nStyle} ltr in 
                         let (rx, ry) = rState.last_position in 
                         {list_actions=rState.list_actions; last_position=(0, ry - nStyle.interligne)}
    | Center (a, ltr) -> display_html_list state ltr
    | Title  (a, ltr) -> display_html_list state ltr
    | Word   (s)      -> display_word state s

(** Affichage d'une liste d'arbres HTML.
    [display_html_list state ltr] retourne la liste des actions d'affichage
    nécessaires pour afficher la liste d'arbres [ltr] à partir de l'état
    d'affichage [state]. On appelle [display_html] sur chaque arbres avec une
    position mise à jour à chaque fois. *)
and display_html_list (state:display_state) (ltr:html_tree list) =
  match ltr with
  | [] -> {list_actions=[]; last_position=state.start_pos}
  | tr::ltr' -> let rstate = display_html state tr in
                let nstate = display_html_list {start_pos=rstate.last_position; style=state.style} ltr' in
                let nla = rstate.list_actions @ nstate.list_actions in
                let (x, y) = nstate.last_position in
                if state.style.underlined then
                  {list_actions=nla @ [Line {line_from=state.start_pos; line_to=(x - text_width state.style " ", y); line_color=state.style.color}]; last_position=nstate.last_position}
                else
                  {list_actions=nla; last_position=nstate.last_position}
  (*let rstate = display_html state tr in rstate.list_actions @ display_html_list state ltr'*)


let display htree =
  (* L'état d'affichage initial: en haut à gauche de la fenêtre graphique, texte normal. *)
  let init_state =
    { start_pos = (x_high_left_corner()-10 , y_high_left_corner()-10);
      style = default_style } in
  display_html init_state htree

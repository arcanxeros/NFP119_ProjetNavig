(** Module des actions graphiques pour le projet + fonctions
    graphiques utiles. N'hésitez pas à ajoutez des fonctions dedans
    (et à déclarer dans le mli celles que vous voulez utiliser depuis
    votre fichier principal). *)

open Graphics

(** {1 Les actions d'affichage du moteur HTML} *)

(** Une position est un couple d'entier (x,y). *)
type position = int * int

(** Description du style d'un texte. Ce type sert dans à stocker
    toutes les informations de style pendant l'affichage. ATTENTION
    les iformations de centrage et de soulignement ne sont pas
    interprétées par draw_action. Cela signifie que pour souligner il
    faut faire une action supplémentaire [Ligne], et pour le centrage
    il faut avoir calculer la position de centrage de la ligne. *)
type text_style =
  { color: Graphics.color;
    size:int; (** Taille du texte: 1 = (H1, grand titre), 5=(H5), -1=texte normal.  *)
    boldness : bool; (** gras ou non *)
    italicness : bool; (** italique ou non *)
    underlined : bool; (**souligné ou non ATTENTION NON PRIS EN COMPTE par [draw_action]*)
    centered : bool; (** centré ou non ATTENTION NON PRIS EN COMPTE par [draw_action] *)
    interligne :int; (** nombre de pixels à sauter entre chaque ligne. *)
  }

(** Information d'affichage d'un bout de texte (typiquement un mot).
   Contient la position de départ (en bas à gauche du texte), la
   chaîne à afficher, et le style à utiliser. *)
type text_action =
  { texte: string; style: text_style; pos: position}

(** Information d'affichage d'une ligne: Contient les positions de
   départ et d'arrivée et la couleur du trait. *)
type line_action =
  { line_from: position; line_to: position; line_color:color}

(** Une action d'affichage décrit l'affichage d'un morceau de texte ou
   d'un trait Chaque action sera dessinée sans saut de ligne. Il faut
   donc découper un texte en plusieurs drawing_action pour l'afficher
   sur plusieurs lignes. Les lignes serviront pour souligner les
   textes ayant la balise <underline> ainsi que les liens
   hypertexte. *)
type drawing_action =
  | Text of text_action
  | Line of line_action

val pr_drawing_action: drawing_action -> string
val default_style: text_style
val draw_action: drawing_action -> unit
val draw_list_actions: drawing_action list -> unit
val text_width: text_style -> string -> int
val text_height: text_style -> string -> int

(** Exception consacrée aux événements non prévus par l'interface. *)
exception Entree_inconnue of string

(** {1 Fonction graphiques possiblement utiles} *)

(** Retourne l'ordonnée du coin supérieur gauche (avec une petite marge) *)
val y_high_left_corner : unit -> int

(** Retourne  l'abscisse du coin supérieur gauche (avec une petite marge) *)
val x_high_left_corner : unit -> int

(** Retourne l'ordonnée du coin inférieur droit (avec une petite marge) *)
val y_bottom_right_corner : unit -> int

(** Retourne l'abscisse du coin inférieur droit (avec une petite marge) *)
val x_bottom_right_corner : unit -> int

(** Exemple de fonction de détection d'événement. Consulter la
   documentation de la librairie Graphics pour plus de détails. Lève
   l'exception [Entree_inconnue] si l'événement n'est pas prévue. *)
val wait_a_key_or_a_mouse : unit -> int

(** Ouvre une fenêtre et règle les fonts. Appelez cette fonction au
    début de votre fonction principale. *)
val start_graph : unit -> unit


(** {2 Quelques couleurs pré-définies.} *)

(** Retourne la couleur par défaut (noir) *)
val default_color : Graphics.color

val black : color
val white : color
val red : color
val green : color
val blue : color
val yellow : color
val cyan : color
val magenta : color

(** [rgb r g b] retourne une nouvelle couleur, avec les paramètres [r]
    (rouge), [g] (vert) et [b] (bleu). [r], [g] et [b] sont des [int]
     compris entre 0 et 250. *)
val rgb : int -> int -> int -> color

(*
 * Navigateur html
 * Copyright (C) 2004 Pierre Courtieu
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU General Public License version 2 for more details
 * (enclosed in the file GPL).
 *)

(* IL N'Y A RIEN À MODIFIER ICI DANS CE FICHIER, sauf pour la gestion des
   événements clavier/souris (ce qui fera l'objet d'une présentation en cours
   le moment venu). *)

(** Ce module est le module principal du programme. Pour info c'est là que:
    - les paramètres passés au programme sont reconnus (déjà fait),
    - le fichier html est ouvert (déjà fait),
    - ce fichier est parsé et transformé en arbre html,
    - votre fonction d'affichage (dans le fichier draw_html.ml) est
      appelée. *)

open Graphics
open Graph_utils
open Html_tree
open Draw_html (* Le fichier ou vous devez travailler *)


(* ********************************************************** *)
(* ************** FONCTION DE L'OPTION -parse *************** *)


(* Affichge dans le terminal de l'arbre HTML reconnu. *)
let parse_seulement htree =
  (* On affiche dans la console le contenu de l'arbre html, en texte. *)
  Format.printf "%a@\n@?" Html_tree.print htree

(* ********************************************************** *)
(* ************* FONCTION DE L'OPTION -exemple ************** *)
  
(* Exemple d'affichage de quelques drawing_action. *)
let draw_exemple () =
  (* Lancement graphique: *)
  Graph_utils.start_graph();
  (* calcul de la largeur de "Essai de style" avec la taille de caractère 2,
     afin de souligner correctement *)
  let width = Graph_utils.text_width {default_style with size = 2; color=red} "Essai de Style" in
  let quelques_drawing_actions =
    [(Text { texte = "Essai de Style"; style = Graph_utils.default_style; pos = (10,10)});
     (Text { texte = "Essai de Style"; style = {default_style with boldness = true} ; pos = (10,25)});
     (Text { texte = "Essai de Style"; style = {default_style with boldness = true; italicness = true} ; pos = (10,40)});
     (Text { texte = "Essai de Style"; style = {default_style with size = 2; color=red} ; pos = (10,55)});
     (Line { line_from = 10,54 ; line_to = 10+width ,54; line_color=Graphics.black});
     (Line { line_from = 10,57 ; line_to = 110 ,57; line_color=Graphics.black})] in

  (* On imprime en texte quelques drawing_action arbitraire, Vous pouvez utiliser
     cela pour tester le résultat de la fonctino display ci-dessus. *)
  Format.printf "drawing_actions affichées:@\n";
  List.iter (fun x -> Format.printf "%s@\n@?" (pr_drawing_action x)) quelques_drawing_actions;
  Format.printf "@\n";


  (* On effectue l'affichage graphique des drawing_action, commentez cette
     partie si cela ne marche pas chez vous, nous testerons en salle TP. *)
  List.iter draw_action quelques_drawing_actions;
  draw_action (Line { line_from = 10,54 ; line_to = 10+width ,54; line_color=Graphics.black});
  (* Ceci attend un événement (voir [Graph_utils.wait_a_key_or_a_mouse]
     ci-dessus) avant de terminer le programme *)
  let _ = try Graph_utils.wait_a_key_or_a_mouse()
          with _ -> -1 (* Valeur de retour *)
  in
  exit 0 (* Sortie définitive et sans erreur du programme navig. *)


(* ********************************************************** *)
(* ******** GESTION DES PARAMÈTRES PASSÉ AU PROGRAMME ******* *)

(** nom du fichier passé en argument. Pour l'instant elle est
    positionnée à la chaine vide. *)
let filename_commandline = ref ""
(** Vérification de l'existence du fichier nommé s et mise à jour de la
   variable globale [filename_commandline]. *)
let get_filename_commandline (s:string) = 
  if !filename_commandline <> ""
  then raise (Invalid_argument "filename" )
  else if Sys.file_exists s then filename_commandline := s
  else raise (Sys_error "File does not exist");;

(* Est-ce-que l'option -exemple a été détectée? *)
let exemple = ref false
let set_exemple() = exemple:=true
let unset_exemple() = exemple:=false
let get_exemple() = !exemple

(* Est-ce-que l'option -parse a été détectée? *)
let parse = ref false
let set_parse() = parse:=true
let unset_parse() = parse:=false
let get_parse() = !parse

(* Les paramètres acceptés par le programme, en plus de -help, --help et du
   nom du fichier html *)
let args = 
  [ ("-parse", Arg.Unit set_parse, "Affiche en texte l'arbre HTML reconnu (fichier HTML non lu).");
    ("-exemple", Arg.Unit set_exemple, "Affiche quelques drawing_actions en texte et graphiquement")
  ]
(** Le message en cas de lancement erroné du programme ou avec l'option -help *)
let usage = "navig [-parse] [-exemple] [fichier.html]

 Lance le navigateur sur le fichier fichier.html.
"
let show_help() = Arg.usage [] usage
(* ********************************************************** *)
(* ********************************************************** *)
    
(* FONCTION PRINCIPALE *)
let main () =
  (** Cette ligne a pour effet de lire les arguments passés à votre programme,
      et affecter le nom de fichier à la variable globale
      [get_filename_commandline]. *)
  let _ = 
    try Arg.parse args get_filename_commandline usage
    with _ -> (print_string "Ce fichier n'existe pas.\n"; show_help();exit 1) in  
  (* Réaction aux différentes option passée au programme *)
  (* option -exemple: affichage texte + test des affichages graphiques simples *)
  if get_exemple() then draw_exemple ()
  else
    (** htree contient l'arbre HTML du fichier. *)
    let htree = Html_tree.lit_fichier (!filename_commandline) in
    if get_parse() then parse_seulement htree (* Option -parse: affiche l'arbre HTML en texte *)
    else
      (* OK on démarre le navigateur pour de bon. *)
      (* Ouverture de la fenêtre graphique *)
      let _ = Graph_utils.start_graph() in
      while true do (* boucle de réaction aux événement, on sort par "exit 0" plus bas *)
        (* Effacement des éventuels affichages précédents *)
        let _ = clear_graph() in
        (* Appel à votre fonction d'affichage, endstate doit contenir entre
           autre la liste de tous les drawing_actions à faire. *)
        let endstate = Draw_html.display htree in
        (* Exécution des drawing_actions *)
        draw_list_actions endstate.list_actions;
        (* Chaque fois que l'utilisateur appuie sur une touche (ou clique), la
           boucle recommence. Sauf si 'q' est tapé. PROGRAMMEZ ICI D'AUTRES
           RÉACTIONS AUX TOUCHE/CLICK. Voir graph_utils.ml la fonction
           wait_a_key_or_a_mouse pour les événements reconnus. *)
        try
          let c = wait_a_key_or_a_mouse() in
          if c = -1 then exit 0 (* wait_a_key_or_a_mouse retourne -1 si 'q' est tapé *)
          else print_string "tapez q pour quitter\n"
        with Entree_inconnue _ -> print_string "tapez q pour quitter\n";
      done;
      ()
;;
(**  *)

(* LANCEMENT DE LA FONCTION PRINCIPALE
   et affichage des exceptions non rattrapées. *)
Printexc.print main ()
;;

(* 
 *** Local Variables:  ***
 *** fill-column: 78 ***
 *** compile-command: "make" ***
 *** End:  ***
 *)

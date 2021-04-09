open Xmlm
open Format

exception Pas_encore_implante

type attribut = 
    | Href of string
    | Color of string
    | Size of int

type html_tree = 
  | Empty 
  | Html of attribut list * html_tree list
  | Head of attribut list * html_tree list
  | Body of attribut list * html_tree list
  | P of attribut list * html_tree list
  | B of attribut list * html_tree list
  | I of attribut list * html_tree list
  | U of attribut list * html_tree list
  | Font of attribut list * html_tree list
  | A of attribut list * html_tree list
  | H1 of attribut list * html_tree list
  | H2 of attribut list * html_tree list
  | H3 of attribut list * html_tree list
  | H4 of attribut list * html_tree list
  | H5 of attribut list * html_tree list
  | Center of attribut list * html_tree list
  | Title of attribut list * html_tree list
  | Word of string

(* Les blancs: blanc, tab ou retour à la ligne. *)
let spaces = [ ' ' ;'\n'; '\t' ]

(* Les lettre accentuées en utf8 et leur traduction en iso-latin. Les
   librairie String et Graphics ne supporte pas correctement
   l'utf-8. *)
let utf_8_codes = [
    '\xc3', ['\xa9','\xe9' ; (* é *)
	     '\xa8','\xe8'; (* è *)
	     '\xaa','\xea'; (* ê *)
	     '\xab','\xeb'; (* ë *)
	     '\xa0','\xe0'; (* à *)
	     '\xa2','\xe2'; (* â *)
	     '\xae','\xee'; (* î *)
	     '\xaf','\xef'; (* ï *)
	     '\xb4','\xf4'; (* ô *)
	     '\xb9','\xf9'; (* ù *)
	     '\xbc','\xfc'; (* ü *)
	    ]
  ]


(* Detecte les pattern de lettres accentuées utf8 et les transforme en
   caractère iso-latin, Graphics ne gère pas l'utf8. *)
let rec code table s =
  let res = ref "" in
  let i = ref 0 in
  while !i < String.length s do
    let c = s.[!i] in
    try 
      let lrepl = List.assoc c table in
      let d = s.[!i+1] in
      let repl = List.assoc d lrepl in
      res := !res^String.make 1 repl;
      i := !i + 2
    with Not_found -> (res := !res^String.make 1 s.[!i]; i := !i + 1)
  done;
  !res

let recode  = code utf_8_codes

let space fmt () = fprintf fmt "@ "
let comma fmt () = fprintf fmt ",@ "
let semi fmt () = fprintf fmt ";@ "

let rec print_list sep print fmt = function
  | [] -> ()
  | [x] -> print fmt x
  | x :: r -> print fmt x; sep fmt (); print_list sep print fmt r

let p_attribute fmt attr =
  match attr with
  | Href s -> fprintf fmt "href=\"%s\"" s
  | Color s -> fprintf fmt "color=\"%s\"" s
  | Size n -> fprintf fmt "size=\"%d\"" n
  
let rec print fmt t =
  let pl = print_list semi print in
  let pla fmt l =
    if l=[] then Format.fprintf fmt ""
    else Format.fprintf fmt "{@[<h>%a@]}," (print_list space p_attribute) l in
  match t with
    | Empty -> fprintf fmt "<empty?>"
    | Html (attr,trees) -> fprintf fmt "html(@[%a@ [ @[%a@] ]@])" pla attr pl trees
    | Head (attr,trees) -> fprintf fmt "head(@[%a@ [ @[%a@] ]@])" pla attr pl trees
    | Body (attr,trees) -> fprintf fmt "body(@[%a@ @[[ @[%a@] ]@])" pla attr pl trees
    | P (attr,trees) -> fprintf fmt "P([ @[%a@] ])" pl trees
    | B (attr,trees) -> fprintf fmt "B([ @[%a@] ])" pl trees
    | I (attr,trees) ->   fprintf fmt "I([ @[%a@] ])" pl trees
    | U (attr,trees) ->   fprintf fmt "U([ @[%a@] ])" pl trees
    | Center (attr,trees) ->   fprintf fmt "Center(%a[ @[%a@] ])" pla attr pl trees
    | A (attr,trees) -> fprintf fmt "A(%a[ @[%a@] ])" pla attr pl trees
    | Font (attr,trees) -> fprintf fmt "Font(%a[ @[%a@] ])" pla attr pl trees
    | H1 (attr,trees) -> fprintf fmt "H1([ @[%a@] ])" pl trees
    | H2 (attr,trees) -> fprintf fmt "H2([ @[%a@] ])" pl trees
    | H3 (attr,trees) -> fprintf fmt "H3(@; [ @[%a@] ])" pl trees
    | H4 (attr,trees) -> fprintf fmt "H4(@; [ @[%a@] ])" pl trees
    | H5 (attr,trees) -> fprintf fmt "H5(@; [ @[%a@] ])" pl trees
    | Title (attr,trees) -> fprintf fmt "title(%a@; [ @[%a@] ])" pla attr pl trees
    | Word s -> fprintf fmt "Word [ @[%s@] ]" s

let attribute_to_string attr =
  match attr with
  | Href s -> "href=\""^s^"\""
  | Color s -> "color=\""^s^"\""
  | Size n -> "size=\""^string_of_int n^"\""

let rec to_string t = 
  fprintf str_formatter "%a" print t;
  flush_str_formatter()


(* Découpage d'une chaine en mots (séparateur: blanc, tab ou retour à
   la ligne.) *)
let rec split i j txt =
  let res =
    if j = String.length txt
    then
      if i<j then [(String.sub txt i (j-i))] else []
    else
      if List.mem (txt.[j]) spaces
      then
        if j>i 
        then
          let wrd = String.sub txt i (j-i) in 
          wrd :: (split (j+1) (j+1) txt)
        else split (j+1) (j+1) txt
      else split i (j+1) txt
  in
  res

let parse_attribut_to_attribut (att,value) =
  match att with
    | "href" -> Href(value)
    | "color" -> Color(value)
    | "size" -> Size(int_of_string value)
    | _ -> failwith ("attribut " ^ att ^ " non traitee")

let build_attribut (l:Xmlm.attribut list): attribut list =
  List.map parse_attribut_to_attribut l


(* Transforme l'arbre xml vers un arbre html. Les balises qui
   n'apparaissent pas ci-dessous sont rejetée (exception levée). Si
   vous voulez étendre l'ensemble des balises reconnu il faut ajouter
   des balises dans le type html_tree et ajouter des cas dans le match
   ci-dessous. *)
let rec from_indif_l t =
  match t with
    | D text -> List.map (fun x -> Word x) (split 0 0 (recode text)) (* recodage utf8 --> iso *)
    | E((s,l),lfils) ->
      let fls () = 
        List.filter (function Word "" -> false | _ -> true)
                    (List.flatten (List.map from_indif_l lfils)) in
      let at () = build_attribut l in
      match String.lowercase s with
	| "a" -> [A(build_attribut l,fls ())]
	| "empty" -> [Empty] 
	| "html" -> [Html(at (),fls ())]
	| "head" -> [Head (at(),fls())]
	| "body" -> [Body (at(),fls())]
	| "p" -> [P (at(),fls())]
	| "b" -> [B (at(),fls())]
	| "i" -> [I (at(),fls())]
	| "u" -> [U (at(),fls())]
	| "center" -> [Center (at(),fls())]
	| "font" -> [Font (at(),fls())]
	| "h1" -> [H1 (at(),fls())]
	| "h2" -> [H2 (at(),fls())]
	| "h3" -> [H3 (at(),fls())]
	| "h4" -> [H4 (at(),fls())]
	| "h5" -> [H5 (at(),fls())]
 	| "title" -> [Title(at(),fls())]
	| _ -> failwith ("balise " ^String.lowercase s^ " inconnue")


let rec from_indif t = List.hd (from_indif_l t)


(* Lit un fichier xml et retourne l'arbre html correspondant. *)
let lit_fichier f =
 from_indif (Xmlm.lit_fichier f)

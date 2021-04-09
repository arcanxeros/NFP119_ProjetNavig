(** Ce module implante le type représentant les fichiers html, ainsi
    que la fonction de lecture dans un fichier. *)


(** Le type des attributs accompagnant un nouveau sous-arbre (ces
    attributs correspondent à la balise qui a servi à ouvrir ce
    sous-arbre dans le fichier html). Vous pouvez compléter ce type
    avec les attributs dont vous avez besoin. *)
type attribut = 
    Href of string 
  | Color of string
  | Size of int

(** Le type [html_tree] représentant une page web. Il s'agit d'un
    arbre à branchement quelconque (chaque noeud à un nombre
    arbitraire de fils). Il y a une liste d'attributs (éventuellement
    vide) sur chaque noeud (sauf les noeuds de type [Word]), et une
    liste de sous-arbres.
    Par exemple:
{v 
   <A href="fichier.html"> 
     <p>Bonjour Toto.</p>
     <p>Aurevoir Toto.</p>
   </A> 
v}
    Sera représenté par:
{[
     A([Href("fichier.html")] , 
       [ P([] , [ Words "Bonjour" ; Word "Toto." ]) ;
         P([] , [ Word "Aurevoir" ; Word "Toto." ])
       ]) 
    ]}
  *)
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


(** [lit_fichier s] lit le fichier nommé [s] et retourne l'arbre html
    correspondant, lève une exception si le format n'est pas reconnu
    ou si une balise inconnue est utilisée. *)
val lit_fichier : string -> html_tree

(** [to_string ht] retourne une chaîne de caractères représentant
    l'arbre html [ht]. Pour débuger. *)
val to_string : html_tree -> string

(** Pour l'utilisation avec Format.fprintf. *)
val print: Format.formatter -> html_tree -> unit

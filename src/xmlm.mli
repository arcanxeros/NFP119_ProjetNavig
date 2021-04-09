
(** Module de parsing non typé, vous n'avez pas à le modifier. L'arbre n'a que:

    - un seul type de noeud ([E]), étiqueté par le nom de la balise et
      la valeur des attributs.

    - Un seul type de feuille ([D]): le texte brut entre les balises. *)

(** Un attribut est un nom d'attribut + une valeur (exemple: [("size","3")]) *)
type attribut = string * string

(** Une balise est un nom de balise + une liste d'attributs (exemple: [("a",[("href","toto.html")])]) *)
type balise = string * attribut list


(** Le type [arbreindif] représente une page web. Il s'agit d'un
    arbre à branchement quelconque (chaque noeud à un nombre
    arbitraire de fils). Il n'y a qu'un seul type de noeud, étiqueté
    par le nom de la balise et la liste de ses attributs. Par exemple:
   {v
   <A href="fichier.html"> 
     <p>Bonjour Toto.</p>
     <p>Aurevoir Toto.</p>
   </A>
   v}

    Sera représenté par: {[
    
     E(("a",[ ("href","fichier.html") ]) , 
       [ E(("p",[]), [D (["Bonjour" ; "Toto."])]);
         E(("p",[]), [D (["Aurevoir" ; "Toto."])])
       ]
     ) 
    ]}

  *)
type arbreindif =
  |  E of balise * arbreindif list
  (** Balise (et ses attributs) + les sous-arbres. *)

  | D of string
  (** Chaque mot est dans un D séparé. *)

(** [lit_fichier file_name] retourne l'arbre de type [arbrenontype] correspondant
    au contenu du fichier [filename]. *)
val lit_fichier : string -> arbreindif



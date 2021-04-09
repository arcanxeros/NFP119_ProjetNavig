#!/bin/bash
# Tapez "./compile.sh" pour tout compiler.

# Remplacez ocamlc par un chemin plus précis (\...\ocamlc) si ocamlc
# n'est pas dans votre PATH.
# montre les commandes exécutées

OPTIONS='-w d'
#OPTIONS="-w 3"
#OPTIONS="-w -deprecated"
#set -o verbose

clear
ocamlc -c $OPTIONS -o obj/graph_utils.cmi src/graph_utils.mli
ocamlc -c $OPTIONS -o obj/xmlm.cmi src/xmlm.mli
ocamlc -c $OPTIONS -o obj/html_tree.cmi src/html_tree.mli
ocamlc -c $OPTIONS -o obj/draw_html.cmo src/draw_html.ml -I obj
ocamlc -c $OPTIONS -o obj/navig.cmo src/navig.ml -I obj
ocamlc -c $OPTIONS -o obj/graph_utils.cmo src/graph_utils.ml -I obj
ocamlc -c $OPTIONS -o obj/html_tree.cmo src/html_tree.ml -I obj
ocamlc -c $OPTIONS -o obj/xmlm.cmo src/xmlm.ml -I obj
ocamlc graphics.cma obj/graph_utils.cmo obj/xmlm.cmo obj/html_tree.cmo obj/draw_html.cmo obj/navig.cmo $OPTIONS -o bin/navig.byte

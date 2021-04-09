#!/bin/bash
# echo "Rappel: Vous devez faire compiler (en bytecode) avant de lancer ce script."
# echo "Si au démarrage du toplevel vous voyez des message \"Cannot find file\""
# echo "cela signifie que vous avez oublié de le faire."

PROJET=navig
MISSING=`ocaml -I bin  utils/init.ml | grep "Cannot find file"`

if [ "$MISSING" = "" ]; then
    ocaml -I bin -init utils/init.ml
else
    echo "Faites make $PROJET.byte d'abord"
fi

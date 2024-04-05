#!/bin/bash

usage() {
    echo "USAGE"
    echo "Tu fais \"history\" dans ton terminal pour voir le numéro des commandes que tu veux supprimer."
    echo "Ensuite tu fais:"
    echo "./rm --toutes les commandes que tu veux supprimer"
    echo "EXAMPLE"
    echo "./rm 45 456 456 123 123"
}

# Afficher l'historique des commandes
history

# Supprimer les commandes spécifiées
rm_commands() {
    local dest=" history -d"
    for arg in "$@"; do
        if [[ $arg =~ ^[0-9]+$ ]]; then
            dest+=" $arg"
        else
            echo "Invalid command number: $arg"
        fi
    done
    eval $dest
}

if [ "$#" -eq 0 ] || [ "$1" = "-h" ]; then
    usage
    exit 1
fi

rm_commands "$@"

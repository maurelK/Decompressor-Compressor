#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void swap(char **a, char **b)
{
    char *c = *a;
    *a = *b;
    *b = c;
}

char **sort(char **tab)
{
    int i = 1;
    int j = 0;

    while (tab[i]) {
        for (j = i + 1; tab[j]; j++) {
            if (strcmp(tab[i], tab[j]) < 0) {
                swap(&tab[i], &tab[j]);
            }
        }
        i++;
    }
    return tab;
}

void rm(char **av)
{
    int i = 1;
    char dest[1000] = "history -d ";
    while (av[i]) {
        strcat(dest, av[i]);
        strcat(dest, " ");
        i++;
    }
    system(dest);
}

void usage()
{
    printf("USAGE\n");
    printf("Tu fais \"history\" dans ton terminal pour voir le numéro des commandes que tu veux supprimer.\n");
    printf("Ensuite tu fais:\n");
    printf("./rm --toutes les commandes que tu veux supprimer\n");
    printf("EXAMPLE\n");
    printf("./rm 45 456 456 123 123\n");
}

int main(int ac, char **av)
{
    system("history");
    /*if (ac == 1 || (ac == 2 && strcmp(av[1], "-h") == 0)) {
        usage();
        return 84;
    }
    rm(av);*/
    return 0;
}

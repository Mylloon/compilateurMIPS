# Compilateur vers du MIPS en OCaml

## Usage

### Compilation

```
$ make
```

### Utilisation

```
$ ./main.byte {fichier source} > sortie.s
```

> Une fois le fichier assembleur créé, il est possible d'utiliser
> [spim](https://sourceforge.net/projects/spimsimulator/) : `spim -file sortie.s`

### Lance les tests

```
$ make test
```

## Fonctionnalités

-   [x] Type `int`
-   [x] Type `bool`
-   [ ] Type `string`
-   [x] Déclaration / assignation de variables
-   [x] Librairie standard (multiplication, addition, comparateur, ...)
-   [ ] Conditions
-   [ ] Boucles
-   [ ] Fonctions utilisateurs (arguments et valeur de retour typé)
-   [ ] Allocation mémoire (`malloc`)
-   [ ] Pointeurs
-   [ ] Listes + fonctions écrites dans notre language pour gérer ses listes

### Améliorations

-   Fonctions de la baselib sont _inline_

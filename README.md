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
-   [x] Type `string`
-   [x] Déclaration / assignation de variables
-   [x] Librairie standard (multiplication, addition, comparateur, print, ...)
-   [x] Conditions
-   [x] Boucles
-   [x] Fonctions utilisateurs récursives (arguments et valeur de retour typé)
-   [ ] Allocation mémoire (`malloc`)
-   [ ] Pointeurs

### Améliorations

-   Fonctions de la baselib sont _inlinées_

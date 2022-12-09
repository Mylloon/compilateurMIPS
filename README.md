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

## Fonctionalités

-   [x] Type `int`
-   [x] Type `bool`
-   [ ] Type `string`
-   [x] Déclaration / Assignation de variables
-   [ ] Quelques fonction de base (multiplication, addition, comparateur, ...)
-   [ ] Conditions
-   [ ] Boucles
-   [ ] Fonctions
-   [ ] Pointeurs

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
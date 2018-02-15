# heh

Give me a development MySQL now!

### Example

```
$ heh start
$ heh repl
mysql> ^DBye
$ heh stop
```

### Usage

```
$ heh --help
dev-mysql

Usage: heh (START | STOP | RESART | REPL)

Available options:
  -h,--help                Show this help text
  START                    start mysql container
  STOP                     stop mysql container
  RESART                   restart mysql container
  REPL                     start repl to mysql container
```

### Install

```
$ git clone https://github.com/jpittis/heh
$ cd heh
$ stack install
```

### Dependencies

- stack ([install](https://docs.haskellstack.org/en/stable/install_and_upgrade))
- docker ([install osx](https://docs.docker.com/docker-for-mac/install/), [install not osx](https://docs.docker.com/install))

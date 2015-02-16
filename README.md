# Haverer

Haverer is a simple implementation of the excellent card game,
[Love Letter](http://boardgamegeek.com/boardgame/129622/love-letter).

It follows the original Japanese rules, rather than the newer American ones.

## Running

At the moment, you'll need to compile from source, which means you'll need to
get set up with `cabal`. The easiest way to do this is to install the
[Haskell Platform](https://www.haskell.org/platform/).

Once you've done that, run the following commands:

```
$ git clone https://github.com/jml/haverer.git
$ cd haverer
$ cabal sandbox init
$ cabal configure
$ cabal install --dependencies-only
$ cabal run
```

That last command, `cabal run`, will run a command-line version of the game.

## License

This code is copyright Jonathan M. Lange, 2014-2015, and is made available
under the Apache license.

## Roadmap

1. Wrap this whole thing in an API server
2. Make that server available to the public
3. Put a nice-ish web frontend in front

## Bugs

Please file issues on the
[Github issue tracker](https://github.com/jml/haverer/issues)

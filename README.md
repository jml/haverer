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
under the Apache License, version 2.0. See LICENSE for details.

## Roadmap

0. Procrastinate endlessly learning new Haskell tricks
1. Wrap this whole thing in an API server
2. Make that server available to the public
3. Put a nice-ish web frontend in front

## Bugs

Please file issues on the
[Github issue tracker](https://github.com/jml/haverer/issues)

## Patches

Are most welcome.

## History

The original implementation of this game is called
[loveletter](https://github.com/jml/loveletter) and is written in
[Rust](http://www.rust-lang.org/).

## Thanks

Thanks to the following people, who have helped with advice, kindness, or by patiently
smiling and nodding while I've rambled on about this project:

* metagnome
* RAOF
* idnar
* pjdelport
* tomprince
* radix
* teh
* simpson
* joliette

Any infelicities are my own.

If you are one of these people and would like to be referred to in another way
(perhaps you'd like me to link to your Github page?), please contact me. You
know how.


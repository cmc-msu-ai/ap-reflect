# ap-reflect

[![Build Status](https://travis-ci.org/cmc-msu-ai/ap-reflect.svg)](https://travis-ci.org/cmc-msu-ai/ap-reflect)

Partial evaluation reflection a la simple-reflect.

## Installation

    $ git clone https://github.com/cmc-msu-ai/ap-reflect
    $ cd ap-reflect
    $ cabal install

For some possibilities of using this library 
you may need to install `simple-reflect`.  
It is available from
[Hackage](http://hackage.haskell.org/package/simple-reflect).
Install it, by typing:

    cabal install simple-reflect

## Documentation

Haddock documentation is available at  
http://cmc-msu-ai.github.io/ap-reflect/doc/html/ap-reflect/

## Usage

For example:

```haskell
let (.+) = makeBinOp "+" (+)
mapM_ print . reductions $ (.+) -$- Just a -*- Just b
```

Result:

    (+) <$> Just a <*> Just b
    Just (a +) <*> Just b
    Just (a + b)

You can find another examples of using this library at  
https://github.com/cmc-msu-ai/ap-reflect/tree/master/examples

## Contributors

* Nickolay Kudasov

## Contribution

Contact me (Oleg Baev): odbaev@yandex.ru

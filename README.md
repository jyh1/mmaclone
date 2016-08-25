# A simple term rewriting system with Mathematica's syntax

Inspired by the book [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).
I decide to write myself a simple Mathematica interpreter to learn more about Haskell as well as
get a deeper understanding of Mahtematica.

## ScreenShot

![mmaclone](https://raw.githubusercontent.com/jyh1/mmaclone/master/demo.GIF)


## Running (Using [Stack](https://github.com/commercialhaskell/stack)):
```
  git clone https://github.com/jyh1/mmaclone.git
  cd mmaclone/mmaclone
  stack setup
  stack build
  stack exec mmaclone-exe
```

Prebulid windows binary file is available on the [release page](https://github.com/jyh1/mmaclone/releases)

For more information please refer to the project [wiki](https://github.com/jyh1/mmaclone/wiki) (still under construction).



## Features that are likely to be added in future versions:
(Some serious design errors are exposed during development, which I consider are inhibating
  the project from scaling up. So currently my primary focus would be on refactor
  rather than adding new features/functions)

1. More mathematical functions (`Sin`, `Cos`, `Mod` etc...)
2. More built-in functions (`Level`, `Import`, `Derivative`etc...)
3. More sophisticated pattern matching
  * head specification (of the form Blank[*Head*], currently it only support list type)
  * ~~Pattern Test~~(Impelemented)
  * BlankSequence, BlankNullSequence
4. ~~RecursionLimit~~(Implemented)
5. Negative index e.g. in `Part`
6. Neagtive level specification
7. Curried function e.g. `f[a][b]` (currently it will throw an error if one is trying to attach value to
  the curried form through `Set` or `SetDelayed`)
8. Use iPython as frontend
9. ~~Replace String with Text~~(Implemented)

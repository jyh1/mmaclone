# A simple term rewriting system with Mathematica's syntax

After reading about the book [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).
I decide to write myself a simple Mathematica interpreter to learn more about Haskell.

## ScreenShot

![mmaclone](https://raw.githubusercontent.com/jyh1/mmaclone/master/demo.GIF)


## Running:
```
  git clone https://github.com/jyh1/mmaclone.git
  cd mmaclone/mmaclone
  stack setup
  stack build
  stack exec mmaclone-exe
```


## Features that are likely to be added in future versions:
1. More mathematical functions (`Sin`, `Cos`, `Mod` etc...)
2. More built-in functions (`Level`, `Import`, `Derivative`etc...)
3. More sophisticated pattern matching
  * head specification (of the form Blank[*Head*], currently it will simply be ignored)
  * BlankSequence, BlankNullSequence
4. RecursionLimit
5. Negative index e.g. in `Part`
6. Neagtive level specification
7. Curried function e.g. `f[a][b]` (currently it will throw an error if one is trying to attach value to
  the curried form through `Set` or `SetDelayed`)

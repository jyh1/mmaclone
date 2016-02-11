module Eval.Primitive.Primi.Primi(primi) where

import Data.DataType
import Eval.Primitive.Primi.List.List
import Eval.Primitive.Primi.Compare.Compare
import Eval.Primitive.Primi.Logic.Logic
import Eval.Primitive.Primi.Arithmatic.Arithmatic

import Eval.Primitive.PrimiType
import Control.Monad
import Control.Monad.Except


primi :: [(String, Primi)]
primi = [
              ("Plus",plusl),
              ("Times", timesl),
              ("Power", powerl),
              -- list mainpulation
              ("car", carl),
              ("cdr", cdrl),
              ("cons", consl),
              ("Length", lengthl),
              ("Part", partl),
              -- list construction
              ("Range", rangel),

              -- comparation
              ("Less", lessl),
              ("LessEqual" , lessEquall),
              ("Great", greaterl),
              ("GreatEqual", greaterEquall),
              ("Equal", equall),
              -- ("symbol?", testHead symbolQ),
              -- ("string?", testHead stringQ),
              -- ("number?", testHead numberQ),
              -- ("quote", quoted)
              -- ("quoteient", numericBinop quot),
              ("And", andl),
              ("Or", orl),
              ("Not", notl)
            ]

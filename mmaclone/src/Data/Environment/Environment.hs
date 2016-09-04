module Data.Environment.Environment where

import Data.DataType
import Eval.Patt.Pattern
import Eval.Patt.PatternPrimi
import Data.Environment.EnvironmentType


import Control.Monad.Except
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Except
import Control.Lens hiding (Context,List)
import Data.Maybe
import qualified Data.Text as T


emptyOwnValue :: OwnValue
emptyOwnValue = M.fromList [("$IterationLimit", Number 50000)]
emptyDownValue :: DownValue
emptyDownValue = M.empty

emptyDown :: Down
emptyDown = Down M.empty []

nullContext :: Context
nullContext = Context emptyOwnValue emptyDownValue

-- readCont :: Env -> IOThrowsError Context
-- readCont = liftIO . readIORef

mergePatt :: PatternRule -> PatternRule -> PatternRule
mergePatt = (++)
addPatt = (:)

insertPattern :: [LispVal] -> LispVal -> Down -> Down
insertPattern lhs rhs downV =
  pattern %~ addPatt (List lhs,rhs) $ downV

insertValue :: [LispVal] -> LispVal -> Down -> Down
insertValue lhs rhs downV =
  value %~ M.insert (List lhs) rhs $ downV

updateDown :: [LispVal] -> LispVal ->Down -> Down
updateDown lhs
  | isPattern (List lhs) = insertPattern lhs
  | otherwise = insertValue lhs

updateDownValue :: LispVal -> LispVal -> DownValue -> DownValue
updateDownValue val rhs =
  let
    name = getSetName val
    List lhs = getLhs val
    initial = updateDown lhs rhs emptyDown
    update = const (updateDown lhs rhs)
  in
    M.insertWith update name initial

updateContext :: LispVal -> LispVal -> Context -> Context
updateContext (Atom name) rhs =
  own %~ M.insert name rhs
updateContext val@(List _) rhs =
  down %~ updateDownValue val rhs


validSet :: LispVal -> Bool
validSet (List [(Atom "Condition"), p, _]) = validSet p
validSet (List ((Atom "Condition"):_)) = False
validSet (List (Atom _ : _)) = True
validSet (Atom _) = True
validSet _ = False

replaceDown :: Down -> LispVal -> ReplaceResult
replaceDown downV lhs =
  let patt = downV ^. pattern.to (replaceRuleList lhs)-- (msum . map (replace lhs))
      val = downV ^. value.to (M.lookup lhs) in
    case val of
      Nothing -> patt
      just -> return just

getSetName :: LispVal -> T.Text
getSetName (List (Atom "Condition":p:_)) = getSetName p
getSetName (List (Atom name:_)) = name

getLhs :: LispVal -> LispVal
getLhs (List [Atom "Condition", p, t]) =
  List [Atom "Condition", getLhs p, t]
getLhs (List (Atom _: lhs)) = List lhs

unpackLhs :: LispVal -> (T.Text, LispVal)
unpackLhs val = (getSetName val, getLhs val)

replaceDownValue :: LispVal -> DownValue -> Primi
-- replaceDownValue val@(List (Atom name : lhs)) downVal =
replaceDownValue val downVal =
  let name = getSetName val
      lhs = getLhs val in
    liftM (fromMaybe val) $ do
      let downV = M.lookup name downVal
      case downV of
        Nothing -> return Nothing
        Just downV' -> replaceDown downV' lhs

replaceOwnValue :: LispVal -> OwnValue -> LispVal
replaceOwnValue val@(Atom name) ownVal =
  fromMaybe val $ M.lookup name ownVal

replaceContext :: LispVal -> Context -> Primi
replaceContext val@(Atom _) con =
  return $ con ^.own.to (replaceOwnValue val)
replaceContext val@(List _) con =
  con ^. down.to (replaceDownValue val)

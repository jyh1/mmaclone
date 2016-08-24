module Show.Pretty(prettyPrint,showLispVal,printLispVal) where
import Data.DataType
import Data.Number.Number
import Data.List
import qualified Data.Text.IO as T
import qualified Data.Text as T

precedence :: [(T.Text, Int)]
precedence = [
              ("Power", 1),
              ("Times",2),
              ("Plus", 3),
              ("Set", 4),
              ("SetDelayed", 5)
              ]

leastPre :: Int
leastPre = 6

infixForm :: T.Text -> T.Text
infixForm "Set" = "="
infixForm "SetDelayed" = ":="
infixForm "Times" = " "
infixForm "Plus" = "+"
infixForm "Power" = "^"
infixForm x = x

getPrecedence :: T.Text -> Maybe Int
getPrecedence = flip lookup precedence

prettyPrint :: LispVal -> T.Text
prettyPrint = prettyPrint' leastPre

prettyPrint' :: Int -> LispVal -> T.Text
prettyPrint' _ (List (Atom "List" : xs)) =
  let args = map prettyPrint xs in
    curlyBrack $ T.intercalate "," args

prettyPrint' now (List (Atom "Times" : Number (Integer (-1)): xs)) =
  '-' `T.cons` prettyPrint' now (List (Atom "Times" : xs))


prettyPrint' now (List (Atom name : xs)) =
  let prec = getPrecedence name in
  case prec of
    Nothing -> functionWrap name (map prettyPrint xs)
    Just n ->
      let args = map (prettyPrint' n) xs
          form = infixForm name
          result = addInfix form args in
        if now <= n then bracket result else result
prettyPrint' _ x = tshow x

encloseWith :: T.Text -> T.Text -> T.Text -> T.Text
encloseWith a b c = T.concat [a, c, b]

bracket :: T.Text -> T.Text
bracket = encloseWith "(" ")"

curlyBrack :: T.Text -> T.Text
curlyBrack = encloseWith "{" "}"

functionWrap :: T.Text -> [T.Text] -> T.Text
functionWrap fun args = encloseWith (fun `T.append` "[") "]" (T.intercalate "," args)

checkMinus :: T.Text -> Bool
checkMinus s
  | T.head s == '-' = True
  | otherwise = False

addInfix :: T.Text -> [T.Text] -> T.Text
addInfix "+" xs = addInfixRule checkMinus id "+" xs
addInfix "^" xs = addInfixRule checkMinus (T.cons '^' . bracket) "^" xs
addInfix sym xs = T.intercalate sym xs

addInfixRule :: (T.Text -> Bool) -> (T.Text -> T.Text)
   -> T.Text -> [T.Text] -> T.Text
addInfixRule _ _ _ [] = ""
addInfixRule _ _ _ [x] = x
addInfixRule check rule syb (x1:res@(x2:_))
  | check x2 = x1 `T.append` rule (addInfixRule check rule syb res)
  | otherwise = x1 `T.append` syb `T.append` addInfixRule check rule syb res

showLispVal (List [Atom "FullForm",val]) = fullForm val
showLispVal val = prettyPrint val

printLispVal = T.putStrLn . showLispVal

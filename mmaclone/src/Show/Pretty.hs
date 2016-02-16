module Show.Pretty(prettyPrint) where
import Data.DataType
import Data.Number.Number
import Data.List

precedence :: [(String, Int)]
precedence = [
              -- ("List",0)
              ("Power", 1),
              ("Times",2),
              -- ("Divide", 2),
              ("Plus", 3),
              -- ("-", 3),
              ("Set", 4),
              ("SetDelayed", 5)
              ]

leastPre :: Int
leastPre = 6

infixForm :: String -> String
infixForm "Set" = "="
infixForm "SetDelayed" = ":="
infixForm "Times" = " "
infixForm "Plus" = "+"
infixForm "Power" = "^"
infixForm x = x

getPrecedence :: String -> Maybe Int
getPrecedence = flip lookup precedence

prettyPrint :: LispVal -> String
prettyPrint = prettyPrint' leastPre

prettyPrint' :: Int -> LispVal -> String
prettyPrint' _ (List (Atom "List" : xs)) =
  let args = map prettyPrint xs in
    curlyBrack $ intercalate "," args

prettyPrint' now (List (Atom "Times" : Number (Integer (-1)): xs)) =
  '-': prettyPrint' now (List (Atom "Times" : xs))


prettyPrint' now (List (Atom name : xs)) =
  let prec = getPrecedence name in
  case prec of
    Nothing -> functionWrap name (map prettyPrint xs)
    Just n ->
      let args = map (prettyPrint' n) xs
          form = infixForm name
          result = addInfix form args in
        if now <= n then bracket result else result
prettyPrint' _ x = show x

encloseWith :: String -> String -> String -> String
encloseWith a b c = a ++ c ++ b

bracket :: String -> String
bracket = encloseWith "(" ")"

curlyBrack :: String -> String
curlyBrack = encloseWith "{" "}"

functionWrap :: String -> [String] -> String
functionWrap fun args = encloseWith (fun ++ "[") "]" (intercalate "," args)

checkMinus :: String -> Bool
checkMinus ('-':_) = True
checkMinus _ = False

addInfix :: String -> [String] -> String
addInfix "+" xs = addInfixRule checkMinus id "+" xs
addInfix "^" xs = addInfixRule checkMinus (('^':).bracket) "^" xs
addInfix sym xs = intercalate sym xs

addInfixRule :: (String -> Bool) -> (String -> String)
   -> String -> [String] -> String
addInfixRule _ _ _ [] = []
addInfixRule _ _ _ [x] = x
addInfixRule check rule syb (x1:res@(x2:_))
  | check x2 = x1 ++ rule (addInfixRule check rule syb res)
  | otherwise = x1 ++ syb ++ addInfixRule check rule syb res

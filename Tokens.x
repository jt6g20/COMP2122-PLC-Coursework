{ 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9     
$alpha = [a-zA-Z]    
$symbols = [\/ \. \# \: \< \>]

tokens :-
    $white+       ;
    "--".*        ;
    SELECT { \p s -> TokenSelect p }
    FROM { \p s -> TokenFrom p }
    WHERE { \p s -> TokenWhere p }
    OUTPUT { \p s -> TokenOutput p }
    AND { \p s -> TokenAnd p }
    OR { \p s -> TokenOr p }
    \<  { \p s -> TokenLess p }
    \>  { \p s -> TokenGreater p }
    IN { \p s -> TokenIn p }
    \+ { \p s -> TokenAdd p }
    \- { \p s -> TokenSub p }
    \= { \p s -> TokenEq p }

    true { \p s -> TokenTrue p }
    false { \p s -> TokenFalse p }

    "<Subj>" { \p s -> TokenSubj p }
    "<Pred>" { \p s -> TokenPred p }
    "<Obj>" { \p s -> TokenObj p }

    $digit+            { \p s -> TokenInt p (read s) }
    [$alpha $digit $symbols]+   { \p s -> TokenString p s }

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
    TokenSelect AlexPosn |
    TokenFrom AlexPosn |
    TokenWhere AlexPosn |
    TokenOutput AlexPosn |
    TokenAnd AlexPosn |
    TokenOr AlexPosn |
    TokenLess AlexPosn |
    TokenGreater AlexPosn |
    TokenIn AlexPosn |
    TokenAdd AlexPosn |
    TokenSub AlexPosn |
    TokenEq AlexPosn |
    TokenTrue AlexPosn |
    TokenFalse AlexPosn |
    TokenSubj AlexPosn |
    TokenPred AlexPosn |
    TokenObj AlexPosn |
    TokenInt AlexPosn Int |
    TokenString AlexPosn String
  deriving (Eq,Show) 

tokenPosn :: Token -> String
tokenPosn (TokenSelect (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenFrom (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenWhere (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenOutput (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenAnd (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenOr (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenLess (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenGreater (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenIn (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenAdd (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenSub (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenEq (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenTrue (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenFalse (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)

tokenPosn (TokenSubj (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenPred (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenObj (AlexPn _ a b)) = (show a) ++ ":" ++ (show b)

tokenPosn (TokenInt (AlexPn _ a b) _) = (show a) ++ ":" ++ (show b)
tokenPosn (TokenString (AlexPn _ a b) _) = (show a) ++ ":" ++ (show b)
}
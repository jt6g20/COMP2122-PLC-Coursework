{ 
module Grammar where 
import Tokens 
}

%name parseSQL 
%tokentype { Token } 
%error { parseError }
%token 
    SELECT { TokenSelect _ }
    FROM { TokenFrom _ }
    WHERE { TokenWhere _ }
    OUTPUT { TokenOutput _ }
    AND { TokenAnd _ }
    OR { TokenOr _ }

    '<' { TokenLess _ }
    '>'  { TokenGreater _ }
    IN { TokenIn _ }
    '+' { TokenAdd _ }
    '-' { TokenSub _ }
    '=' { TokenEq _ }

    true { TokenTrue _ }
    false { TokenFalse _ }

    '<Subj>' { TokenSubj _ }
    '<Pred>' { TokenPred _ }
    '<Obj>' { TokenObj _ }

    int { TokenInt _ $$ }
    string { TokenString _ $$ }

%% 
Stmt : Query { Stmt $1 }
    | Query OUTPUT string { StmtOutput $1 $3 }
Query : SELECT Attribute FROM File WHERE Condition { QueryCondition $2 $4 $6 }
    | SELECT Attribute FROM File { Query $2 $4 }
File : File AND File { Files $1 $3 }
    | string { File $1 }
Condition : Condition AND Condition { ConditionAND $1 $3 }
    | Condition OR Condition { ConditionOR $1 $3 }
    | int '>' int { Greater $1 $3 }
    | int '<' int { Less $1 $3 }
    | int '=' int { NumEq $1 $3 }
    | Attribute '=' Attribute { AttributeEq $1 $3 }
    | Attribute IN Query { AttributeIn $1 $3 }
Attribute : Attribute AND Attribute { Attributes $1 $3 }
    | '<Subj>' { Subj }
    | '<Pred>' { Pred }
    | '<Obj>' { Obj }
    | string { AttributeString $1 }
    

{ 
parseError :: [Token] -> a
parseError (x:xs) = error ("Parse error:" ++ tokenPosn x)
parseError _ = error "Parse error" 

data Stmt = Stmt Query
          | StmtOutput Query String
            deriving Show 
data Query = QueryCondition Attribute File Condition
            | Query Attribute File
            deriving Show 
data File = Files File File
            | File String
            deriving Show 
data Condition = ConditionAND Condition Condition
                | ConditionOR Condition Condition
                | Greater Int Int
                | Less Int Int
                | NumEq Int Int
                | AttributeEq Attribute Attribute
                | AttributeIn Attribute Query
                deriving Show
data Attribute = Attributes Attribute Attribute
                | Subj
                | Pred
                | Obj
                | AttributeString String
                deriving Show 
}
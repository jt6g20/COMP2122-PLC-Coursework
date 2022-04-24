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
Query : SELECT Attributes FROM File WHERE Condition { QueryCondition $2 $4 $6 }
    | SELECT Attributes FROM File { Query $2 $4 }
File : File AND File { Files $1 $3 }
    | string { File $1 }
Condition : Condition AND Condition { ConditionAND $1 $3 }
    | Condition OR Condition { ConditionOR $1 $3 }
    | Attribute '>' int { Greater $1 $3 }
    | Attribute '<' int { Less $1 $3 }
    | Attribute '=' int { NumEq $1 $3 }
    | Attribute '=' Attribute { AttributeEq $1 $3 }
    | Attribute IN Query { AttributeIn $1 $3 }
Attributes : Attribute Attributes { Attributes $1 $2 }
           | Attribute { $1 }
Attribute : '<Subj>' { Subj }
          | '<Pred>' { Pred }
          | Obj { AttributeObj $1 }
          | string { AttributeString $1 }
          | Boolean { AttributeBoolean $1 }
Obj : '<Obj>' { Obj }
    | '<Obj>' '+' int { ObjAdd $3 }
Boolean : true { True }
        | false { False }

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
                | Greater Attribute Int
                | Less Attribute Int
                | NumEq Attribute Int
                | AttributeEq Attribute Attribute
                | AttributeIn Attribute Query
                deriving Show
data Attribute = Attributes Attribute Attribute
                | Subj
                | Pred
                | AttributeObj Obj
                | AttributeString String
                | AttributeBoolean Bool
                deriving Show
data Obj = Obj
         | ObjAdd Int
         deriving Show
}
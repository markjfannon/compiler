module Data where

-- Type aliases used in parsing and code generation
type Identifier = String
type Label = String
type Stack = [Int]
type VarEnv = [(Identifier, Address)]
type Code = [TAMInst]
type Exprs = [Expr]
type VarContext = [(Identifier,VFType)]

data Declaration = 
  FunDecl Identifier VarDecls VFType Expr 
 | VarDecl Identifier VFType 
 | VarInit Identifier VFType Expr deriving Show 

data VarDecl = Decl Identifier Type deriving Show
type VarDecls = [VarDecl] 
data Type = TyInt | TyBool deriving (Show, Eq)
data VFType = VarType Type | FunType [Type] Type deriving (Show, Eq)
type Error = Maybe String

data Address = LBAdd Int | LBMinus Int | SBAdd Int | SBMinus Int deriving Eq  
instance Show Address where 
  show (LBAdd i) = "[LB + " ++ show i ++ "]"
  show (LBMinus i) = "[LB - " ++ show i ++ "]"
  show (SBAdd i) = "[SB + " ++ show i ++ "]"
  show (SBMinus i) = "[SB - " ++ show i ++ "]"

-- Operators used in the MT language
data UnOperator = Negation | LogicalNegation deriving Show
data BinOperator = 
  Addition
  | Subtraction
  | Multiplication 
  | Division 
  | LogicalOr 
  | LogicalAnd 
  | LessThan 
  | LessThanEqual 
  | GreaterThan 
  | GreaterThanEqual 
  | Equals 
  | NotEqual 
  deriving Show

-- Expressions in the MT language
data Expr = 
 LitInteger Int 
 | LitBool Bool
 | Var Identifier
 | BinOp BinOperator Expr Expr
 | UnOp UnOperator Expr
 | Conditional Expr Expr Expr
 | FuncApp Identifier Exprs
 deriving Show 

-- Commands in the MT language
data Command = 
  Assignment Identifier Expr
 | IfThenElse Expr Command Command
 | While Expr Command
 | GetInt Identifier
 | PrintInt Expr
 | BeginEnd [Command]
 deriving Show 

-- Programs in the MT language, consisting of variable declarations and then a command(s)
data Program = LetIn [Declaration] Command deriving Show

-- The instructions used by the TAM, compiled to using the code generator
data TAMInst 
    = LOADL Int 
    | ADD 
    | SUB 
    | MUL 
    | DIV 
    | NEG 
    | LSS 
    | GRT 
    | EQL 
    | AND 
    | OR 
    | NOT 
    | HALT 
    | GETINT 
    | PUTINT 
    | Label Label  
    | JUMP Label 
    | JUMPIFZ Label 
    | LOAD Address
    | STORE Address
    | CALL Label 
    | RETURN Int Int 
    deriving (Show, Eq)

newtype ST st a = S(st -> (a,st))

instance Functor (ST st) where
  -- fmap :: (a -> b) -> ST st a -> ST st b
  fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative (ST st) where 
  --pure :: a -> ST st a
  pure x = S (\s -> (x,s))
  --(<*>) :: ST st (a -> b) -> ST st a -> ST st b
  stf <*> stx = S (\s ->
     let (f,s')  = app stf s
         (x,s'') = app stx s' in (f x, s''))

instance Monad (ST st) where 
  --return :: a -> ST st a
  return = pure 
  -- (>>=) :: ST st a -> (a -> ST st b) -> ST st b
  st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

-- State transformer utilities
app :: ST st a -> st -> (a,st)
app (S sttr) = sttr

stUpdate :: st -> ST st () 
stUpdate st1 = S(\st0 -> ((),st1))

stState :: ST st st 
stState = S(\st -> (st, st))
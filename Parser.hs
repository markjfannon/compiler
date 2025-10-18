module Parser where
import Data
import Data.Char
import Control.Applicative (Alternative, empty, many, (<|>))

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) src = p src

-- Parser functor definition
instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\src -> map (\(v,src') -> (f v, src'))
               (parse p src))

-- Parser applicative definition
instance Applicative Parser where
    --pure :: a -> Parser a
    pure v = P (\src -> [(v,src)])

    --(<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\src -> concat $
                map (\(g,src') -> parse (fmap g px) src')
                (parse pg src))

-- Parser monad definition
instance Monad Parser where
   --return :: a -> Parser a
   return = pure
    --(>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\src -> concat $
                map (\(v,src') -> parse (f v) src')
                (parse p src))

-- Parser alternative declaration
instance Alternative Parser where
    empty = P (\src -> [])
    p <|> q = P (\src -> case parse p src of
                      [] -> parse q src
                      rs -> rs)


---------------------------
-------- TAM PARSER -------
-- THIS PARSES TAM FILES --
---------------------------

-- Parses a whole TAM file
parseCode :: Parser Code
parseCode = many parseInst

-- Parses a single TAM instruction 
parseInst :: Parser TAMInst
parseInst = do
   parseWhiteString "HALT"
   return HALT
   <|> do
   parseWhiteString "GETINT"
   return GETINT
   <|> do
   parseWhiteString "PUTINT"
   return PUTINT
   <|> do
   parseWhiteString "ADD"
   return ADD
   <|> do
   parseWhiteString "SUB"
   return SUB
   <|> do
   parseWhiteString "MUL"
   return MUL
   <|> do
   parseWhiteString "DIV"
   return DIV
   <|> do
   parseWhiteString "NEG"
   return NEG
   <|> do
   parseWhiteString "LSS"
   return LSS
   <|> do
   parseWhiteString "GRT"
   return GRT
   <|> do
   parseWhiteString "EQL"
   return EQL
   <|> do
   parseWhiteString "AND"
   return AND
   <|> do
   parseWhiteString "OR"
   return OR
   <|> do
   parseWhiteString "NOT"
   return NOT
   <|> do
   parseWhiteString "Label "
   label <- many symbol
   return (Label label)
   <|> do
   parseWhiteString "JUMP "
   label <- many symbol
   return (JUMP label)
   <|> do
   parseWhiteString "JUMPIFZ "
   label <- many symbol
   return (JUMPIFZ label)
   <|> do
   parseWhiteString "LOAD"
   parseWhiteString "["
   parseWhiteString "SB"
   parseWhiteString "+"
   x <- parseNatural
   parseWhiteString "]"
   return (LOAD (SBAdd x))
   <|> do
   parseWhiteString "LOAD"
   parseWhiteString "["
   parseWhiteString "SB"
   parseWhiteString "-"
   x <- parseNatural
   parseWhiteString "]"
   return (LOAD (SBMinus x))
   <|> do
   parseWhiteString "LOAD"
   parseWhiteString "["
   parseWhiteString "LB"
   parseWhiteString "+"
   x <- parseNatural
   parseWhiteString "]"
   return (LOAD (LBAdd x))
   <|> do
   parseWhiteString "LOAD"
   parseWhiteString "["
   parseWhiteString "LB"
   parseWhiteString "-"
   x <- parseNatural
   parseWhiteString "]"
   return (LOAD (LBMinus x))
   <|> do
   parseWhiteString "STORE"
   parseWhiteString "["
   parseWhiteString "LB"
   parseWhiteString "-"
   x <- parseNatural
   parseWhiteString "]"
   return (STORE (LBMinus x))
   <|> do
   parseWhiteString "STORE"
   parseWhiteString "["
   parseWhiteString "SB"
   parseWhiteString "-"
   x <- parseNatural
   parseWhiteString "]"
   return (STORE (SBMinus x))
   <|> do
   parseWhiteString "STORE"
   parseWhiteString "["
   parseWhiteString "SB"
   parseWhiteString "+"
   x <- parseNatural
   parseWhiteString "]"
   return (STORE (SBAdd x))
   <|> do
   parseWhiteString "STORE"
   parseWhiteString "["
   parseWhiteString "LB"
   parseWhiteString "+"
   x <- parseNatural
   parseWhiteString "]"
   return (STORE (LBAdd x))
   <|> do
   parseWhiteString "LOADL"
   LOADL <$> parseNatural
   <|> do 
   parseWhiteString "CALL " 
   CALL <$> many symbol
   <|> do 
   parseWhiteString "RETURN"
   x <- parseNatural 
   RETURN x <$> parseNatural

--- MAIN MINITRIANGLE PARSER ---
--- THIS PARSES .MT FILES ---


-- Parses a full MT program
parseProgram :: Parser Program
parseProgram = do
       parseWhiteString "let"
       decs <- parseDeclarations
       parseWhiteString "in"
       LetIn decs <$> parseCommand

-- Parses a singular MT variable declaration
parseDeclaration :: Parser Declaration
parseDeclaration =
    do parseWhiteString "var"
       name <- parseIdentifier
       parseWhiteString ":"
       t <- parseType
       parseWhiteString ":="
       VarInit name (VarType t) <$> parseExpressionTree
    <|>
    do parseWhiteString "var"
       name <- parseIdentifier
       parseWhiteString ":"
       VarDecl name . VarType <$> parseType
    <|>
    do parseWhiteString "fun"
       id <- parseIdentifier
       parseWhiteString "("
       decls <- parseVarDecls
       parseWhiteString ")"
       parseWhiteString ":"
       t <- parseType
       parseWhiteString "="
       expr <- parseExpressionTree
       let ts = typesFromDecls decls
       return (FunDecl id decls (FunType ts t) expr)

typesFromDecls :: VarDecls -> [Type]
typesFromDecls ((Decl id t):ds) = t : typesFromDecls ds
typesFromDecls [] = []

parseVarDecl :: Parser VarDecl
parseVarDecl = do
   id <- parseIdentifier
   parseWhiteString ":"
   Decl id <$> parseType

parseVarDecls :: Parser VarDecls
parseVarDecls = do
   decl <- parseVarDecl
   parseWhiteString ","
   decls <- parseVarDecls
   return (decl:decls)
   <|> do
   decl <- parseVarDecl
   return [decl]

-- Parses multiple MT variable declarations
parseDeclarations :: Parser [Declaration]
parseDeclarations = do
   d <- parseDeclaration
   parseWhiteString ";"
   ds <- parseDeclarations
   return (d:ds)
   <|> do
   d <- parseDeclaration
   return [d]

-- Parses a MT command, with whitespace
parseCommand :: Parser Command
parseCommand = parseCommand'

-- Parses a MT command
parseCommand' :: Parser Command
parseCommand' = do
       id <- parseIdentifier
       parseWhiteString ":="
       Assignment id <$> parseExpressionTree
    <|> do
       parseWhiteString "if"
       expr <- parseExpressionTree
       parseWhiteString "then"
       c1 <- parseCommand
       parseWhiteString "else"
       IfThenElse expr c1 <$> parseCommand
    <|> do
       parseWhiteString "while"
       expr <- parseExpressionTree
       parseWhiteString "do"
       While expr <$> parseCommand
    <|>
    do parseWhiteString "getint"
       parseWhiteString "("
       id <- parseIdentifier
       parseWhiteString ")"
       return (GetInt id)
    <|>
    do parseWhiteString "printint"
       parseWhiteString "("
       exp <- parseExpressionTree
       parseWhiteString ")"
       return (PrintInt exp)
    <|>
    do parseWhiteString "begin"
       coms <- parseCommands
       parseWhiteString "end"
       return (BeginEnd coms)

-- Parses a series of MT commands
parseCommands :: Parser [Command]
parseCommands = do
       com <- parseCommand
       parseWhiteString ";"
       coms <- parseCommands
       return (com:coms)
    <|> do
        com <- parseCommand
        return [com]

parseIdentifier :: Parser Identifier
parseIdentifier = parseToken parseIdentifier'

parseType :: Parser Type
parseType = do
   parseWhiteString "Boolean"
   return TyBool
   <|> do
   parseWhiteString "Integer"
   return TyInt

parseIdentifier' :: Parser Identifier
parseIdentifier' = do
   char <- character
   alphanums <- many alphanum
   return (char:alphanums)

parseExpressionTree :: Parser Expr
parseExpressionTree = parseCond

parseExpr :: Parser Expr
parseExpr = parseExpr' id

parseMexp :: Parser Expr
parseMexp = parseMexp' id

parseTerm :: Parser Expr
parseTerm = parseTerm' id

parseLor :: Parser Expr
parseLor = parseLor' id

parseLand :: Parser Expr
parseLand = parseLand' id

parseRel :: Parser Expr
parseRel = parseRel' id


-- Parses a series of MT commands
parseExprs :: Parser Exprs
parseExprs = do
       exp <- parseExpressionTree
       parseWhiteString ","
       exps <- parseExprs
       return (exp:exps)
    <|> do
        exp <- parseExpressionTree
        return [exp]

parseCond ::  Parser Expr
parseCond = do
   x <- parseLor
   parseWhiteString "?"
   y <- parseCond
   parseWhiteString ":"
   Conditional x y <$> parseCond
   <|> do parseLor

parseLor' :: (Expr -> Expr) -> Parser Expr
parseLor' f = do
   c1 <- parseLand
   parseWhiteString "||"
   parseLor' (\c2 -> BinOp LogicalOr (f c1) c2)
   <|> do
   x <- parseLand
   return (f x)

parseLand' :: (Expr -> Expr) -> Parser Expr
parseLand' f = do
   c1 <- parseNot
   parseWhiteString "&&"
   parseLand' (\c2 -> BinOp LogicalAnd (f c1) c2)
   <|> do
   x <- parseNot
   return (f x)

parseNot :: Parser Expr
parseNot = do
   parseWhiteString "!"
   n <- parseNot
   return (UnOp LogicalNegation n)
   <|> do
   x <- parseRel
   return x

parseRel' :: (Expr -> Expr) -> Parser Expr
parseRel' f = do
   x <- parseExpr
   parseWhiteString "<"
   parseRel' (\y -> BinOp LessThan (f x) y)
   <|> do
   x <- parseExpr
   parseWhiteString "<="
   parseRel' (\y -> BinOp LessThanEqual (f x) y)
   <|> do
   x <- parseExpr
   parseWhiteString ">"
   parseRel' (\y -> BinOp GreaterThan (f x) y)
   <|> do
   x <- parseExpr
   parseWhiteString ">="
   parseRel' (\y -> BinOp GreaterThanEqual (f x) y)
   <|> do
   x <- parseExpr
   parseWhiteString "=="
   parseRel' (\y -> BinOp Equals (f x) y)
   <|> do
   x <- parseExpr
   parseWhiteString "!="
   parseRel' (\y -> BinOp NotEqual (f x) y)
   <|> do
   x <- parseExpr
   return (f x)

-- Parses for productions of expr in the Arith language
parseExpr' :: (Expr -> Expr) -> Parser Expr
parseExpr' f = do
               x <- parseMexp
               parseWhiteString "+"
               parseExpr' (\y -> BinOp Addition (f x) y)
            <|>
            do x <- parseMexp
               parseWhiteString "-"
               parseExpr' (\y -> BinOp Subtraction (f x) y)
            <|>
            do x <- parseMexp
               return (f x)


-- Parses for productions of mexp in the Arith language
parseMexp' :: (Expr ->  Expr) -> Parser Expr
parseMexp' f = do
               x <- parseTerm
               parseWhiteString "*"
               parseMexp' (\y -> BinOp Multiplication (f x) y)
            <|>
            do x <- parseTerm
               parseWhiteString "/"
               parseMexp' (\y -> BinOp Division (f x) y)
            <|>
            do f <$> parseTerm


-- Parses for productions of term in the arith language
parseTerm' :: (Expr -> Expr) -> Parser Expr
parseTerm' f = do
               LitInteger <$> parseNatural
            <|>
            do parseWhiteString "-"
               parseTerm' (\x -> UnOp Negation (f x))
            <|>
            do parseWhiteString "("
               x <- parseExpressionTree
               parseWhiteString ")"
               return (f x)
            <|> do
               id <- parseIdentifier
               parseWhiteString "("
               exprs <- parseExprs
               parseWhiteString ")"
               return (FuncApp id exprs)
            <|> do
               parseWhiteString "True"
               return (LitBool True)
            <|> do
               parseWhiteString "False"
               return (LitBool False)
            <|> do
               parseWhiteString "true"
               return (LitBool True)
            <|> do
               parseWhiteString "false"
               return (LitBool False)
            <|> do
            Var <$> parseIdentifier


------------------------------
----- UTILITY PARSERS! -------
------------------------------

-- Parses a natural number                
parseNat :: Parser Int
parseNat = P (\src -> let (ds,src') = span isDigit src
              in if ds=="" then []
              else [(read ds,src')])

-- Parses spaces, discards them
parseSpace :: Parser ()
parseSpace = P (\src -> let (ds,src') = span isSpace src in [((),src')])

-- Parses tokens - i.e whitespace and then something else
parseToken :: Parser a -> Parser a
-- parseToken pa = P (concatMap (\(_,src) -> parse pa src) . parse parseSpace)
parseToken pa = parseSpace >> pa

-- Parses natural numbers that may have spaces before them
parseNatural :: Parser Int
parseNatural = parseToken parseNat

-- Parses specific strings
parseString :: String -> Parser String
parseString [] = return []
parseString (x:xs) = do
                   char x
                   parseString xs
                   return (x:xs)

-- Parses a string, with whitespace
parseWhiteString :: String -> Parser String
parseWhiteString a = parseToken (parseString a)

-- Always fails
pFail :: Parser a
pFail = P (\src -> [])

-- Parses a specific character
char :: Char -> Parser Char
char x = satisfy item (==x)

-- Creates a parser to meet a predicate
satisfy :: Parser a -> (a -> Bool) -> Parser a
satisfy p cond = do x <- p
                    if cond x
                    then return x
                    else pFail

-- Parses alphabetical characters
character :: Parser Char
character = satisfy item isAlpha

-- Parses alphanumerics
alphanum :: Parser Char
alphanum = satisfy item isAlphaNum

symbol :: Parser Char
symbol = satisfy item isPrint

-- Parses a single item
item :: Parser Char
item = P (\src -> case src of
           [] -> []
           (c:src') -> ([(c, src')]))


module CodeGenerator where
import Data

-- Generates fresh labels for use in jump instructions
fresh :: ST (VarEnv, Int, Int) Label
fresh = do (env, addr, n) <- stState
           stUpdate (env, addr, n+1)
           return ('#':show n)

-- Compiles an entire MT program into TAM instructions, using commCode and declTAM
progCode :: Program -> Code
progCode (LetIn decs com) = concat inits ++ body ++ [HALT] ++ concat funcs where
    (inits, (env,addr,n)) = app (mapM declTAM (filter (not . isFunction) decs)) ([],0, 0)
    (body, (env',addr',n')) = app (commCode env com) (env,addr,n)
    (funcs, (env'',addr'',n'')) = app (mapM funcTAM (filter isFunction decs)) (env',0,n')


-- Compiles MT commands into TAM instructions
commCode :: VarEnv -> Command -> ST (VarEnv, Int, Int) Code
commCode env (Assignment id exp) = do
    let addr = findAddress env id
    exp' <- expCode exp env
    return (exp' ++ [STORE addr])
    where addr = findAddress env id
commCode env (GetInt x) = return (GETINT : [STORE addr])
    where addr = findAddress env x
commCode env (PrintInt exp) = do
    exp' <- expCode exp env
    return (exp' ++ [PUTINT])
commCode env (BeginEnd []) = return []
commCode env (BeginEnd (c:cs)) = do
    c' <- commCode env c
    cs' <- commCode env (BeginEnd cs)
    return (c'++cs')
commCode env (IfThenElse exp c1 c2) = do
    exp' <- expCode exp env
    l1 <- fresh
    l2 <- fresh
    c1' <- commCode env c1
    c2' <- commCode env c2
    return (exp' ++ [JUMPIFZ l1] ++ c1' ++ [JUMP l2] ++ [Label l1] ++ c2' ++ [Label l2])
commCode env (While exp c) = do
    l1 <- fresh
    exp' <- expCode exp env
    l2 <- fresh
    c' <- commCode env c
    return ([Label l1] ++ exp' ++ [JUMPIFZ l2] ++ c' ++ [JUMP l1] ++ [Label l2])

-- Compiles MT expressions into TAM instructions
expCode :: Expr -> VarEnv -> ST (VarEnv, Int, Int) Code
expCode (LitInteger i) env = return [LOADL i]
expCode (LitBool True) env = return [LOADL 1]
expCode (LitBool False) env = return [LOADL 0]
expCode (Var id) env = return [LOAD addr]
  where addr = findAddress env id
expCode (BinOp Addition e1 e2) env = do
    e1' <- expCode e1 env
    e2' <- expCode e2 env
    return (e1' ++ e2' ++ [ADD])
expCode (BinOp Multiplication e1 e2) env = do
    e1' <- expCode e1 env
    e2' <- expCode e2 env
    return (e1' ++ e2' ++ [MUL])
expCode (BinOp Division e1 e2) env = do
    e1' <- expCode e1 env
    e2' <- expCode e2 env
    return (e1' ++ e2' ++ [DIV])
expCode (BinOp Subtraction e1 e2) env = do
    e1' <- expCode e1 env
    e2' <- expCode e2 env
    return (e1' ++ e2' ++ [SUB])
expCode (BinOp LessThan e1 e2) env = do
    e1' <- expCode e1 env
    e2' <- expCode e2 env
    return (e1' ++ e2' ++ [LSS])
expCode (BinOp GreaterThan e1 e2) env = do
    e1' <- expCode e1 env
    e2' <- expCode e2 env
    return (e1' ++ e2' ++ [GRT])
expCode (BinOp LessThanEqual e1 e2) env = do
    e1' <- expCode e1 env
    e2' <- expCode e2 env
    return (e1' ++ e2' ++ [LSS] ++ e1' ++ e2' ++ [EQL] ++ [OR])
expCode (BinOp GreaterThanEqual e1 e2) env = do
    e1' <- expCode e1 env
    e2' <- expCode e2 env
    return (e1' ++ e2' ++ [GRT] ++ e1' ++ e2' ++ [EQL] ++ [OR])
expCode (BinOp LogicalOr e1 e2) env = do
    e1' <- expCode e1 env
    e2' <- expCode e2 env
    return (e1' ++ e2' ++ [OR])
expCode (BinOp LogicalAnd e1 e2) env = do
    e1' <- expCode e1 env
    e2' <- expCode e2 env
    return (e1' ++ e2' ++ [AND])
expCode (BinOp Equals e1 e2) env = do
    e1' <- expCode e1 env
    e2' <- expCode e2 env
    return (e1' ++ e2' ++ [EQL])
expCode (BinOp NotEqual e1 e2) env = do
    e1' <- expCode e1 env
    e2' <- expCode e2 env
    return (e1' ++ e2' ++ [EQL] ++ [NOT])
expCode (UnOp Negation e) env = do
    e' <- expCode e env
    return (e' ++ [NEG])
expCode (UnOp LogicalNegation e) env = do
    e' <- expCode e env
    return (e' ++ [NOT])
expCode (Conditional e1 e2 e3) env = do
    e1' <- expCode e1 env
    l1 <- fresh
    e2' <- expCode e2 env
    l2 <- fresh
    e3' <- expCode e3 env
    return (e1' ++ [JUMPIFZ l1] ++ e2' ++ [JUMP l2] ++ [Label l1] ++ e3' ++ [Label l2])
expCode (FuncApp id exps) env = do
    x <- mapM (`expCode` env) (reverse exps)
    return (concat x ++ [CALL id])

isFunction :: Declaration -> Bool
isFunction (FunDecl {} ) = True
isFunction _ = False

-- Compiles variable declarations into TAM commands
declTAM :: Declaration -> ST (VarEnv, Int, Int) Code
declTAM (VarDecl id t) = do
    (env, addr, i) <- stState
    stUpdate ((id, SBAdd addr):env, addr+1,i)
    return [LOADL 0]
declTAM (VarInit id t exp) = do
    (env, addr, i) <- stState
    exp' <- expCode exp env
    (env, addr, i) <- stState
    stUpdate ((id, SBAdd addr):env, addr+1, i)
    return exp'

funcTAM :: Declaration -> ST (VarEnv, Int, Int) Code
funcTAM (FunDecl id decls ftype exp) = do
    (env, addr, i) <- stState
    let lenv = localEnv decls
    exp' <- expCode exp (lenv++env)
    return (Label id : exp' ++ [RETURN 1 (length decls)])

localEnv = localEnv' 1

localEnv' :: Int -> VarDecls -> VarEnv
localEnv' n [] = []
localEnv' n ((Decl id _):ds) = (id, LBMinus n):localEnv' (n+1) ds


-- Finds a stack address for a given identifier, searching the variable environment
-- Returns 0 if it can't find the identifier (though this shouldn't happen!)
findAddress :: VarEnv -> Identifier -> Address
findAddress (x:xs) id = let (name, addr) = x
                        in if name == id
                            then addr
                            else findAddress xs id
findAddress [] id = SBAdd 0



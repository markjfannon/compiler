module TypeChecker where
import Data

checkDecls :: [Declaration] -> ST VarContext Error
checkDecls [] = return Nothing
checkDecls ((VarDecl id t):decs) = do
    context <- stState
    case checkContext id t context of
        Nothing -> do
           stUpdate (context ++ [(id, t)])
           checkDecls decs
        Just (VarType t') -> do
            stUpdate (context ++ [(id, t)])
            let strt = typeToString t'
            otherErrors <- checkDecls decs
            case otherErrors of
                Nothing -> return (Just ("TypeError: Variable " ++
                 id ++ " has already been declared as "
                 ++ strt ++ "\n"))
                Just e -> return (Just ("TypeError: Variable " ++
                 id ++ " has already been declared as "
                  ++ strt ++ "\n" ++ e))
checkDecls ((VarInit id t exp):decs) = do
    context <- stState
    case checkContext id t context of
        Nothing -> do
            case typeCheckExpr context exp of
                Nothing -> do
                    stUpdate (context ++ [(id, t)])
                    otherErrors <- checkDecls decs
                    let (VarType t') = t
                    let strt = typeToString t'
                    case otherErrors of
                        Nothing ->
                            return (Just ("TypeError: Variable " ++ id
                                ++ " has been declared as "
                                ++ strt ++ " but has been defined incorrectly" ++ "\n"))
                        Just e ->
                            return (Just ("TypeError: Variable " ++ id
                                ++ " has been declared as "
                                ++ strt ++ " but has been defined incorrectly" ++ "\n" ++ e))
                Just actualtype -> if VarType actualtype == t then do
                    stUpdate (context ++ [(id, t)])
                    checkDecls decs
                    else do
                        stUpdate (context ++ [(id, t)])
                        let stract = typeToString actualtype
                        otherErrors <- checkDecls decs
                        let (VarType t') = t
                        let strt = typeToString t'
                        case otherErrors of
                           Nothing -> do
                            return (Just ("TypeError: Variable " ++ id
                             ++ " has been declared as "
                             ++ strt ++ " but assigned "
                             ++ stract ++ " value\n"))
                           Just e -> return (Just ("TypeError: Variable " ++ id ++ " has been declared as "++ strt ++ " but has been assigned " ++ stract ++ " value\n" ++ e))

        Just (VarType x) -> do
            otherErrors <- checkDecls decs
            let strt = typeToString x
            case otherErrors of
              Nothing -> return (Just ("TypeError: Variable " ++ id ++ " has already been declared as " ++ strt ++ "\n"))
              Just e -> return (Just ("TypeError: Variable " ++ id ++ " has already been declared as " ++ strt ++ "\n" ++ e))
checkDecls ((FunDecl id decls typ exp):decs) = do
    context <- stState
    let localContext = buildLocalContext [] decls
    case localContext of
        Nothing -> do
            errs <- checkDecls decs
            case errs of
                Nothing -> return (Just ("TypeError: Function declaration for " ++ id ++ "has incorrect variable usage"))
                Just e -> return (Just ("TypeError: Function declaration for " ++ id ++ "has incorrect variable usage\n" ++ e))
        Just ct -> do
                    let eType = typeCheckExpr ((id,typ):ct++context) exp
                    let (FunType argts rett) = typ
                    case eType of
                        Nothing -> return (Just ("TypeError: Function " ++ id ++
                         " has invalid definition, was expecting " ++ typeToString rett ++ "\n"))
                        Just x -> if x == rett then do
                            stUpdate (context ++ [(id, typ)])
                            checkDecls decs
                        else do
                            errs <- checkDecls decs
                            case errs of
                                Nothing -> return (Just ("TypeError: Function is meant to return "
                                 ++ typeToString rett ++ " but returns " ++ typeToString x ++ "\n"))
                                Just e -> return (Just e)

buildLocalContext :: VarContext -> VarDecls -> Maybe VarContext
buildLocalContext c [] = Just c
buildLocalContext c ((Decl id typ):ds) = case getVarType id c of
    Nothing -> buildLocalContext ((id,VarType typ) : c) ds
    Just x -> Nothing

typeCheckExpr :: VarContext -> Expr -> Maybe Type
typeCheckExpr c (LitInteger i) = Just TyInt
typeCheckExpr c (LitBool t) = Just TyBool
typeCheckExpr c (Var id) = Just =<< getVarType id c
typeCheckExpr c (BinOp LogicalAnd e1 e2) =
    case (typeCheckExpr c e1, typeCheckExpr c e2) of
    (Just TyBool, Just TyBool) -> Just TyBool
    _ -> Nothing
typeCheckExpr c (BinOp LogicalOr e1 e2) =
    case (typeCheckExpr c e1, typeCheckExpr c e2) of
    (Just TyBool, Just TyBool) -> Just TyBool
    _ -> Nothing
typeCheckExpr c (BinOp GreaterThan e1 e2) = case (typeCheckExpr c e1, typeCheckExpr c e2) of
    (Just TyInt, Just TyInt) -> Just TyBool
    _ -> Nothing
typeCheckExpr c (BinOp GreaterThanEqual e1 e2) = case (typeCheckExpr c e1, typeCheckExpr c e2) of
    (Just TyInt, Just TyInt) -> Just TyBool
    _ -> Nothing
typeCheckExpr c (BinOp LessThan e1 e2) = case (typeCheckExpr c e1, typeCheckExpr c e2) of
    (Just TyInt, Just TyInt) -> Just TyBool
    _ -> Nothing
typeCheckExpr c (BinOp LessThanEqual e1 e2) = case (typeCheckExpr c e1, typeCheckExpr c e2) of
    (Just TyInt, Just TyInt) -> Just TyBool
    _ -> Nothing
typeCheckExpr c (BinOp Equals e1 e2) = case (typeCheckExpr c e1, typeCheckExpr c e2) of
    (Just TyInt, Just TyInt) -> Just TyBool
    _ -> Nothing
typeCheckExpr c (BinOp NotEqual e1 e2) = case (typeCheckExpr c e1, typeCheckExpr c e2) of
    (Just TyInt, Just TyInt) -> Just TyBool
    _ -> Nothing
typeCheckExpr c (BinOp op e1 e2) = case (typeCheckExpr c e1, typeCheckExpr c e2) of
    (Just TyInt, Just TyInt) -> Just TyInt
    _ -> Nothing
typeCheckExpr c (UnOp Negation e) = case typeCheckExpr c e of
    Just TyInt -> Just TyInt
    _ -> Nothing
typeCheckExpr c (UnOp LogicalNegation e) = case typeCheckExpr c e of
    Just TyBool -> Just TyBool
    _ -> Nothing
typeCheckExpr c (Conditional e1 e2 e3) = case (typeCheckExpr c e1, typeCheckExpr c e2, typeCheckExpr c e3) of
    (Just TyBool, Just x, Just y) -> if x == y then Just x else Nothing
    _ -> Nothing
typeCheckExpr c (FuncApp id exps) =  do
    case getFunType id c of
        Nothing -> Nothing
        Just (FunType args ret) -> if length args /= length exps then Nothing else
             do
               let pairs = zip args (map (typeCheckExpr c) exps)
               if checkPairs pairs then Just ret else Nothing

checkPairs :: [(Type, Maybe Type)] -> Bool
checkPairs [] = True
checkPairs ((t, Nothing):ts) = False
checkPairs ((t, Just t'):ts) = (t == t') && checkPairs ts

typeToString :: Type -> String
typeToString TyInt =  "an Integer"
typeToString TyBool = "a Boolean"

typeCheckCommand :: Command -> ST VarContext Error
typeCheckCommand (PrintInt e) = do
    context <- stState
    case typeCheckExpr context e of
        Nothing -> return (Just "TypeError: PrintInt expected an Integer, was provided an invalid expression\n")
        Just TyBool -> return (Just "TypeError: PrintInt expected an Integer, was provided Boolean\n")
        Just TyInt -> return Nothing
typeCheckCommand (GetInt id) = do
    context <- stState
    case getVarType id context of
        Nothing -> return (Just ("VarError: Variable " ++ id ++ " does not exist\n"))
        Just TyBool -> return (Just ("TypeError: Variable " ++ id ++ " is boolean, not integer\n"))
        Just TyInt -> return Nothing
typeCheckCommand (Assignment id e) = do
    context <- stState
    case getVarType id context of
        Nothing -> return (Just ("VarError: Variable " ++ id ++ " does not exist\n"))
        Just t -> case typeCheckExpr context e of
            Nothing -> return (Just ("TypeError: Variable " ++ id ++ " is " ++ typeToString t ++ " but was given an invalid expression\n"))
            Just t' -> if t == t' then return Nothing else return (Just ("TypeError: Variable "
             ++ id ++ " is " ++ typeToString t
             ++ " but was assigned " ++ typeToString t' ++ " value\n"))
typeCheckCommand (BeginEnd (com:coms)) = do
    com' <- typeCheckCommand com
    coms' <- typeCheckCommand (BeginEnd coms)
    case (com', coms') of
        (Nothing, Nothing) -> return Nothing
        (Just e, Nothing) -> return (Just e)
        (Just e1, Just e2) -> return (Just (e1 ++ e2))
        (Nothing, Just e) -> return (Just e)
typeCheckCommand (BeginEnd []) = return Nothing
typeCheckCommand (IfThenElse exp c1 c2) = do
    context <- stState
    c1' <- typeCheckCommand c1
    c2' <- typeCheckCommand c2
    case typeCheckExpr context exp of
        Just TyBool -> do
            case (c1', c2') of
                (Nothing, Nothing) -> return Nothing
                (Just e, Nothing) -> return (Just e)
                (Nothing, Just e) -> return (Just e)
                (Just e1, Just e2) -> return (Just (e1 ++ e2))
        _ -> case (c1', c2') of
                (Nothing, Nothing) -> return Nothing
                (Just e, Nothing) -> return (Just ("TypeError: If statement requires a boolean condition\n" ++ e))
                (Nothing, Just e) -> return (Just ("TypeError: If statement requires a boolean condition\n" ++ e))
                (Just e1, Just e2) -> return (Just ("TypeError: If statement requires a boolean condition\n" ++ e1 ++ e2))
typeCheckCommand (While exp com) = do
    context <- stState
    com' <- typeCheckCommand com
    case typeCheckExpr context exp of
        Just TyBool -> case com' of
            Nothing -> return Nothing
            Just e -> return (Just e)
        _ -> case com' of
            Nothing -> return (Just "TypeError: While loop requires boolean condition\n")
            Just e -> return (Just ("TypeError: While loop requires boolean condition\n" ++ e))


getVarType :: Identifier -> VarContext -> Maybe Type
getVarType id [] = Nothing
getVarType id ((identifier, VarType typ):cs) = if id == identifier
                                      then Just typ
                                    else
                                        getVarType id cs
getVarType id (_:cs) = getVarType id cs

getFunType :: Identifier -> VarContext -> Maybe VFType
getFunType id [] = Nothing
getFunType id ((identifier, FunType args ret):cs) = if id == identifier
                                      then Just (FunType args ret)
                                    else
                                        getFunType id cs
getFunType id (_:cs) = getFunType id cs


checkContext :: Identifier -> VFType -> VarContext -> Maybe VFType
checkContext id t [] = Nothing
checkContext id t ((identifier, typ):cs) = if id == identifier
                                              then Just typ
                                           else checkContext id t cs




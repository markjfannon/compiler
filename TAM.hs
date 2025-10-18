module TAM where
import Data

newtype StateIO st a = StT (st -> IO (a,st))

-- The TAM state datatype is made up of the code, the program counter, and the stack
data TAMState = TAMState {
    tsCode :: Code,
    tsCounter :: Int,
    tsStack :: Stack,
    tsSB :: Int,
    tsLB :: Int
}

-- Constructs an initial state given some code. Counter and stack are set to 0 and empty
constructInit :: Code -> TAMState
constructInit code = TAMState {
    tsCode = code,
    tsCounter = 0,
    tsStack = [],
    tsSB = 0,
    tsLB = 0
}

-- 1,2,3,4
-- 4,3,2,1
-- 3,2,1
-- 1,2,3
-- Produces a mutated stack, rewriting a given index to a certain value
writeVar :: Address -> Int -> TAMState -> Stack
writeVar (SBAdd 0) val st = frontStack++[val] where
    stk = tsStack st
    frontStack = init stk
writeVar (SBAdd i) val st = xs++val:tail ys where
    stk = tsStack st
    sb = tsSB st
    (xs,ys) = splitAt addr stk
    addr = length stk - 1 - sb - i
writeVar (LBAdd i) val st = xs++val:tail ys where
    stk = tsStack st
    lb = tsLB st
    (xs,ys) = splitAt addr stk
    addr = length stk - 1 - lb - i
writeVar (LBMinus i) val st = xs++val:tail ys where
    stk = tsStack st
    lb = tsLB st
    (xs,ys) = splitAt addr stk
    addr = length stk - 1 - lb + i


accessAddress :: TAMState -> Address -> Int
accessAddress s (SBAdd i) = let sb = tsSB s
                                stk = tsStack s
                            in stk !! (length stk - 1 - sb - i)
accessAddress s (LBAdd i) = let lb = tsLB s
                                stk = tsStack s
                            in stk !! (length stk - 1 - lb - i)
accessAddress s (LBMinus i) = let lb = tsLB s
                                  stk = tsStack s
                            in stk !! (length stk - 1 - lb + i)


-- Gets the instruction index of a Label
-- This is used when performing jumps
getInst :: Code -> Label -> Int
getInst = getInst' 0

getInst' :: Int -> Code -> Label -> Int
getInst' n (Label lab:cs) l = if lab == l
    then n
    else getInst' (n+1) cs l
getInst' n [] l = 0
getInst' n (c:cs) l = getInst' (n+1) cs l

-- Transforms an IO action into a StateIO action
lift :: IO a -> StateIO st a
lift mx = StT (\s -> do
                     x <- mx
                     return (x,s))


-- Applies the stateIO transformer to a fully ran program
run :: Code -> IO (Stack, TAMState)
run code = appT execFullT (constructInit code)

-- Executes a full TAM program
execFullT :: StateIO TAMState Stack
execFullT = do
    inst <- nextInst
    executeT inst
    --lift $ print ("Just executed: " ++ show inst)
    stk <- stackT
    c <- codeT 
    --lift $ print ("Current stack: " ++ show stk)
    --lift $ print ("Full code: " ++ show c)

    --lift getLine

    if inst == HALT
        then stackT
    else execFullT

-- Gets the next instruction
nextInst :: StateIO TAMState TAMInst
nextInst = do
    tam <- codeT
    c <- counterT
    return (tam!!c)


-- Returns the current local base
lbaseT :: StateIO TAMState Int
lbaseT = fmap tsLB ioState

-- Returns the value of the program counter
counterT :: StateIO TAMState Int
counterT = fmap tsCounter ioState

-- Returns the value of the code 
codeT :: StateIO TAMState Code
codeT = fmap tsCode ioState

-- Returns the stack
stackT :: StateIO TAMState Stack
stackT = fmap tsStack ioState

-- Puts an item on top of the stack
pushT :: Int -> StateIO TAMState ()
pushT x = do
    stk <- stackT
    stackUpdateT (x:stk)

-- Removes item from top of stack, returns it
popT :: StateIO TAMState Int
popT = do
    stk <- stackT
    stackUpdateT (tail stk)
    return (head stk)

-- Updates the program counter to a certain value
countUpdateT :: Int -> StateIO TAMState ()
countUpdateT c = do
                 ts <- ioState
                 ioUpdate (ts{tsCounter = c})

-- Updates the stack to a certain value
stackUpdateT :: Stack -> StateIO TAMState ()
stackUpdateT s = do
    ts <- ioState
    ioUpdate (ts{tsStack = s})

lbaseUpdateT :: Int -> StateIO TAMState ()
lbaseUpdateT lb = do
    ts <- ioState
    ioUpdate (ts{tsLB = lb})

-- Increments the program counter
continueT :: StateIO TAMState ()
continueT = do
              c <- counterT
              countUpdateT (c+1)

-- Executes a single TAM instruction
executeT :: TAMInst -> StateIO TAMState ()
executeT HALT = return ()
executeT GETINT = do
    lift $ putStrLn "Enter a number: "
    s <- lift getLine
    pushT (read s)
    continueT
executeT PUTINT = do
    x <- popT
    lift $ putStrLn ("Output > " ++ show x)
    continueT
executeT (Label _) = do
    continueT
executeT (LOADL i) = do
    pushT i
    continueT
executeT ADD = do
    x <- popT
    y <- popT
    pushT (x + y)
    continueT
executeT SUB = do
    y <- popT
    x <- popT
    pushT (x - y)
    continueT
executeT DIV = do
    y <- popT
    x <- popT
    pushT (x `div` y)
    continueT
executeT MUL = do
    x <- popT
    y <- popT
    pushT (x * y)
    continueT
executeT LSS = do
    y <- popT
    x <- popT
    if x < y then
        pushT 1
    else
        pushT 0
    continueT
executeT GRT = do
    y <- popT
    x <- popT
    if x > y then
        pushT 1
    else
        pushT 0
    continueT
executeT EQL = do
    x <- popT
    y <- popT
    if x == y then
        pushT 1
    else
        pushT 0
    continueT
executeT NEG = do
    x <- popT
    pushT (-x)
    continueT
executeT OR = do
    x <- popT
    y <- popT
    if x == 1 || y == 1 then
        pushT 1
    else
        pushT 0
    continueT
executeT AND = do
    x <- popT
    y <- popT
    if x == 1 && y == 1 then
        pushT 1
    else
        pushT 0
    continueT
executeT NOT = do
    x <- popT
    if x == 1 then
        pushT 0
    else
        pushT 1
    continueT
executeT (JUMP l) = do
    code <- codeT
    let x = getInst code l in
       countUpdateT x
    continueT
executeT (JUMPIFZ l) = do
    x <- popT
    if x == 0 then do
        code <- codeT
        let x = getInst code l in
         countUpdateT x
        continueT
    else continueT
executeT (LOAD addr) = do
    st <- ioState
    let item = accessAddress st addr in
        pushT item
    continueT
executeT (STORE addr) = do
    x <- popT
    st <- ioState
    stackUpdateT (writeVar addr x st)
    continueT
executeT (CALL func) = do
    ra <- counterT 
    cd <- codeT
    lb <- lbaseT
    pushT lb
    stk <- stackT
    pushT (ra+1)
    lbaseUpdateT (length stk - 1)
    let x = getInst cd func in
         countUpdateT x
executeT (RETURN ret args) = do
    stk <- stackT
    let (ra,lb) = getAcRecord stk ret 
    stackUpdateT (clearAcRecord stk ret args)
    lbaseUpdateT lb
    countUpdateT ra

getAcRecord :: Stack -> Int -> (Int,Int)
getAcRecord stk i = let (rets,arstack) = splitAt i stk
                        [ra,lb] = take 2 arstack 
                    in (ra,lb)
   

clearAcRecord :: Stack -> Int -> Int -> Stack 
clearAcRecord stk returncount argcount = let (rets,arstack) = splitAt returncount stk in rets ++ drop (2+argcount) arstack
    

-- FALSE = 0
-- ANYTHING ELSE = TRUE (though normally good practice to do 1)

-- Returns the current state of the StateIO monad
ioState :: StateIO st st
ioState = StT (\s -> return (s,s))

-- Updates the state of the StateIO monad
ioUpdate :: st -> StateIO st ()
ioUpdate s = StT (\_ -> return ((),s))

-- Applies the StateIO transformer
appT :: StateIO st a -> st -> IO (a, st)
appT (StT st) = st

-- Returns the result of a StateIO transformer
resultT :: StateIO st a -> st -> IO a
resultT st s = do
        (x,_) <- appT st s
        return x

-- Functor, Applicative and Monad definitions of StateIO :D 
instance Functor (StateIO st) where
    fmap g st = StT (\s -> do
         (x,s') <- appT st s
         return (g x, s'))

instance Applicative (StateIO st) where
    pure x = StT (\s -> return (x,s))
    (<*>) :: StateIO st (a -> b) -> StateIO st a -> StateIO st b
    stf <*> stx = StT (\s -> do
     (f,s') <- appT stf s
     (x,s'') <- appT stx s'
     return (f x, s''))

instance Monad (StateIO st) where
    return = pure
    st >>= f = StT (\s -> do
         (x,s') <- appT st s
         appT (f x) s')


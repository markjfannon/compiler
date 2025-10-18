module Main where
import Data
import Parser 
import TAM 
import CodeGenerator
import System.Environment (getArgs)
import TypeChecker

main :: IO ()
main = do
       a <- getArgs
       let (name, ext) = getNameExt (head a)
       if ext == "tam" then
           do
            file <- readFile (head a)
            (stk, _) <- run (fst (head (parse parseCode file)))
            print ("Final stack: " ++ show stk)
        else if ext == "mt" then 
            do 
                file <- readFile (head a)
                let (LetIn decs coms) = fst (head (parse parseProgram file))
                let (potErrs, cont) = app (checkDecls decs) []
                case potErrs of 
                    Nothing -> do 
                        putStr "INFO: Type check of declarations complete. No errors to report. Type checking program body...\n"
                        let (potErrs, _) = app (typeCheckCommand coms) cont
                        case potErrs of 
                            Nothing -> do 
                                putStr ("INFO: Type check of program complete. No errors to report. Generating " ++ name ++ ".tam\n")
                                writeFile (name ++".tam") (unlines (map show (progCode (LetIn decs coms))))
                            Just errs -> putStr errs
                    Just errs -> do 
                        putStr ("INFO: No TAM file has been generated due to the following errors:\n" ++ errs) 
                        let (potErrs, _) = app (typeCheckCommand coms) cont
                        case potErrs of 
                            Nothing -> putStr ""
                            Just errs -> putStr errs
                
        else 
            print "ERR: Incorrect File Type. Please provide a valid .mt or .tam file"



-- Because I forgot we could use Data.List (splitAt)...
getNameExt :: String -> (String, String)
getNameExt = getNameExt' []

getNameExt' :: String -> String -> (String, String)
getNameExt' xs ys = if head ys == '.' 
    then (xs, tail ys) 
    else getNameExt' (xs++[head ys]) (tail ys)

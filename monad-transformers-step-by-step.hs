{-# LANGUAGE ImportQualifiedPost #-}
-- | Monad transformers step by step

module Main where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe
import Data.Map qualified as Map

main = return ()

type Name = String
data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
  deriving (Show)
data Value = IntVal Integer
           | FunVal Env Name Exp
  deriving (Show)
type Env = Map.Map Name Value


-------------------------------------------------
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
-------------------------------------------------
eval0 :: Env -> Exp -> Value
eval0 env   (Lit i)      = IntVal i
eval0 env (Var n)      = fromJust (Map.lookup n env)  -- possible error
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1 -- possible error
                             IntVal i2 = eval0 env e2 -- possible error
                         in  IntVal (i1 + i2)
eval0 env (Abs n e)    = FunVal env n e
eval0 env (App f v)    = let val1 = eval0 env f
                             val2 = eval0 env v
                         in case val1 of            -- possible error
                              FunVal env' n body -> eval0 (Map.insert n val2 env') body

-------------------------------------------------
type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 e = runIdentity e

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i)      = return $ IntVal i
eval1 env (Var n)      = case Map.lookup n env of
                           Nothing -> error ("unbound variable " ++ n)
                           Just v  -> return v
eval1 env (Plus e1 e2) = do ~(IntVal i1) <- eval1 env e1
                            ~(IntVal i2) <- eval1 env e2
                            return $ IntVal (i1 + i2)
eval1 env (Abs n e)    = return $ FunVal env n e
eval1 env (App f e)    = do val1 <- eval1 env f
                            val2 <- eval1 env e
                            case val1 of
                              FunVal env' n body ->
                                eval1 (Map.insert n val2 env') body


-------------------------------------------------
type ExceptionRep = String
type Eval2 a      = ExceptT ExceptionRep Identity a

runEval2 :: Eval2 a -> Either ExceptionRep a
runEval2 e = runIdentity (runExceptT e)

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i)      = return $ IntVal i
eval2a env (Var n)      = case Map.lookup n env of
                           Nothing -> error ("unbound variable " ++ n)
                           Just v  -> return v
eval2a env (Plus e1 e2) = do ~(IntVal i1) <- eval2a env e1
                             ~(IntVal i2) <- eval2a env e2
                             return $ IntVal (i1 + i2)
eval2a env (Abs n e)    = return $ FunVal env n e
eval2a env (App f e)    = do val1 <- eval2a env f
                             val2 <- eval2a env e
                             case val1 of
                               FunVal env' n body ->
                                 eval2a (Map.insert n val2 env') body

-------------------------------------------------
eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i)      = return $ IntVal i
eval2b env (Var n)      = case Map.lookup n env of
                           Nothing -> throwError ("unbound variable: " ++ n)
                           Just v  -> return v
eval2b env (Plus e1 e2) = do  e1' <- eval2b env e1
                              e2' <- eval2b env e2
                              case (e1', e2') of
                                (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                _                      -> throwError "Plus: type error"
eval2b env (Abs n e)    = return $ FunVal env n e
eval2b env (App f e)    = do val1 <- eval2b env f
                             val2 <- eval2b env e
                             case val1 of
                               FunVal env' n body ->
                                 eval2b (Map.insert n val2 env') body
                               _                  -> throwError "App: type error"

-------------------------------------------------
eval2c :: Env -> Exp -> Eval2 Value
eval2c env (Lit i)      = return $ IntVal i
eval2c env (Var n)      = case Map.lookup n env of
                           Nothing -> throwError ("unbound variable: " ++ n)
                           Just v  -> return v
eval2c env (Plus e1 e2) = do  ~(IntVal i1) <- eval2c env e1
                              ~(IntVal i2) <- eval2c env e2
                              return $ IntVal (i1 + i2)
eval2c env (Abs n e)    = return $ FunVal env n e
eval2c env (App f e)    = do ~(FunVal env' n body) <- eval2c env f
                             val2 <- eval2c env e
                             eval2c (Map.insert n val2 env') body

-------------------------------------------------
eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i)      = return $ IntVal i
eval2 env (Var n)      = case Map.lookup n env of
                           Nothing -> throwError ("unbound variable: " ++ n)
                           Just v  -> return v
eval2 env (Plus e1 e2) = do  e1' <- eval2 env e1
                             e2' <- eval2 env e2
                             case (e1', e2') of
                               (IntVal i1, IntVal i2) ->
                                 return $ IntVal (i1 + i2)
                               _                      ->
                                 throwError "Plus: type error"
eval2 env (Abs n e)    = return $ FunVal env n e
eval2 env (App f e)    = do val1 <- eval2 env f
                            val2 <- eval2 env e
                            case val1 of
                              FunVal env' n body ->
                                eval2 (Map.insert n val2 env') body
                              _                  ->
                                throwError "App: type error"


-------------------------------------------------
type Eval3 a = ReaderT Env (ExceptT ExceptionRep Identity) a

runEval3 :: Env -> Eval3 a -> Either ExceptionRep a
runEval3 env e = runIdentity (runExceptT (runReaderT e env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i)      = return $ IntVal i
eval3 (Var n)      = do env <- ask
                        case Map.lookup n env of
                          Nothing -> throwError ("unbound variable: " ++ n)
                          Just v  -> return v
eval3 (Plus e1 e2) = do e1' <- eval3 e1
                        e2' <- eval3 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) ->
                            return $ IntVal (i1 + i2)
                          _                      ->
                            throwError "Plus: type error"
eval3 (Abs n e)    = do env <- ask
                        return $ FunVal env n e
eval3 (App f e)    = do val1 <- eval3 f
                        val2 <- eval3 e
                        case val1 of
                          FunVal env' n body ->
                            local (const (Map.insert n val2 env')) (eval3 body)
                          _                  ->
                            throwError "App: type error"


-------------------------------------------------
type StateRep = Integer
type Eval4 a = ReaderT Env (ExceptT ExceptionRep (StateT StateRep Identity)) a

runEval4 :: Env -> StateRep -> Eval4 a -> (Either ExceptionRep a, StateRep)
runEval4 env st e = runIdentity (runStateT (runExceptT (runReaderT e env)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i)      = do tick
                        return $ IntVal i
eval4 (Var n)      = do tick
                        env <- ask
                        case Map.lookup n env of
                          Nothing -> throwError ("unbound variable: " ++ n)
                          Just v  -> return v
eval4 (Plus e1 e2) = do tick
                        e1' <- eval4 e1
                        e2' <- eval4 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) ->
                            return $ IntVal (i1 + i2)
                          _                      ->
                            throwError "Plus: type error"
eval4 (Abs n e)    = do tick
                        env <- ask
                        return $ FunVal env n e
eval4 (App f e)    = do tick
                        val1 <- eval4 f
                        val2 <- eval4 e
                        case val1 of
                          FunVal env' n body ->
                            local (const (Map.insert n val2 env')) (eval4 body)
                          _                  ->
                            throwError "App: type error"


-------------------------------------------------
type LogsRep = [String]
type Eval5 a = ReaderT Env (ExceptT ExceptionRep
                             (WriterT LogsRep
                               (StateT StateRep Identity))) a

runEval5 :: Env -> StateRep -> Eval5 a -> ((Either ExceptionRep a, LogsRep), StateRep)
runEval5 env st e =
  runIdentity (runStateT (runWriterT (runExceptT (runReaderT e env))) st)

eval5 :: Exp -> Eval5 Value
eval5 (Lit i)      = do tick
                        return $ IntVal i
eval5 (Var n)      = do tick
                        tell [n]
                        env <- ask
                        case Map.lookup n env of
                          Nothing -> throwError ("unbound variable: " ++ n)
                          Just v  -> return v
eval5 (Plus e1 e2) = do tick
                        e1' <- eval5 e1
                        e2' <- eval5 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) ->
                            return $ IntVal (i1 + i2)
                          _                      ->
                            throwError "Plus: type error"
eval5 (Abs n e)    = do tick
                        env <- ask
                        return $ FunVal env n e
eval5 (App f e)    = do tick
                        val1 <- eval5 f
                        val2 <- eval5 e
                        case val1 of
                          FunVal env' n body ->
                            local (const (Map.insert n val2 env')) (eval5 body)
                          _                  ->
                            throwError "App: type error"


-------------------------------------------------
type Eval6 a = ReaderT Env (ExceptT ExceptionRep
                             (WriterT LogsRep
                               (StateT StateRep IO))) a

runEval6 :: Env -> StateRep -> Eval6 a -> IO ((Either ExceptionRep a, LogsRep), StateRep)
runEval6 env st e =
  runStateT (runWriterT (runExceptT (runReaderT e env))) st

eval6 :: Exp -> Eval6 Value
eval6 (Lit i)      = do tick
                        liftIO $ print i
                        return $ IntVal i
eval6 (Var n)      = do tick
                        tell [n]
                        env <- ask
                        case Map.lookup n env of
                          Nothing -> throwError ("unbound variable: " ++ n)
                          Just v  -> return v
eval6 (Plus e1 e2) = do tick
                        e1' <- eval6 e1
                        e2' <- eval6 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) ->
                               return $ IntVal (i1 + i2)
                          _                      ->
                            throwError "Plus: type error"
eval6 (Abs n e)    = do tick
                        env <- ask
                        return $ FunVal env n e
eval6 (App f e)    = do tick
                        val1 <- eval6 f
                        val2 <- eval6 e
                        case val1 of
                          FunVal env' n body ->
                            local (const (Map.insert n val2 env')) (eval6 body)
                          _                  ->
                            throwError "App: type error"

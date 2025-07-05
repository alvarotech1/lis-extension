module Eval3 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  
import Debug.Trace


-- Estados
type Funcs = [(String, ([Variable], Comm))]
type Env = [(Variable,Int)]


-- Estado nulo
initState :: Env
initState = []

-- Mónada estado-error-tick
newtype StateErrorTick a = StateErrorTick { runStateErrorTick :: Env -> Maybe (a, Env, Int) }

instance Monad StateErrorTick where
    return x = StateErrorTick (\s -> Just (x, s, 0))
    m >>= f = StateErrorTick (\s -> do (v, s', t) <- runStateErrorTick m s
                                       (v', s'', t') <- runStateErrorTick (f v) s'
                                       return (v', s'', t + t'))
--    m >>= f = StateErrorTick (\s -> case runStateErrorTick m s of
--                                      Nothing -> Nothing
--                                      Just (v, s', t) -> case runStateErrorTick (f v) s' of
--                                                           Nothing -> Nothing
--                                                           Just (v', s'', t') -> Just (v', s'', t + t'))

-- Clase para representar mónadas con estado de variables.
class Monad m => MonadState m where
    lookfor :: Variable -> m Int
    update :: Variable -> Int -> m ()
    getEnv  :: m Env
    setEnv  :: Env -> m ()

instance MonadState StateErrorTick where
    lookfor var = StateErrorTick (\s -> maybe Nothing (\v -> Just (v, s, 0)) (lookfor' var s))
                  where lookfor' var []               = Nothing
                        lookfor' var ((var', val):ss) | var == var' = Just val
                                                      | otherwise   = lookfor' var ss
    update var val = StateErrorTick (\s -> Just ((), update' var val s, 0))
                     where update' var val [] = [(var, val)]
                           update' var val ((var', val'):ss) | var == var' = (var, val):ss
                                                             | otherwise   = (var', val'): update' var val ss
    getEnv = StateErrorTick (\s -> Just (s, s, 0))
    setEnv s' = StateErrorTick (\_ -> Just ((), s', 0))

-- Clase para representar mónadas que lanzan errores.
class Monad m => MonadError m where
    throw :: m a

instance MonadError StateErrorTick where
    throw = StateErrorTick (\_ -> Nothing)

-- Clase para representar mónadas que cuentan operaciones.
class Monad m => MonadTick m where
    tick :: m ()

instance MonadTick StateErrorTick where
    tick = StateErrorTick (\s -> Just ((), s, 1))

-- Para calmar al GHC
instance Functor StateErrorTick where
    fmap = liftM

instance Applicative StateErrorTick where
    pure = return
    (<*>) = ap

-- Evalua un programa en el estado nulo
eval :: Comm -> (Env, Int)
eval p =
  let (funcs, mainComm) = splitDefs p
  in case runStateErrorTick (evalComm' mainComm funcs) initState of
       Just (_, s, t) -> (s, t)
       Nothing        -> error "ERROR!"

splitDefs :: Comm -> (Funcs, Comm)
splitDefs Skip = ([], Skip)
splitDefs (Sub f params body) = ([(f, (params, body))], Skip)
splitDefs (Seq c1 c2) =
  let (fs1, c1') = splitDefs c1
      (fs2, c2') = splitDefs c2
  in (fs1 ++ fs2, Seq c1' c2')
splitDefs c = ([], c)


-- Evalua un comando en un estado dado
evalComm' :: (MonadState m, MonadError m, MonadTick m) => Comm -> Funcs -> m (Maybe Int)
evalComm' Skip _ = return Nothing
evalComm' (Let v e) _ = do
  val <- evalIntExp e
  update v val
  return Nothing
evalComm' (Seq l r) fs = do
  res <- evalComm' l fs
  case res of
    Just val -> return (Just val)  
    Nothing  -> evalComm' r fs
evalComm' (Cond b c1 c2) fs = do
  val <- evalBoolExp b
  if val then evalComm' c1 fs else evalComm' c2 fs
evalComm' (Inc v) _ = do
  val <- lookfor v
  update v (val + 1)
  tick
  return Nothing
evalComm' (Dec v) _ = do
  val <- lookfor v
  update v (val - 1)
  tick
  return Nothing
evalComm' (While b c) fs = do
  cond <- evalBoolExp b
  if cond
    then do
      rv <- evalComm' c fs
      case rv of
        Just v  -> return (Just v)  
        Nothing -> evalComm' (While b c) fs
    else return Nothing
evalComm' (Sub _ _ _) _ = return Nothing 
evalComm' (Return e) _ = do
  val <- evalIntExp e
  return (Just val)
evalComm' (Call f args) funcs =
  case lookup f funcs of
    Just (params, body) -> do
      argVals <- mapM evalIntExp args
      oldEnv <- getEnv
      let paramBindings = zip params argVals
      setEnv (paramBindings ++ oldEnv)
      _ <- evalComm' body funcs  
      setEnv oldEnv
      return Nothing
    Nothing -> throw

evalComm' (LetCall v f args) funcs =
  case lookup f funcs of
    Just (params, body) -> do
      argVals <- mapM evalIntExp args
      oldEnv <- getEnv
      let paramBindings = zip params argVals
          newEnv = paramBindings ++ oldEnv
      setEnv newEnv
      ret <- evalComm' body funcs
      setEnv oldEnv
      case ret of
        Just val -> do update v val
                       return Nothing
        Nothing   -> throw
    Nothing -> throw


-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: (MonadState m, MonadError m, MonadTick m) => IntExp -> m Int
evalIntExp (Const n)   = return n
evalIntExp (Var v)     = do val <- lookfor v
                            return val
evalIntExp (UMinus e)  = do val <- evalIntExp e
                            return (negate val)
evalIntExp (Plus l r)  = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            tick
                            return (lval + rval)
evalIntExp (Minus l r) = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            tick
                            return (lval - rval)
evalIntExp (Times l r) = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            tick
                            return (lval * rval)
evalIntExp (Div l r)   = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            if rval == 0 then throw
                            else do tick
                                    return (div lval rval)
evalIntExp (Mod l r) = do lval <- evalIntExp l
                          rval <- evalIntExp r
                          if rval == 0 then throw
                                      else do tick
                                              return (mod lval rval)



-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m, MonadTick m) => BoolExp -> m Bool
evalBoolExp BTrue     = return True
evalBoolExp BFalse    = return False
evalBoolExp (Eq l r)  = do lval <- evalIntExp l
                           rval <- evalIntExp r
                           return (lval == rval)
evalBoolExp (Lt l r)  = do lval <- evalIntExp l
                           rval <- evalIntExp r
                           return (lval < rval)
evalBoolExp (Gt l r)  = do lval <- evalIntExp l
                           rval <- evalIntExp r
                           return (lval > rval)
evalBoolExp (And l r) = do lval <- evalBoolExp l
                           rval <- evalBoolExp r
                           return (lval && rval)
evalBoolExp (Or l r)  = do lval <- evalBoolExp l
                           rval <- evalBoolExp r
                           return (lval || rval)
evalBoolExp (Not b)   = do bval <- evalBoolExp b
                           return (not bval)



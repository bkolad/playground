module MonadT where

{-# LANGUAGE DatatypeContexts  #-}


import Data.Foldable (foldl')
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Except as E
import qualified Control.Monad.State as ST
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Set as S



newtype Compose f g a = Compose (f (g a)) deriving Show

 -- FUNCTOR
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap fun (Compose x) = Compose $ fmap (\k-> fmap fun k) x
  
  
composedListMaybe :: Compose [] Maybe Integer 
composedListMaybe = Compose $ [Just 2, Just 3, Just 4]  
  
composedIOList :: Compose IO [] Integer 
composedIOList = Compose $ return [2, 3,  4]  
  
  
tryFunctor1 = fmap (+9) composedListMaybe  
  
tryFunctor2 = showTry (fmap (+88) composedIOList)  
  
showTry (Compose x) = x  
  
  
  -- APPLICATIVE   
  
 
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure
  (Compose fun) <*> (Compose x) = Compose $ lift2 (<*>) fun x
 
 
lift2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
lift2 f a b = fmap f a <*> b 
 
 
tryApplicative1 = Compose [Just (+99)] <*> composedListMaybe 
 
tryApplicative2 = showTry $ Compose (return [(+3), (+9)]) <*> composedIOList  
 
 -- Monad
 
instance (Monad f, Monad g) => Monad (Compose f g) where
  -- return = undefined
  m >>= f = undefined -- NOT POSSIBLE
  
  
--               
    
newtype  MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }



instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT x) = MaybeT $ fmap (fmap f) x
    

instance (Applicative m) => Applicative (MaybeT m) where
    pure = MaybeT . pure . pure  
    MaybeT f <*> (MaybeT x) = MaybeT $ lift2 (<*>) f x 
 
 
 
instance (Monad m) => Monad (MaybeT m) where
    return = pure
    MaybeT x >>= f = MaybeT $ do
        v <- x
        case v of
             Nothing -> return Nothing
             Just y  -> runMaybeT (f y)
 

 
newtype ListT m a = ListT { runListT :: m [a] }  



instance (Functor m) => Functor (ListT m) where
    fmap f (ListT x) = ListT $ fmap (fmap f) x
    
    
    
instance (Applicative m) => Applicative (ListT m) where
    pure  = ListT . pure . pure
    ListT fun <*> (ListT x) =  ListT $ lift2 (<*>) fun x
    
    

instance (Monad m) => Monad (ListT m) where
    return = pure
    ListT x >>= fun = ListT $ 
         do ls <- x
            ll <- traverse (\k -> runListT $ fun k) ls 
            return $ concat ll
                                           
    

-- ====

-- Control.Monad
liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f m1              = f <$> m1--do { x1 <- m1; return (f x1) }

-- MonadTrans
liftToMaybeT :: [a] -> MaybeT [] a
liftToMaybeT = MaybeT . liftM Just



fun :: [Maybe Integer]
fun = runMaybeT $
   do x <- liftToMaybeT [1,2]
      y <- MaybeT $ [Just 7, Nothing]
      return $ x+y
         
     
     
liftToListT :: (Maybe a) -> ListT Maybe a
liftToListT m = ListT $ liftM (\x->[x]) m    
     
     
     
fun2 :: Maybe [Integer]     
fun2 = runListT $
   do x <- liftToListT $ Just 2
      y <- ListT $  Just [7, 9]
      return $ x+y
      
     
     
-- TODO Maybe IO     
    
    {--
parseFromFile :: String ->  ExceptT String IO BEncode 
parseFromFile path = do content <- liftIO $ B.readFile path
                        liftEither $ P.parseOnly bencodeParser content    
                        
                        --}
    
liftToExceptT :: Either e a -> E.ExceptT e IO a
liftToExceptT = E.ExceptT . return     
     
getNum :: IO Int    
getNum = return  3
     
     
validateNum :: Int -> Either String Int
validateNum x =
     if x>10 then Left "Too big"
             else Right x
             
             
data Person = Person Int deriving Show             
             
dbOperation :: Int -> IO Person
dbOperation x = return $ Person $ x + 20
             
             
     
fun4 :: E.ExceptT String IO Int     
fun4 = do x <- liftIO getNum
          liftToExceptT $ validateNum x
          
          
--fun5 :: E.ExceptT String IO Int 
fun5 = E.runExceptT $
  do x <- fun4
     p <- liftIO $ dbOperation x
     return p
     
  
  
fun5' :: IO (Either String Person)  
fun5' = do x<- getNum 
           let num = validateNum x
           case num of
                Left l -> return $ Left l
                Right r -> do p <- dbOperation r
                              return $ Right p 
          
          
-- ====================================================





firstRepeat :: Ord a =>  [a] -> IO(Maybe a)
firstRepeat ls = ST.evalStateT (foo ls) S.empty

foo :: Ord a => [a] -> ST.StateT (S.Set a) IO (Maybe a)
foo [] = pure Nothing
foo (x:xs) = do set <- ST.get
                liftIO $ print "lala"
                if (S.member x set) 
                   then 
                        return $ Just x
                   else
                        do ST.put (S.insert x set)
                           (foo xs)
          
   
   
   
class  Show1 m where
  show1 :: Show a => m a -> String
  
  
  
instance Show1 [] where
  show1 = show  
  

  
instance (Show1 m, Show a) => Show (MaybeT m a) where
  show (MaybeT m) = show1 m 

    

  

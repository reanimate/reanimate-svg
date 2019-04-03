module Graphics.SvgTree.Memo
  ( memo
  , preRender
  ) where

import           Control.Lens
import           Data.IORef
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Typeable
import           Graphics.SvgTree.Printer
import           Graphics.SvgTree.Types   (Tree, preRendered)
import           System.IO.Unsafe

{-# NOINLINE intCache #-}
intCache :: IORef (Map Int Tree)
intCache = unsafePerformIO (newIORef Map.empty)

{-# NOINLINE doubleCache #-}
doubleCache :: IORef (Map Double Tree)
doubleCache = unsafePerformIO (newIORef Map.empty)

{-# NOINLINE anyCache #-}
anyCache :: IORef (Map (TypeRep,String) Tree)
anyCache = unsafePerformIO (newIORef Map.empty)

memo :: (Typeable a, Show a) => (a -> Tree) -> (a -> Tree)
memo fn =
  case listToMaybe (catMaybes caches) of
    Just ret -> ret
    Nothing  -> memoAny fn
  where
    caches = [try intCache, try doubleCache]
    try cache = cast . memoUsing cache =<< cast fn

memoUsing :: Ord a => IORef (Map a Tree) -> (a -> Tree) -> (a -> Tree)
memoUsing cache fn a = unsafePerformIO $
  atomicModifyIORef cache $ \m ->
    let newVal = preRender $ fn a
        notFound =
          (Map.insert a newVal m, newVal) in
    case Map.lookup a m of
      Nothing -> notFound
      Just t  -> (m, t)

memoAny :: (Typeable a, Show a) => (a -> Tree) -> (a -> Tree)
memoAny fn a = unsafePerformIO $
  atomicModifyIORef anyCache $ \m ->
    let newVal = preRender $ fn a
        notFound =
          (Map.insert (typeOf a, show a) newVal m, newVal) in
    case Map.lookup (typeOf a, show a) m of
      Nothing -> notFound
      Just t  -> (m, t)

preRender :: Tree -> Tree
preRender t = t & preRendered .~ Just (ppTree t)

-- {-# INLINE memo #-}
-- memo :: (a -> b) -> (a -> b)
-- memo fn = unsafePerformIO $ do
--   ref <- newIORef Map.empty
--   return $ \a -> unsafePerformIO $ do
--     stableA <- makeStableName a
--     let key = hashStableName stableA
--     atomicModifyIORef ref $ \m ->
--       case Map.lookup key m of
--         -- Just (s,b) | s == stableA ->
--         --   (m, b)
--         _Nothing -> let !b = fn a in
--           (Map.insert key (stableA, b) m, b)
-- memo fn = unsafePerformIO $ do
--   ht <- HT.new :: IO (HT.BasicHashTable (StableName Any) Any)
--   return $ \a -> unsafePerformIO $ do
--     stableA <- makeStableName $ unsafeCoerce a
--     mbB <- HT.lookup ht stableA
--     case mbB of
--       Just b -> return (unsafeCoerce b)
--       Nothing -> do
--         let !b = fn a
--         HT.insert ht stableA (unsafeCoerce b)
--         return b

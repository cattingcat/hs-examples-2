{-# LANGUAGE TemplateHaskell #-}

module Extensions.DeriveLiftConsumer () where

import Extensions.DeriveLift
import Language.Haskell.TH.Syntax
import Control.Monad.IO.Class (MonadIO (..))

foo :: Foo String
foo = $(lift $ Foo "foo")

fooExp :: Lift a => Foo a -> Q Exp
fooExp f = [| f |]

test :: Q Exp -> IO ()
test q = runQ $ qmonad q where
  qmonad :: Quasi m => m Exp -> m ()
  qmonad qq = do
    e <- qq
    liftIO $ print e
    pure ()

--  test (fooExp foo)


-- | Levity polymorphic example:
--    Type = TYPE LiftedRep
-- 
-- So t - is wider than just Type,
-- t is suitable for unlifted types
--class Lift (t :: TYPE r) where
--  lift :: t -> Q Exp
--  default lift :: (r ~ 'LiftedRep) => t -> Q Exp - default behavior for all Lifted types (just Type)
--  lift = unTypeQ . liftTyped
--
--  liftTyped :: t -> Q (TExp t)
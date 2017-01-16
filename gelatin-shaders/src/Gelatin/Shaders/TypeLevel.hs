{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Gelatin.Shaders.TypeLevel
  ( -- * Type level combinators
    (:&)(..)
    -- * Generating symbol values on type lists
  , GetLits(..)
    -- * Generating function class
  , HasGenFunc(..)
    -- * Mapping types
  , TypeMap
  ) where

import           Data.Proxy   (Proxy (..))
import           GHC.TypeLits (KnownNat, KnownSymbol, natVal, symbolVal)
--------------------------------------------------------------------------------
-- Type level combinators
--------------------------------------------------------------------------------
-- | A heterogenious list.
data a :& b = a :& b
infixr 8 :&

class GetLits a t where
  getSymbols :: Proxy a -> t

instance GetLits '[] [t] where
  getSymbols _ = []

instance (GetLits a t, GetLits as [t]) => GetLits (a ': as) [t] where
  getSymbols _ = getSymbols (Proxy :: Proxy a) : getSymbols (Proxy :: Proxy as)

instance KnownSymbol a => GetLits a String where
  getSymbols = symbolVal

instance KnownNat a => GetLits a Integer where
  getSymbols = natVal
--------------------------------------------------------------------------------
-- Generating a function from a type
--------------------------------------------------------------------------------
class HasGenFunc a where
  type GenFunc a  :: *
  genFunction :: Proxy a -> GenFunc a

instance (HasGenFunc a, HasGenFunc b) => HasGenFunc (a :& b) where
  type GenFunc (a :& b) = GenFunc a :& GenFunc b
  genFunction _ =
    let a = (Proxy :: Proxy a)
        b = (Proxy :: Proxy b)
    in genFunction a :& genFunction b

instance HasGenFunc '[] where
  type GenFunc '[] = ()
  genFunction _ = ()

instance (HasGenFunc a, HasGenFunc as) => HasGenFunc (a ': as) where
  type GenFunc (a ': as) = GenFunc a :& GenFunc as
  genFunction _ =
    let a  = (Proxy :: Proxy a)
        as = (Proxy :: Proxy as)
    in genFunction a :& genFunction as

type family TypeMap (a :: * -> *) (xs :: [*]) :: [*]
type instance TypeMap t '[] = '[]
type instance TypeMap t (x ': xs) = t x ': TypeMap t xs

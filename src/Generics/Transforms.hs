{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Generics.Transforms where

import Data.Functor.Kan.Rift
import Data.Functor.Yoneda
import GHC.Generics
import Control.Applicative

genericTransform ::
  forall a c f. (Applicative f, Generic a, GTransform c (Rep a)) => Fun c f -> a -> f a
genericTransform (Fun f) x = lowerYoneda (pure to <*>^ gtransform f' (from x))
  where
  f' :: Fun c (Rift (Yoneda f) (Yoneda f))
  f' = Fun (liftRiftYoneda . f)
{-# INLINE genericTransform #-}

newtype Fun c f = Fun (forall a. c a => a -> f a)

class GTransform c g where
  gtransform :: Applicative f => Fun c f -> g p -> f (g p)

instance GTransform c f => GTransform c (M1 i d f) where
  gtransform f (M1 x) = M1 <$> gtransform f x
  {-# INLINE gtransform #-}

instance (GTransform c f, GTransform c g) => GTransform c (f :+: g) where
  gtransform f (L1 x) = L1 <$> gtransform f x
  gtransform f (R1 x) = R1 <$> gtransform f x
  {-# INLINE gtransform #-}

instance (GTransform c f, GTransform c g) => GTransform c (f :*: g) where
  gtransform f (x :*: y) = (:*:) <$> gtransform f x <*> gtransform f y
  {-# INLINE gtransform #-}

instance c a => GTransform c (K1 i a) where
  gtransform (Fun f) (K1 x) = K1 <$> f x
  {-# INLINE gtransform #-}

instance GTransform c U1 where
  gtransform _ U1 = pure U1
  {-# INLINE gtransform #-}

instance GTransform c V1 where
  gtransform _ v = v `seq` error "gtransform: v1"
  {-# INLINE gtransform #-}

------------------------------------------------------------------------
-- Make generic traversal efficient
------------------------------------------------------------------------

liftRiftYoneda :: Applicative f => f a -> Rift (Yoneda f) (Yoneda f) a
liftRiftYoneda fa = Rift (`yap` fa)
{-# INLINE liftRiftYoneda #-}

yap :: Applicative f => Yoneda f (a -> b) -> f a -> Yoneda f b
yap (Yoneda k) fa = Yoneda (\ab_r -> k (ab_r .) <*> fa )
{-# INLINE yap #-}

-- | Run function for 'Rift'
(<*>^) :: f (a -> b) -> Rift f g a -> g b
x <*>^ Rift y = y x
infixl 4 <*>^
{-# INLINE (<*>^) #-}

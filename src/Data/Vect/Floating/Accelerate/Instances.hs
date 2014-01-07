{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
This module exports vect-floating instances to make Vec2, Normal2, Vec3, Normal3, Vec4, Normal4, Quaternion, and UnitQuaternion
compatible with accelerate.

The instances are defined:

Vec2 Accelerate Instances:

  * @instance Elt a => Elt (Vec2 a)@

  * @instance IsTuple (Vec2 a)@

  * @instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Vec2 a)@

  * @instance (Elt a) => Unlift Exp (Vec2 (Exp a))@

Normal2 Accelerate Instances

  * @instance (Elt a, Floating a) => Elt (Normal2 a)

  * @instance Floating a => IsTuple (Normal2 a)

  * @instance (Lift Exp a, Elt (Plain a), Floating a, Floating (Plain a)) => Lift Exp (Normal2 a)@

  * @instance (Elt a, Floating a, IsFloating a) => Unlift Exp (Normal2 (Exp a))@

Vec3 Accelerate Instances

 * @instance Elt a => Elt (Vec3 a)@

 * @instance IsTuple (Vec3 a)@

 * @instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Vec3 a)@

 * @instance Elt a => Unlift Exp (Vec3 (Exp a))@

Normal3 Accelerate Instances

 * @instance (Elt a, Floating a) => Elt (Normal3 a)@

 * @instance Floating a => IsTuple (Normal3 a)@

 * @instance (Lift Exp a, Elt (Plain a), Floating a, Floating (Plain a)) => Lift Exp (Normal3 a)@

 * @instance (Elt a, Floating a, IsFloating a) => Unlift Exp (Normal3 (Exp a))@

Vec4 Accelerate Instances

 * @instance Elt a => Elt (Vec4 a)@

 * @instance IsTuple (Vec4 a)@

 * @instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Vec4 a)@

 * @instance Elt a => Unlift Exp (Vec4 (Exp a))@

Normal4 Accelerate Instances

 * @instance (Elt a, Floating a) => Elt (Normal4 a)@

 * @instance Floating a => IsTuple (Normal4 a)@

 * @instance (Lift Exp a, Elt (Plain a), Floating a, Floating (Plain a)) => Lift Exp (Normal4 a)@

 * @instance (Elt a, Floating a, IsFloating a) => Unlift Exp (Normal4 (Exp a))@

Quaternion Accelerate Instances
 
 * @instance Elt a => Elt (Quaternion a)@

 * @instance IsTuple (Quaternion a)@

 * @instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Quaternion a)@

 * @instance Elt a => Unlift Exp (Quaternion (Exp a))@

UnitQuaternion Accelerate Instances

 * @instance (Elt a, Floating a) => Elt (UnitQuaternion a)@

 * @instance Floating a => IsTuple (UnitQuaternion a)@

 * @instance (Lift Exp a, Elt (Plain a), Floating a, Floating (Plain a)) => Lift Exp (UnitQuaternion a)@

 * @instance (Elt a, IsFloating a) => Unlift Exp (UnitQuaternion (Exp a))@
-}

module Data.Vect.Floating.Accelerate.Instances () where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar
import Data.Vect.Floating
import Data.Vect.Floating.Util.Quaternion

{- Vec2 Accelerate Instances -}

type instance EltRepr (Vec2 a) = EltRepr (a,a)
type instance EltRepr' (Vec2 a) = EltRepr' (a,a)

instance Elt a => Elt (Vec2 a) where
  eltType (_ :: Vec2 a) = eltType (undefined :: (a,a))
  toElt p = let (x,y) = toElt p in Vec2 x y
  fromElt (Vec2 x y) = fromElt (x,y)
  
  eltType' (_ :: Vec2 a) = eltType (undefined :: (a,a))
  toElt' p = let (x,y) = toElt p in Vec2 x y
  fromElt' (Vec2 x y) = fromElt (x,y)
  
instance IsTuple (Vec2 a) where
  type TupleRepr (Vec2 a) = TupleRepr (a,a)
  fromTuple (Vec2 x y) = fromTuple (x,y)
  toTuple t = let (x,y) = toTuple t in Vec2 x y
  
instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Vec2 a) where
  type Plain (Vec2 a) = Vec2 (Plain a)
  lift (Vec2 x y) = Exp . Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y
  
instance (Elt a) => Unlift Exp (Vec2 (Exp a)) where
  unlift t = let x = Exp $ SuccTupIdx ZeroTupIdx `Prj` t
                 y = Exp $ ZeroTupIdx `Prj` t
             in Vec2 x y
                
{- Normal2 Accelerate Instances -}

type instance EltRepr (Normal2 a) = EltRepr (a,a)
type instance EltRepr' (Normal2 a) = EltRepr' (a,a)

instance (Elt a, Floating a) => Elt (Normal2 a) where
  eltType (_ :: Normal2 a) = eltType (undefined :: (a,a))
  toElt p = let (x,y) = toElt p in toNormalUnsafe (Vec2 x y)
  fromElt n = let (Vec2 x y) = fromNormal n in fromElt (x,y)
  
  eltType' (_ :: Normal2 a) = eltType (undefined :: (a,a))
  toElt' p = let (x,y) = toElt p in toNormalUnsafe (Vec2 x y)
  fromElt' n = let (Vec2 x y) = fromNormal n in fromElt (x,y)
  
instance Floating a => IsTuple (Normal2 a) where
  type TupleRepr (Normal2 a) = TupleRepr (a,a)
  fromTuple n = let Vec2 x y = fromNormal n in fromTuple (x,y)
  toTuple t = let (x,y) = toTuple t in toNormalUnsafe (Vec2 x y)
  
instance (Lift Exp a, Elt (Plain a), Floating a, Floating (Plain a)) => Lift Exp (Normal2 a) where
  type Plain (Normal2 a) = Normal2 (Plain a)
  lift n = let (Vec2 x y) = fromNormal n in Exp . Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y
  
instance (Elt a, Floating a, IsFloating a) => Unlift Exp (Normal2 (Exp a)) where
  unlift t = let x = Exp $ SuccTupIdx ZeroTupIdx `Prj` t
                 y = Exp $ ZeroTupIdx `Prj` t
             in toNormalUnsafe (Vec2 x y)

{- Vec3 Accelerate Instances -}

type instance EltRepr (Vec3 a) = EltRepr (a,a,a)
type instance EltRepr' (Vec3 a) = EltRepr' (a,a,a)

instance Elt a => Elt (Vec3 a) where
  eltType (_ :: Vec3 a) = eltType (undefined :: (a,a,a))
  toElt p = let (x,y,z) = toElt p in Vec3 x y z
  fromElt (Vec3 x y z) = fromElt (x,y,z)
  
  eltType' (_ :: Vec3 a) = eltType (undefined :: (a,a,a))
  toElt' p = let (x,y,z) = toElt p in Vec3 x y z
  fromElt' (Vec3 x y z) = fromElt (x,y,z)
  
instance IsTuple (Vec3 a) where
  type TupleRepr (Vec3 a) = TupleRepr (a,a,a)
  fromTuple (Vec3 x y z) = fromTuple (x,y,z)
  toTuple t = let (x,y,z) = toTuple t in Vec3 x y z
  
instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Vec3 a) where
  type Plain (Vec3 a) = Vec3 (Plain a)
  lift (Vec3 x y z) = Exp . Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z
  
instance Elt a => Unlift Exp (Vec3 (Exp a)) where
  unlift t = let x = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t
                 y = Exp $ SuccTupIdx ZeroTupIdx `Prj` t
                 z = Exp $ ZeroTupIdx `Prj` t
             in Vec3 x y z
                
{- Normal3 Accelerate Instances -}

type instance EltRepr (Normal3 a) = EltRepr (a,a,a)
type instance EltRepr' (Normal3 a) = EltRepr' (a,a,a)

instance (Elt a, Floating a) => Elt (Normal3 a) where
  eltType (_ :: Normal3 a) = eltType (undefined :: (a,a,a))
  toElt p = let (x,y,z) = toElt p in toNormalUnsafe (Vec3 x y z)
  fromElt n = let (Vec3 x y z) = fromNormal n in fromElt (x,y,z)
  
  eltType' (_ :: Normal3 a) = eltType (undefined :: (a,a,a))
  toElt' p = let (x,y,z) = toElt p in toNormalUnsafe (Vec3 x y z)
  fromElt' n = let (Vec3 x y z) = fromNormal n in fromElt (x,y,z)
  
instance Floating a => IsTuple (Normal3 a) where
  type TupleRepr (Normal3 a) = TupleRepr (a,a,a)
  fromTuple n = let Vec3 x y z = fromNormal n in fromTuple (x,y,z)
  toTuple t = let (x,y,z) = toTuple t in toNormalUnsafe (Vec3 x y z)
  
instance (Lift Exp a, Elt (Plain a), Floating a, Floating (Plain a)) => Lift Exp (Normal3 a) where
  type Plain (Normal3 a) = Normal3 (Plain a)
  lift n = let (Vec3 x y z) = fromNormal n in Exp . Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z
  
instance (Elt a, Floating a, IsFloating a) => Unlift Exp (Normal3 (Exp a)) where
  unlift t = let x = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t
                 y = Exp $ SuccTupIdx ZeroTupIdx `Prj` t
                 z = Exp $ ZeroTupIdx `Prj` t
             in toNormalUnsafe (Vec3 x y z)
                
{- Vec4 Accelerate Instances -}
                
type instance EltRepr (Vec4 a) = EltRepr (a,a,a,a)
type instance EltRepr' (Vec4 a) = EltRepr' (a,a,a,a)

instance Elt a => Elt (Vec4 a) where
  eltType (_ :: Vec4 a) = eltType (undefined :: (a,a,a,a))
  toElt p = let (x,y,z,w) = toElt p in Vec4 x y z w
  fromElt (Vec4 x y z w) = fromElt (x,y,z,w)
  
  eltType' (_ :: Vec4 a) = eltType (undefined :: (a,a,a,a))
  toElt' p = let (x,y,z,w) = toElt p in Vec4 x y z w
  fromElt' (Vec4 x y z w) = fromElt (x,y,z,w)
  
instance IsTuple (Vec4 a) where
  type TupleRepr (Vec4 a) = TupleRepr (a,a,a,a)
  fromTuple (Vec4 x y z w) = fromTuple (x,y,z,w)
  toTuple t = let (x,y,z,w) = toTuple t in Vec4 x y z w
  
instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Vec4 a) where
  type Plain (Vec4 a) = Vec4 (Plain a)
  lift (Vec4 x y z w) = Exp . Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z `SnocTup` lift w
  
instance Elt a => Unlift Exp (Vec4 (Exp a)) where
  unlift t = let x = Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t
                 y = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t
                 z = Exp $ SuccTupIdx ZeroTupIdx `Prj` t
                 w = Exp $ ZeroTupIdx `Prj` t
             in Vec4 x y z w

{- Normal4 Accelerate Instances -}

type instance EltRepr (Normal4 a) = EltRepr (a,a,a,a)
type instance EltRepr' (Normal4 a) = EltRepr' (a,a,a,a)

instance (Elt a, Floating a) => Elt (Normal4 a) where
  eltType (_ :: Normal4 a) = eltType (undefined :: (a,a,a,a))
  toElt p = let (x,y,z,w) = toElt p in toNormalUnsafe (Vec4 x y z w)
  fromElt n = let (Vec4 x y z w) = fromNormal n in fromElt (x,y,z,w)
  
  eltType' (_ :: Normal4 a) = eltType (undefined :: (a,a,a,a))
  toElt' p = let (x,y,z,w) = toElt p in toNormalUnsafe (Vec4 x y z w)
  fromElt' n = let (Vec4 x y z w) = fromNormal n in fromElt (x,y,z,w)
  
instance Floating a => IsTuple (Normal4 a) where
  type TupleRepr (Normal4 a) = TupleRepr (a,a,a,a)
  fromTuple n = let Vec4 x y z w = fromNormal n in fromTuple (x,y,z,w)
  toTuple t = let (x,y,z,w) = toTuple t in toNormalUnsafe (Vec4 x y z w)
  
instance (Lift Exp a, Elt (Plain a), Floating a, Floating (Plain a)) => Lift Exp (Normal4 a) where
  type Plain (Normal4 a) = Normal4 (Plain a)
  lift n = let (Vec4 x y z w) = fromNormal n in Exp . Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z `SnocTup` lift w
  
instance (Elt a, Floating a, IsFloating a) => Unlift Exp (Normal4 (Exp a)) where
  unlift t = let x = Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t
                 y = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t
                 z = Exp $ SuccTupIdx ZeroTupIdx `Prj` t
                 w = Exp $ ZeroTupIdx `Prj` t
             in toNormalUnsafe (Vec4 x y z w)

{- Quaternion Accelerate Instances -}
                
type instance EltRepr (Quaternion a) = EltRepr (a,a,a,a)
type instance EltRepr' (Quaternion a) = EltRepr' (a,a,a,a)

instance Elt a => Elt (Quaternion a) where
  eltType (_ :: Quaternion a) = eltType (undefined :: (a,a,a,a))
  toElt p = let (x,y,z,w) = toElt p in Q (Vec4 x y z w)
  fromElt (Q (Vec4 x y z w)) = fromElt (x,y,z,w)
  
  eltType' (_ :: Quaternion a) = eltType (undefined :: (a,a,a,a))
  toElt' p = let (x,y,z,w) = toElt p in Q (Vec4 x y z w)
  fromElt' (Q (Vec4 x y z w)) = fromElt (x,y,z,w)
  
instance IsTuple (Quaternion a) where
  type TupleRepr (Quaternion a) = TupleRepr (a,a,a,a)
  fromTuple (Q (Vec4 x y z w)) = fromTuple (x,y,z,w)
  toTuple t = let (x,y,z,w) = toTuple t in Q (Vec4 x y z w)
  
instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Quaternion a) where
  type Plain (Quaternion a) = Quaternion (Plain a)
  lift (Q (Vec4 x y z w)) = Exp . Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z `SnocTup` lift w
  
instance Elt a => Unlift Exp (Quaternion (Exp a)) where
  unlift t = let x = Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t
                 y = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t
                 z = Exp $ SuccTupIdx ZeroTupIdx `Prj` t
                 w = Exp $ ZeroTupIdx `Prj` t
             in Q $ Vec4 x y z w
                
{- Unit Quaternion Accelerate Instances -}
                
type instance EltRepr (UnitQuaternion a) = EltRepr (a,a,a,a)
type instance EltRepr' (UnitQuaternion a) = EltRepr' (a,a,a,a)

instance (Elt a, Floating a) => Elt (UnitQuaternion a) where
  eltType (_ :: UnitQuaternion a) = eltType (undefined :: (a,a,a,a))
  toElt p = let (x,y,z,w) = toElt p in toNormalUnsafe $ Q (Vec4 x y z w)
  fromElt u = let (Q (Vec4 x y z w)) = fromNormal u in fromElt (x,y,z,w)
  
  eltType' (_ :: UnitQuaternion a) = eltType (undefined :: (a,a,a,a))
  toElt' p = let (x,y,z,w) = toElt p in toNormalUnsafe $ Q (Vec4 x y z w)
  fromElt' u = let (Q (Vec4 x y z w)) = fromNormal u in fromElt (x,y,z,w)
  
instance Floating a => IsTuple (UnitQuaternion a) where
  type TupleRepr (UnitQuaternion a) = TupleRepr (a,a,a,a)
  fromTuple u = let (Q (Vec4 x y z w)) = fromNormal u in fromTuple (x,y,z,w)
  toTuple t = let (x,y,z,w) = toTuple t in toNormalUnsafe $ Q (Vec4 x y z w)
  
instance (Lift Exp a, Elt (Plain a), Floating a, Floating (Plain a)) => Lift Exp (UnitQuaternion a) where
  type Plain (UnitQuaternion a) = UnitQuaternion (Plain a)
  lift u = let (Q (Vec4 x y z w)) = fromNormal u in Exp . Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z `SnocTup` lift w
  
instance (Elt a, IsFloating a) => Unlift Exp (UnitQuaternion (Exp a)) where
  unlift t = let x = Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t
                 y = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t
                 z = Exp $ SuccTupIdx ZeroTupIdx `Prj` t
                 w = Exp $ ZeroTupIdx `Prj` t
             in toNormalUnsafe . Q $ Vec4 x y z w
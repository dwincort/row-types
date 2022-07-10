{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Row.Aeson
--
-- This module adds orphan Aeson instances for 'Rec' and 'Var'.
--
-----------------------------------------------------------------------------

module Data.Row.Aeson () where

import           Data.Aeson
import           Data.Aeson.Encoding (pairStr)
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Aeson.Types    (typeMismatch)
import           Data.List           (intercalate)
import           Data.String         (IsString (fromString))

import           Data.Row
import qualified Data.Row.Records  as Rec
import qualified Data.Row.Variants as Var

instance Forall r ToJSON => ToJSON (Rec r) where
  toJSON = Object . KeyMap.fromList . Rec.eraseWithLabels @ToJSON toJSON

  toEncoding =
    pairs . foldMap (uncurry pairStr) . Rec.eraseWithLabels @ToJSON toEncoding

instance (AllUniqueLabels r, Forall r FromJSON) => FromJSON (Rec r) where
  parseJSON (Object o) = do
    r <- Rec.fromLabelsA @FromJSON $ \ l -> do x <- o .: show' l
                                               x `seq` pure x
    r `seq` pure r

  parseJSON v = typeMismatch msg v
    where msg = "REC: {" ++ intercalate "," (labels @r @FromJSON) ++ "}"

instance Forall r ToJSON => ToJSON (Var r) where
  toJSON v = object [foo l]
    where (l, foo) = Var.eraseWithLabels @ToJSON (\v l -> l .= v) v

instance (AllUniqueLabels r, Forall r FromJSON) => FromJSON (Var r) where
  parseJSON (Object o) = Var.fromLabels @FromJSON $ \ l -> o .: show' l
  parseJSON v = typeMismatch msg v
    where msg = "VAR: {" ++ intercalate "," (labels @r @FromJSON) ++ "}"

show' :: (IsString s, Show a) => a -> s
show' = fromString . show

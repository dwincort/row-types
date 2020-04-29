{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module OverridingTypeClassInstances where

import           Data.Aeson       (ToJSON(..), encode)
import           Data.Coerce
import           Data.Row
import           Data.Row.Aeson   ()
import qualified Data.Row.Records as Rec
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           GHC.Generics     (Generic)


newtype CharArray = CharArray { unCharArray :: String }
instance ToJSON CharArray where
  toJSON = toJSON . map (:[]) . unCharArray

newtype Uptext = Uptext { unUptext :: Text }
instance ToJSON Uptext where
  toJSON = toJSON . Text.toUpper . unUptext


data MyRec = MyRec
  { foo :: Int
  , bar :: String
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)
      via Override MyRec (
           "bar" .== CharArray
        .+ "baz" .== Uptext)


v = MyRec 3 "french" "hens"

newtype Override a (mods :: Row *) = Override {unOverride :: a}

-- | A version of 'Override' that accepts first the value and then the mods type.
override :: a -> (forall mods. Override a mods)
override = Override

x = override v @Empty
y = override v @("bar" .== CharArray .+ "baz" .== Uptext)

main = putStrLn $ show $ encode v

instance
  ( ρ ≈ Rec.NativeRow t
  , ρ' ≈ mods .// ρ
  , BiForall ρ ρ' Coercible
  , Rec.FromNative t
  , Forall ρ' ToJSON
  ) => ToJSON (Override t mods) where
  toJSON = toJSON . Rec.coerceRec @ρ @ρ' . Rec.fromNative . unOverride

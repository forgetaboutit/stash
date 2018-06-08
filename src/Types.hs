{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Types where

import           Control.Concurrent.STM as STM
import           Control.Lens
import           Data.Decimal
import           Data.Generics.Fixplate
import           Data.Map               as M
import           Data.Serialize         hiding (encode)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Vector            as V
import           GHC.Generics           (Generic)
import           Web.Hashids

-- | Higher-kinded data
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

idCtx :: HashidsContext
idCtx = hashidsSimple "foobar"

-- | Type for position IDs to refer to positions indirectly
newtype PositionItemId = PositionItemId
  { unPositionItemId :: Int
  } deriving (Eq, Ord, Generic)

instance Serialize PositionItemId

showPositionItemId :: PositionItemId -> T.Text
showPositionItemId = TE.decodeUtf8 . encode idCtx . unPositionItemId

instance Show PositionItemId where
  show = T.unpack . showPositionItemId

newtype PositionGroupId = PositionGroupId
  { unPositionGroupId :: Int
  } deriving (Eq, Ord, Generic)

instance Serialize PositionGroupId

showPositionGroupId :: PositionGroupId -> T.Text
showPositionGroupId = TE.decodeUtf8 . encode idCtx . unPositionGroupId

instance Show PositionGroupId where
  show = T.unpack . showPositionGroupId

newtype SerializableDecimal = SerializableDecimal
  { unSerializableDecimal :: Decimal
  } deriving (Show, Generic)

$(makeWrapped ''SerializableDecimal)

instance Serialize SerializableDecimal where
  put m =
    let decimalRaw = unSerializableDecimal m
     in do put $ decimalPlaces decimalRaw
           put $ decimalMantissa decimalRaw
  get = do
    SerializableDecimal <$> (Decimal <$> get <*> get)

-- | Iso for making a Decimal serializable
serializableDecimalIso :: Iso' SerializableDecimal Decimal
serializableDecimalIso = iso unSerializableDecimal SerializableDecimal

-- | Type for money
newtype Money = Money
  { unMoney :: Decimal
  } deriving (Show, Eq, Num, Generic)

$(makeWrapped ''Money)

instance Serialize Money where
  put = put . view (from serializableDecimalIso) . unMoney
  get = fmap (Money . view serializableDecimalIso) get

data RecurrencePattern
  = MonthlyRecurrence
  | YearlyRecurrence
  deriving (Show, Eq)

$(makePrisms ''RecurrencePattern)

data PositionRecurrence
  = OneTimePosition
  | RepeatingPosition RecurrencePattern
  deriving (Show, Eq)

$(makePrisms ''PositionRecurrence)

-- | Arguments for creating a CreatePositionItem command
data CreatePositionItemArgs = CreatePositionItemArgs
  { _title      :: Maybe T.Text
  , _amount     :: Money
  , _repetition :: PositionRecurrence
  } deriving (Show, Eq)

$(makeFieldsNoPrefix ''CreatePositionItemArgs)

-- | Arguments of a created CreatePositionItem command
data CreatePositionItemCmdArgs = CreatePositionItemCmdArgs
  { _id         :: PositionItemId
  , _title      :: Maybe T.Text
  , _amount     :: Money
  , _repetition :: PositionRecurrence
  } deriving (Show, Eq)

$(makeFieldsNoPrefix ''CreatePositionItemCmdArgs)

-- | PositionCommand validation errors
data PositionCommandError =
  Empty
  deriving (Show, Eq)

data PositionProps = PositionProps
  { _positionPropsCost :: Money
  , _positionPropsName :: Maybe T.Text
  } deriving (Show, Eq)

$(makeClassy ''PositionProps)

data PositionGroupProps = PositionGroupProps
  { _id :: !PositionGroupId
  } deriving (Show, Generic)

data PositionItemProps = PositionItemProps
  { _id :: !PositionItemId
  } deriving (Show, Generic)

-- | Tree for position items and position groups
data PositionF f
  = PositionGroup {-# UNPACK #-}!PositionGroupProps
                  !(V.Vector f)
  | PositionItem {-# UNPACK #-}!PositionItemProps
  deriving (Functor, Foldable)

$(makePrisms ''PositionF)

instance Show (PositionF f) where
  show (PositionGroup _ _) = "Group"
  show (PositionItem _)    = "Item"

instance ShowF PositionF where
  showsPrecF _ pos = \_ -> show pos

type Position = Mu PositionF

data UserFields' f = UserFields
  { _firstName    :: !(HKD f Text)
  , _lastName     :: !(HKD f Text)
  , _emailAddress :: !(HKD f Text)
  } deriving (Generic)

me :: UserFields
me =
  UserFields
    { _firstName = "Sammy"
    , _lastName = "Schuhmacher"
    , _emailAddress = "sammy.schuhmacher@gmail.com"
    }

type UserFields = UserFields' Identity

type MaybeUserFields = UserFields' Maybe

data PositionCommand =
  CreatePositionItemCommand CreatePositionItemCmdArgs
  deriving (Show, Eq)

$(makePrisms ''PositionCommand)

data UserCommand
  = CreateUserCommand
  | ModifyUserCommand
  | DeleteUserCommand
  deriving (Show)

$(makePrisms ''UserCommand)

data Command
  = PositionCommand PositionCommand
  | UserCommand UserCommand
  deriving (Show)

$(makePrisms ''Command)

data UserKey =
  UserKey

data User =
  User

data AppState = AppState
  { _users :: STM.TVar (M.Map UserKey (STM.TVar User))
  }

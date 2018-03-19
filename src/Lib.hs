{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Lib where

import           Control.Lens
import           Data.Decimal
import qualified Data.Text    as T
import qualified Data.Vector  as V

-- | Type
newtype Money = Money
  { unMoney :: Decimal
  } deriving (Show, Eq, Num)

$(makeWrapped ''Money)

newtype Tag = Tag
  { unTag :: T.Text
  } deriving (Show, Eq)

$(makeWrapped ''Tag)

data RepetitionPattern
  = MonthlyRepetition
  | YearlyRepetition
  deriving (Show, Eq)

$(makePrisms ''RepetitionPattern)

data ExpenseRepetition
  = OneTimeExpense
  | RepeatingExpense RepetitionPattern
  deriving (Show, Eq)

$(makePrisms ''ExpenseRepetition)

data Expense = Expense
  { _expenseCost       :: Money
  , _expenseName       :: Maybe T.Text
  , _expenseTags       :: V.Vector Tag
  , _expenseRepetition :: ExpenseRepetition
  } deriving (Show, Eq)

$(makeClassy ''Expense)

money :: Decimal -> Money
money = Money

noName :: Maybe T.Text
noName = Nothing

mkName :: T.Text -> Maybe T.Text
mkName = Just

tag :: T.Text -> Tag
tag = Tag

oneTimeExpense :: Money -> Maybe T.Text -> V.Vector Tag -> Expense
oneTimeExpense cost name tags =
  Expense
    { _expenseCost = cost
    , _expenseName = name
    , _expenseTags = tags
    , _expenseRepetition = OneTimeExpense
    }

monthlyExpense :: Money -> Maybe T.Text -> V.Vector Tag -> Expense
monthlyExpense cost name tags =
  Expense
    { _expenseCost = cost
    , _expenseName = name
    , _expenseTags = tags
    , _expenseRepetition = RepeatingExpense MonthlyRepetition
    }

allExpenses :: V.Vector Expense
allExpenses =
  V.fromList
    [ monthlyExpense (money $ -2274.62) (mkName "Gehalt") V.empty
    , monthlyExpense (money 460) (mkName "Miete") V.empty
    , monthlyExpense (money 13.99) (mkName "Netflix") V.empty
    , monthlyExpense (money 49.86) (mkName "Hetzner") V.empty
    , monthlyExpense (money 700) (mkName "FIL Fondsbank") V.empty
    , monthlyExpense (money 49.99) (mkName "Vodafone") V.empty
    , monthlyExpense (money 93.37) (mkName "SWISS LIFE AG") V.empty
    ]

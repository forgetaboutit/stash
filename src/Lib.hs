{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Lib where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Reader
import           Data.Bifunctor         (first)
import           Data.Decimal
import           Data.Generics.Fixplate
import           Data.List              (intercalate)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           System.Random
import           Types

{-
Design ideas:
- Servant-bases REST API
- Basic structure: event sourcing based on commands/events
- Serialize commands (and optionally snapshots of the application state)
  using safecopy
- Tree-based structure for positions
- Identifiers: random ints for internal IDs
  User-visible IDs: hash-ids?
  Serialized identifiers?
- Position = PositionGroup | PositionItem
- Intervals for repeating position items
- Possibly Git as backend?
- Validation should be a two-step process:
  - First, validate the command.  The command itself needs to be valid in order
    to get to processing
  - Second, validate the command processing (check, if it can be applied or if
    application fails for some reason)
-}
-- | Application state
data Env = Env
  { _rng                  :: !(TVar (StdGen))
  , _usedPositionItemIds  :: !(TVar (Set PositionItemId))
  , _usedPositionGroupIds :: !(TVar (Set PositionGroupId))
  }

$(makeFieldsNoPrefix ''Env)

-- | Creates a new environment.  Uses a random seed internally.
mkEnv :: IO Env
mkEnv = do
  seed <- randomIO
  mkEnv' seed

-- | Creates a new environment from a seed
mkEnv' :: Int -> IO Env
mkEnv' seed = Env <$> (newTVarIO $ mkStdGen seed) <*> newTVarIO Set.empty

prettyEnv :: Env -> IO ()
prettyEnv env = do
  idSet <-
    atomically $ do
      usedIdSet <- readTVar $ view usedPositionIds env
      pure usedIdSet
  putStrLn $ "Used position IDs (" ++ show (Set.size idSet) ++ " in total):"
  putStrLn $ prettySet idSet
  where
    prettySet = intercalate ", " . (fmap show) . Set.elems

-- | Generates a unique position ID
mkPositionId ::
     ( Monad m
     , MonadIO m
     , MonadReader r m
     , HasRng r (TVar StdGen)
     , HasUsedPositionIds r (TVar (Set Integer))
     )
  => m Integer
mkPositionId = do
  e <- ask
  let tGen = view rng e
  let tSet = view usedPositionIds e
  liftIO $
    atomically $ do
      gen <- readTVar tGen
      idSet <- readTVar tSet
      let (positionId, newGen) = mkUniqId gen idSet
      writeTVar tGen newGen
      writeTVar tSet $ Set.insert positionId idSet
      pure positionId

-- | Keep generating new IDs until a new unique one is found
mkUniqId :: StdGen -> Set Integer -> (Integer, StdGen)
mkUniqId gen idSet =
  let res@(newId, newGen) = first (PositionId . abs) (random gen)
   in if Set.member newId idSet
        then mkUniqId newGen idSet
        else res

-- | TODO: Implement actual validation
validateCreatePositionItemArgs ::
     CreatePositionItemArgs
  -> Either PositionCommandError (PositionId -> CreatePositionItemCmdArgs)
validateCreatePositionItemArgs args =
  Right
    (\pid ->
       CreatePositionItemCmdArgs
         { _id = pid
         , _title = view title args
         , _amount = view amount args
         , _repetition = view repetition args
         })

mkCreatePositionItemCmd ::
     ( Monad m
     , MonadIO m
     , MonadReader r m
     , HasRng r (TVar StdGen)
     , HasUsedPositionIds r (TVar (Set PositionItemId))
     )
  => CreatePositionItemArgs
  -> m (Either PositionCommandError (PositionItemId, PositionCommand))
mkCreatePositionItemCmd args =
  case validateCreatePositionItemArgs args of
    Left e -> pure $ Left e
    Right mkCmdArgs -> do
      positionId <- mkPositionItemId
      pure $
        Right (positionId, CreatePositionItemCommand $ mkCmdArgs positionId)

mkCreatePositionGroupCmd :: () -> m PositionGroupId
mkCreatePositionGroupCmd _ = undefined

-- class Monad m =>
--       PositionCommander m
--   where
--   createPositionItem :: PositionProps -> m PositionId
positionGroup :: V.Vector Position -> Position
positionGroup c = Fix $ PositionGroup c

-- positionItem :: Position
-- positionItem = Fix $ PositionItem
cost :: Decimal -> Money
cost = Money . (* (-1))

earning :: Decimal -> Money
earning = Money

noName :: Maybe T.Text
noName = Nothing

mkName :: T.Text -> Maybe T.Text
mkName = Just
-- oneTimeExpense :: Money -> Maybe T.Text -> V.Vector Tag -> Expense
-- oneTimeExpense c n t =
--   Expense
--     { _expenseCost = c
--     , _expenseName = n
--     , _expenseRepetition = OneTimeExpense
--     }
-- monthlyExpense :: Money -> Maybe T.Text -> V.Vector Tag -> ExpenseCommand
-- monthlyExpense c n t =
--   CreateExpenseCommand $
--   CreateExpense
--     { _createExpenseCost = c
--     , _createExpenseName = n
--     , _createExpenseTags = t
--     , _createExpenseRepetition = RepeatingExpense MonthlyRepetition
--     }
-- history :: V.Vector ExpenseCommand
-- history =
--   V.fromList
--     [ monthlyExpense (earning 2274.62) (mkName "Gehalt") V.empty
--     , monthlyExpense (cost 460) (mkName "Miete") V.empty
--     , monthlyExpense (cost 13.99) (mkName "Netflix") V.empty
--     , monthlyExpense (cost 49.86) (mkName "Hetzner") V.empty
--     , monthlyExpense (cost 700) (mkName "FIL Fondsbank") V.empty
--     , monthlyExpense (cost 49.99) (mkName "Vodafone") V.empty
--     , monthlyExpense (cost 93.37) (mkName "SWISS LIFE AG") V.empty
--     ]
-- mkExpense :: CreateExpense -> Expense
-- mkExpense cec =
--   Expense
--     { _expenseCost = _createExpenseCost cec
--     , _expenseName = _createExpenseName cec
--     , _expenseTags = _createExpenseTags cec
--     , _expenseRepetition = _createExpenseRepetition cec
--     }
-- replayHistory :: V.Vector ExpenseCommand -> V.Vector Expense
-- replayHistory cmds = V.foldl replayCommand V.empty cmds
--   where
--     replayCommand exps (CreateExpenseCommand cec) = V.snoc exps $ mkExpense cec

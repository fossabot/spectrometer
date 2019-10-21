module Control.Parallel
  ( runActions
  , Progress(..)
  ) where

import Prologue

import Control.Concurrent.STM
import Polysemy
import Polysemy.Async
import Polysemy.Resource

newtype ActionId = ActionId { unActionId :: Int }
  deriving (Eq, Ord, Show, Generic)

data Action ty = Action
  { actionId     :: Int
  , actionAction :: ty
  , actionDeps   :: [ActionId]
  }

data State ty = State
  { stActionIdCur :: TVar Int
  , stQueued      :: TVar [Action ty]
  , stRunning     :: TVar [ActionId]
  , stCompleted   :: TVar Int
  }

data Progress = Progress
  { pRunning   :: Int
  , pQueued    :: Int
  , pCompleted :: Int
  } deriving (Eq, Ord, Show, Generic)

-- | Run arbitrary actions in parallel, given:
--
-- - @numThreads@ - The number of worker threads to run
-- - @initial@ - The initial list of actions
-- - @runAction@ - A function that runs an action, which can itself enqueue more actions
--   via the @enqueue@ function provided as an argument
-- - A function that can be used to report 'Progress'
--
-- All tasks will complete before this returns.
runActions :: forall r action
            . Members '[Embed IO, Async, Resource] r
           => Int -- number of threads
           -> [action] -- initial actions
           -> ((action -> [ActionId] -> Sem r ActionId) -> action -> Sem r ()) -- given an action to enqueue more actions, run an action
           -> (Progress -> Sem r ()) -- get progress updates
           -> Sem r ()
runActions numThreads initial runAction reportProgress = do
  state <- embed $
    State <$> newTVarIO 0
          <*> newTVarIO []
          <*> newTVarIO []
          <*> newTVarIO 0

  let enqueue :: Member (Embed IO) r => action -> [ActionId] -> Sem r ActionId
      enqueue action deps = embed $ atomically $ do
        actionId <- readTVar (stActionIdCur state)
        writeTVar (stActionIdCur state) (actionId + 1)

        modifyTVar (stQueued state) (Action actionId action deps :)
        pure (ActionId actionId)

  -- Enqueue initial actions
  traverse_ (\act -> enqueue act []) initial

  _ <- async $ updateProgress reportProgress state

  if numThreads > 1
    then do
      replicateM_ numThreads (async (worker (runAction enqueue) state))

      -- wait for queued and running tasks to end
      embed $ atomically $ do
        queued  <- readTVar (stQueued state)
        check (null queued)
        running <- readTVar (stRunning state)
        check (null running)

    else worker (runAction enqueue) state

  pure ()

updateProgress :: Member (Embed IO) r => (Progress -> Sem r ()) -> State any -> Sem r ()
updateProgress f st@State{..} = loop (Progress 0 0 0)
  where
  loop prev = join $ embed $ atomically $ stopWhenDone st $ do
    running <- readTVar stRunning
    queued <- length <$> readTVar stQueued
    completed <- readTVar stCompleted

    let new = Progress (length running) queued completed

    check (prev /= new)

    pure $ f new *> loop new

stopWhenDone :: State any -> STM (Sem r ()) -> STM (Sem r ())
stopWhenDone State{..} act = do
  queued <- readTVar stQueued
  case queued of
    [] -> do
      running <- readTVar stRunning
      if null running
        then pure (pure ())
        else act
    _ -> act

worker :: forall r action. (Member (Embed IO) r, Member Resource r) => (action -> Sem r ()) -> State action -> Sem r ()
worker runAction st@State{..} = loop
  where

  loop = join $ embed $ atomically $ stopWhenDone st $ do
    running <- readTVar stRunning
    queued <- readTVar stQueued
    case queued of
      [] -> retry
      (x:xs) -> do
        writeTVar stQueued xs
        addRunning
        --pure $ runAction x `finally` (complete *> loop)
        undefined

  addRunning :: STM ()
  --addRunning = modifyTVar stRunning (+1)
  addRunning = undefined

  complete :: Sem r ()
  complete = undefined
  --complete = embed $ atomically $ modifyTVar stRunning (subtract 1) *> modifyTVar stCompleted (+1)

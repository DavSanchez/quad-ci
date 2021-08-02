module JobHandler.Memory where

import qualified Agent
import qualified Control.Concurrent.STM as STM
import Core
import qualified JobHandler
import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map

data State = State
  { jobs :: Map BuildNumber JobHandler.Job,
    logs :: Map (BuildNumber, StepName) ByteString,
    nextBuild :: Int
  }
  deriving (Eq, Show)

createService :: IO JobHandler.Service
createService = do
  state <-
    STM.newTVarIO
      State
        { jobs = mempty,
          logs = mempty,
          nextBuild = 1
        }
  pure
    JobHandler.Service
      { queueJob = \pipeline -> STM.atomically do
          STM.stateTVar state $ queueJob' pipeline,
        findJob = \number -> STM.atomically do
          s <- STM.readTVar state
          pure $ findJob' number s,
        dispatchCmd = STM.atomically do
          STM.stateTVar state dispatchCmd',
        processMsg = \msg -> STM.atomically do
          STM.modifyTVar' state $ processMsg' msg
      }

queueJob' :: Pipeline -> State -> (BuildNumber, State)
queueJob' pipeline state = (number, updatedState)
  where
    number = BuildNumber state.nextBuild
    job =
      JobHandler.Job
        { pipeline = pipeline,
          state = JobHandler.JobQueued
        }
    updatedState =
      state
        { jobs = Map.insert number job state.jobs,
          nextBuild = state.nextBuild + 1
        }

findJob' :: BuildNumber -> State -> Maybe JobHandler.Job
findJob' number state = Map.lookup number state.jobs

dispatchCmd' :: State -> (Maybe Agent.Cmd, State)
dispatchCmd' state =
  case List.find queued $ Map.toList state.jobs of
    Just (number, job) ->
      let updatedJob = job{state = JobHandler.JobAssigned}
          updatedState = Map.insert number updatedJob state.jobs
          cmd = Just $ Agent.StartBuild number job.pipeline
       in (cmd, state{jobs = updatedState})
    _ -> (Nothing, state)
  where
    queued (_, job) = job.state == JobHandler.JobQueued

processMsg' :: Agent.Msg -> State -> State
processMsg' msg state = case msg of
  Agent.BuildUpdated number build ->
    let f job = job{state = JobHandler.JobScheduled build}
    in state{jobs = Map.adjust f number state.jobs}
  Agent.LogCollected number log ->
    let updatedLogs = Map.insertWith (flip (<>)) (number, log.step) log.output state.logs
    in state{logs = updatedLogs}
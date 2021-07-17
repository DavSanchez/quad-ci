module Runner where

import Core
import qualified Docker
import RIO

data Hooks = Hooks {
  logCollected :: Log -> IO ()
}

data Service = Service
  { runBuild :: Hooks -> Build -> IO Build,
    prepareBuild :: Pipeline -> IO Build
  }

createService :: Docker.Service -> IO Service
createService docker = do
  pure
    Service
      { runBuild = runBuild_ docker,
        prepareBuild = prepareBuild' docker
      }

runBuild_ :: Docker.Service -> Hooks -> Build -> IO Build
runBuild_ docker hooks build = do
  loop build $ Core.initLogCollection build.pipeline
  where
    loop :: Build -> LogCollection -> IO Build
    loop build collection = do
      (newCollection, logs) <- Core.collectLogs docker collection build
      newBuild <- Core.progress docker build
      traverse_ hooks.logCollected logs
      case newBuild.state of
        BuildFinished _ -> pure newBuild
        _ -> do
          threadDelay (1 * 1000 * 1000)
          loop newBuild newCollection

prepareBuild' :: Docker.Service -> Pipeline -> IO Build
prepareBuild' docker pipeline = do
  volume <- docker.createVolume
  pure
    Build
      { pipeline = pipeline,
        state = BuildReady,
        completedSteps = mempty,
        volume = volume
      }
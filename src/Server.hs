module Server where

import qualified Codec.Serialise as Serialise
import Core
import qualified Data.Aeson as Aeson
import qualified GitHub
import qualified JobHandler
import qualified Network.HTTP.Types as HTTP.Types
import RIO
import qualified RIO.NonEmpty as NonEmpty
import qualified Web.Scotty as Scotty

data Config = Config {port :: Int}

run :: Config -> JobHandler.Service -> IO ()
run config handler =
  Scotty.scotty config.port do
    Scotty.post "/agent/pull" do
      cmd <- Scotty.liftAndCatchIO $ handler.dispatchCmd
      Scotty.raw $ Serialise.serialise cmd

    Scotty.post "/agent/send" do
      msg <- Serialise.deserialise <$> Scotty.body
      Scotty.liftAndCatchIO $ handler.processMsg msg
      Scotty.json ("message processed" :: Text)

    Scotty.post "/webhook/github" do
      body <- Scotty.body

      number <- Scotty.liftAndCatchIO do
        info <- GitHub.parsePushEvent (toStrictBytes body)
        pipeline <- GitHub.fetchRemotePipeline info
        let step = GitHub.createCloneStep info

        handler.queueJob $
          pipeline
            { steps = NonEmpty.cons step pipeline.steps
            }
      Scotty.json $
        Aeson.object
          [ ("number", Aeson.toJSON $ Core.buildNumberToInt number),
            ("status", "job queued")
          ]

    Scotty.get "/build/:number" do
      number <- BuildNumber <$> Scotty.param "number"

      job <-
        Scotty.liftAndCatchIO (handler.findJob number) >>= \case
          Nothing -> Scotty.raiseStatus HTTP.Types.status404 "Build not found"
          Just j -> pure j

      Scotty.json $ jobToJson number job

jobToJson :: BuildNumber -> JobHandler.Job -> Aeson.Value
jobToJson number job =
  Aeson.object
    [ ("number", Aeson.toJSON $ Core.buildNumberToInt number),
      ("state", Aeson.toJSON $ jobStateToText job.state),
      ("steps", Aeson.toJSON steps)
    ]
  where
    build = case job.state of
      JobHandler.JobQueued -> Nothing
      JobHandler.JobAssigned -> Nothing
      JobHandler.JobScheduled b -> Just b
    steps =
      job.pipeline.steps <&> \step ->
        Aeson.object
          [ ("name", Aeson.String $ Core.stepNameToText step.name),
            ( "state",
              Aeson.String $ case build of
                Just b -> stepStateToText b step
                Nothing -> "ready"
            )
          ]

jobStateToText :: JobHandler.JobState -> Text
jobStateToText = \case
  JobHandler.JobQueued -> "queued"
  JobHandler.JobAssigned -> "assigned"
  JobHandler.JobScheduled b -> case b.state of
    BuildReady -> "ready"
    BuildRunning _ -> "running"
    BuildFinished result -> case result of
      BuildSucceeded -> "succeeded"
      BuildFailed -> "failed"
      BuildUnexpectedState _ -> "unexpectedstate"
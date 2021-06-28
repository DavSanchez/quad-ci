module Docker where

import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified Socket

data Service = Service
  { createContainer :: CreateContainerOptions -> IO ContainerId,
    startContainer :: ContainerId -> IO (),
    containerStatus :: ContainerId -> IO ContainerStatus,
    createVolume :: IO Volume
  }

data CreateContainerOptions = CreateContainerOptions
  { image :: Image,
    script :: Text,
    volume :: Volume
  }

data ContainerStatus = ContainerRunning | ContainerExited ContainerExitCode | ContainerOther Text
  deriving (Eq, Show)

newtype Image = Image Text deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int deriving (Eq, Show)

newtype ContainerId = ContainerId Text
  deriving (Eq, Show)

type RequestBuilder = Text -> HTTP.Request

newtype Volume = Volume Text deriving (Eq, Show)

volumeToText :: Volume -> Text
volumeToText (Volume v) = v

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId c) = c

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

imageToText :: Docker.Image -> Text
imageToText (Docker.Image image) = image

createService :: IO Service
createService = do
  -- Init manager once
  manager <- Socket.newManager "/var/run/docker.sock"

  -- Make Request
  let makeReq :: RequestBuilder
      makeReq path = HTTP.defaultRequest & HTTP.setRequestPath (encodeUtf8 $ "/v1.41" <> path) & HTTP.setRequestManager manager

  pure
    Service
      { createContainer = createContainer' makeReq,
        startContainer = startContainer' makeReq,
        containerStatus = containerStatus' makeReq,
        createVolume = createVolume' makeReq
      }

createContainer' :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainer' makeReq options = do
  manager <- Socket.newManager "/var/run/docker.sock"
  let image = imageToText options.image
      bind = volumeToText options.volume <> ":/app"
      body =
        Aeson.object
          [ ("Image", Aeson.toJSON image),
            ("Tty", Aeson.toJSON True),
            ("Labels", Aeson.object [("quad", "")]),
            ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"]),
            ("Cmd", "echo \"$QUAD_SCRIPT\" | /bin/sh"),
            ("Env", Aeson.toJSON ["QUAD_SCRIPT=" <> options.script]),
            ("WorkingDir", "/app"),
            ("HostConfig", Aeson.object [("Binds", Aeson.toJSON [bind])])
          ]
      req =
        makeReq "/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body
      parser = Aeson.withObject "create-container" $ \o -> do
        cId <- o .: "Id"
        pure $ ContainerId cId
  res <- HTTP.httpBS req
  parseResponse res parser

startContainer' :: RequestBuilder -> ContainerId -> IO ()
startContainer' makeReq container = do
  let path = "/containers/" <> containerIdToText container <> "/start"
      req = makeReq path & HTTP.setRequestMethod "POST"
  void $ HTTP.httpBS req

containerStatus' :: RequestBuilder -> ContainerId -> IO ContainerStatus
containerStatus' makeReq container = do
  let parser = Aeson.withObject "container-inspect" $ \o -> do
        state <- o .: "State"
        status <- state .: "Status"
        case status of
          "running" -> pure ContainerRunning
          "exited" -> do
            code <- state .: "ExitCode"
            pure $ ContainerExited (ContainerExitCode code)
          other -> pure $ ContainerOther other
  let req = makeReq $ "/containers/" <> containerIdToText container <> "/json"
  res <- HTTP.httpBS req
  parseResponse res parser

createVolume' :: RequestBuilder -> IO Volume
createVolume' makeReq = do
  let body = Aeson.object [("Labels", Aeson.object [("quad", "")])]
      req = makeReq "/volumes/create" & HTTP.setRequestMethod "POST" & HTTP.setRequestBodyJSON body
      parser = Aeson.withObject "create-volume" $ \o -> do
        name <- o .: "Name"
        pure $ Volume name
  res <- HTTP.httpBS req
  parseResponse res parser

parseResponse :: HTTP.Response ByteString -> (Aeson.Value -> Aeson.Types.Parser a) -> IO a
parseResponse res parser = do
  let result = do
        value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
        Aeson.Types.parseEither parser value
  case result of
    Left e -> throwString e
    Right status -> pure status
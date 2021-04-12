import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Managed ( managed, managed_, runManaged )
import qualified DearImGui as G
import DearImGui.OpenGL2
import DearImGui.GLFW
import DearImGui.GLFW.OpenGL
import Graphics.GL
import Graphics.UI.GLFW (Window)
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad.State ( StateT(runStateT), MonadState(get) )
import Data.IORef
import qualified Hw.Gui.ServerWindow as ServerWindow
import qualified Hw.Gui.ClientWindow as ClientWindow
import Data.Maybe

main :: IO ()
main = do
  initialised <- GLFW.init
  unless initialised $ error "GLFW init failed"

  runManaged $ do
    mwin <- managed $ bracket
      (GLFW.createWindow 800 600 "Hello, Dear ImGui!" Nothing Nothing)
      (maybe (return ()) GLFW.destroyWindow)
    case mwin of
      Just win -> do
        liftIO $ do
          GLFW.maximizeWindow win
          GLFW.makeContextCurrent (Just win)
          GLFW.swapInterval 1

        -- Create an ImGui context
        _ <- managed $ bracket G.createContext G.destroyContext

        -- Initialize ImGui's GLFW backend
        _ <- managed_ $ bracket_ (glfwInitForOpenGL win True) glfwShutdown

        -- Initialize ImGui's OpenGL backend
        _ <- managed_ $ bracket_ openGL2Init openGL2Shutdown

        liftIO $ runMainLoop win
      Nothing -> do
        error "GLFW createWindow failed"

  GLFW.terminate

data LoopContext = LoopContext {
  lcServerWS :: IORef ServerWindow.St,
  lcClientWS :: IORef ClientWindow.St,
  lcServerPendingException :: IORef (Maybe String),
  lcClientPendingException :: IORef (Maybe String)
}

runMainLoop :: Window -> IO ()
runMainLoop win = do
  serverWS <- ServerWindow.mkSt >>= newIORef
  serverPendingException <- newIORef Nothing
  clientWS <- ClientWindow.mkSt >>= newIORef
  clientPendingException <- newIORef Nothing
  let ctx = LoopContext {
    lcServerWS = serverWS,
    lcServerPendingException = serverPendingException,
    lcClientWS = clientWS,
    lcClientPendingException = clientPendingException
  }
  mainLoop win ctx

mainLoop :: Window -> LoopContext -> IO ()
mainLoop win ctx = do
  -- Process the event loop
  GLFW.pollEvents
  close <- GLFW.windowShouldClose win
  unless close do

    -- Tell ImGui we're starting a new frame
    openGL2NewFrame
    glfwNewFrame
    G.newFrame

    -- Build the GUI
    bracket_ (G.begin "Server") G.end $
      handle (catchWindowException "server" $ lcServerPendingException ctx) do
        renderWindowException "server" (lcServerPendingException ctx)
        stIn <- readIORef (lcServerWS ctx)
        (_, out) <- runStateT ServerWindow.render stIn
        writeIORef (lcServerWS ctx) $! out
    bracket_ (G.begin "Client") G.end $
      handle (catchWindowException "client" $ lcClientPendingException ctx) do
        renderWindowException "client" (lcClientPendingException ctx)
        stIn <- readIORef (lcClientWS ctx)
        (_, out) <- runStateT ClientWindow.render stIn
        writeIORef (lcClientWS ctx) $! out

    -- Render
    glClear GL_COLOR_BUFFER_BIT

    G.render
    openGL2RenderDrawData =<< G.getDrawData

    GLFW.swapBuffers win

    mainLoop win ctx

catchWindowException :: String -> IORef (Maybe String) -> SomeException -> IO ()
catchWindowException title store e = do
  print e
  G.openPopup $ "Exception (" ++ title ++ ")"
  writeIORef store (Just $ show e)

renderWindowException :: String -> IORef (Maybe String) -> IO ()
renderWindowException title store = do
  exc <- readIORef store
  opened <- G.beginPopupModal $ "Exception (" ++ title ++ ")"
  when opened do
    G.textWrapped $ fromMaybe "?" exc
    ok <- G.button "OK"
    when ok do
      G.closeCurrentPopup
      writeIORef store Nothing
    G.endPopup

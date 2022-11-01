module Termbox.Internal.Main
  ( initialize,
    shutdown,
    InitError (..),
  )
where

import Control.Exception
import qualified Termbox.Bindings.Hs

-- | @termbox@ initialization errors.
data InitError
  = FailedToOpenTTY
  | PipeTrapError
  | UnsupportedTerminal
  deriving anyclass (Exception)
  deriving stock (Show)

-- | Initialize a @termbox@ program.
--
-- If @initialize@ succeeds, it must be paired with a call to 'shutdown'.
initialize :: IO (Either InitError ())
initialize =
  Termbox.Bindings.Hs.tb_init >>= \case
    Left err ->
      (pure . Left) case err of
        Termbox.Bindings.Hs.TB_EFAILED_TO_OPEN_TTY -> FailedToOpenTTY
        Termbox.Bindings.Hs.TB_EPIPE_TRAP_ERROR -> PipeTrapError
        Termbox.Bindings.Hs.TB_EUNSUPPORTED_TERMINAL -> UnsupportedTerminal
    Right () -> do
      _ <- Termbox.Bindings.Hs.tb_select_input_mode Termbox.Bindings.Hs.TB_INPUT_MOUSE
      _ <- Termbox.Bindings.Hs.tb_select_output_mode Termbox.Bindings.Hs.TB_OUTPUT_256
      pure (Right ())

-- | Shut down a @termbox@ program.
shutdown :: IO ()
shutdown = do
  _ <- Termbox.Bindings.Hs.tb_select_output_mode Termbox.Bindings.Hs.TB_OUTPUT_NORMAL
  Termbox.Bindings.Hs.tb_shutdown

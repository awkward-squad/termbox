module Termbox.Internal.Main
  ( run,
    initialize,
    finalize,
    InitError (..),
  )
where

import Control.Exception (Exception, mask, onException)
import qualified Termbox.Bindings.Hs

-- | @termbox@ initialization errors.
data InitError
  = FailedToOpenTTY
  | PipeTrapError
  | UnsupportedTerminal
  deriving anyclass (Exception)
  deriving stock (Show)

-- | Initialize a @termbox@ program, and if that succeeds, run the provided action, then finalize the @termbox@ program.
run :: IO a -> IO (Either InitError a)
run action =
  mask \unmask ->
    initialize >>= \case
      Left err -> pure (Left err)
      Right () -> do
        result <- unmask action `onException` finalize
        finalize
        pure (Right result)

-- | Initialize a @termbox@ program.
--
-- If @initialize@ succeeds, it must be paired with a call to 'finalize'.
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
finalize :: IO ()
finalize = do
  _ <- Termbox.Bindings.Hs.tb_select_output_mode Termbox.Bindings.Hs.TB_OUTPUT_NORMAL
  Termbox.Bindings.Hs.tb_shutdown

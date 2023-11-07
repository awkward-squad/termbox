module Termbox2.Bindings.Hs.Internal.Error
  ( Tb_error
      ( Tb_error,
        TB_ERR,
        TB_ERR_CAP_COLLISION,
        TB_ERR_INIT_ALREADY,
        TB_ERR_INIT_OPEN,
        TB_ERR_MEM,
        TB_ERR_NEED_MORE,
        TB_ERR_NOT_INIT,
        TB_ERR_NO_EVENT,
        TB_ERR_NO_TERM,
        TB_ERR_OUT_OF_BOUNDS,
        TB_ERR_POLL,
        TB_ERR_READ,
        TB_ERR_RESIZE_IOCTL,
        TB_ERR_RESIZE_PIPE,
        TB_ERR_RESIZE_POLL,
        TB_ERR_RESIZE_READ,
        TB_ERR_RESIZE_SIGACTION,
        TB_ERR_RESIZE_SSCANF,
        TB_ERR_RESIZE_WRITE,
        TB_ERR_TCGETATTR,
        TB_ERR_TCSETATTR,
        TB_ERR_UNSUPPORTED_TERM
      ),
  )
where

import Foreign.C (CInt)
import Termbox2.Bindings.C

newtype Tb_error
  = Tb_error CInt
  deriving stock (Eq)

pattern TB_ERR :: Tb_error
pattern TB_ERR <- ((== Tb_error _TB_ERR) -> True)
  where
    TB_ERR = Tb_error _TB_ERR

pattern TB_ERR_CAP_COLLISION :: Tb_error
pattern TB_ERR_CAP_COLLISION <- ((== Tb_error _TB_ERR_CAP_COLLISION) -> True)
  where
    TB_ERR_CAP_COLLISION = Tb_error _TB_ERR_CAP_COLLISION

pattern TB_ERR_INIT_ALREADY :: Tb_error
pattern TB_ERR_INIT_ALREADY <- ((== Tb_error _TB_ERR_INIT_ALREADY) -> True)
  where
    TB_ERR_INIT_ALREADY = Tb_error _TB_ERR_INIT_ALREADY

pattern TB_ERR_INIT_OPEN :: Tb_error
pattern TB_ERR_INIT_OPEN <- ((== Tb_error _TB_ERR_INIT_OPEN) -> True)
  where
    TB_ERR_INIT_OPEN = Tb_error _TB_ERR_INIT_OPEN

pattern TB_ERR_MEM :: Tb_error
pattern TB_ERR_MEM <- ((== Tb_error _TB_ERR_MEM) -> True)
  where
    TB_ERR_MEM = Tb_error _TB_ERR_MEM

pattern TB_ERR_NEED_MORE :: Tb_error
pattern TB_ERR_NEED_MORE <- ((== Tb_error _TB_ERR_NEED_MORE) -> True)
  where
    TB_ERR_NEED_MORE = Tb_error _TB_ERR_NEED_MORE

pattern TB_ERR_NOT_INIT :: Tb_error
pattern TB_ERR_NOT_INIT <- ((== Tb_error _TB_ERR_NOT_INIT) -> True)
  where
    TB_ERR_NOT_INIT = Tb_error _TB_ERR_NOT_INIT

pattern TB_ERR_NO_EVENT :: Tb_error
pattern TB_ERR_NO_EVENT <- ((== Tb_error _TB_ERR_NO_EVENT) -> True)
  where
    TB_ERR_NO_EVENT = Tb_error _TB_ERR_NO_EVENT

pattern TB_ERR_NO_TERM :: Tb_error
pattern TB_ERR_NO_TERM <- ((== Tb_error _TB_ERR_NO_TERM) -> True)
  where
    TB_ERR_NO_TERM = Tb_error _TB_ERR_NO_TERM

pattern TB_ERR_OUT_OF_BOUNDS :: Tb_error
pattern TB_ERR_OUT_OF_BOUNDS <- ((== Tb_error _TB_ERR_OUT_OF_BOUNDS) -> True)
  where
    TB_ERR_OUT_OF_BOUNDS = Tb_error _TB_ERR_OUT_OF_BOUNDS

pattern TB_ERR_POLL :: Tb_error
pattern TB_ERR_POLL <- ((== Tb_error _TB_ERR_POLL) -> True)
  where
    TB_ERR_POLL = Tb_error _TB_ERR_POLL

pattern TB_ERR_READ :: Tb_error
pattern TB_ERR_READ <- ((== Tb_error _TB_ERR_READ) -> True)
  where
    TB_ERR_READ = Tb_error _TB_ERR_READ

pattern TB_ERR_RESIZE_IOCTL :: Tb_error
pattern TB_ERR_RESIZE_IOCTL <- ((== Tb_error _TB_ERR_RESIZE_IOCTL) -> True)
  where
    TB_ERR_RESIZE_IOCTL = Tb_error _TB_ERR_RESIZE_IOCTL

pattern TB_ERR_RESIZE_PIPE :: Tb_error
pattern TB_ERR_RESIZE_PIPE <- ((== Tb_error _TB_ERR_RESIZE_PIPE) -> True)
  where
    TB_ERR_RESIZE_PIPE = Tb_error _TB_ERR_RESIZE_PIPE

pattern TB_ERR_RESIZE_POLL :: Tb_error
pattern TB_ERR_RESIZE_POLL <- ((== Tb_error _TB_ERR_RESIZE_POLL) -> True)
  where
    TB_ERR_RESIZE_POLL = Tb_error _TB_ERR_RESIZE_POLL

pattern TB_ERR_RESIZE_READ :: Tb_error
pattern TB_ERR_RESIZE_READ <- ((== Tb_error _TB_ERR_RESIZE_READ) -> True)
  where
    TB_ERR_RESIZE_READ = Tb_error _TB_ERR_RESIZE_READ

pattern TB_ERR_RESIZE_SIGACTION :: Tb_error
pattern TB_ERR_RESIZE_SIGACTION <- ((== Tb_error _TB_ERR_RESIZE_SIGACTION) -> True)
  where
    TB_ERR_RESIZE_SIGACTION = Tb_error _TB_ERR_RESIZE_SIGACTION

pattern TB_ERR_RESIZE_SSCANF :: Tb_error
pattern TB_ERR_RESIZE_SSCANF <- ((== Tb_error _TB_ERR_RESIZE_SSCANF) -> True)
  where
    TB_ERR_RESIZE_SSCANF = Tb_error _TB_ERR_RESIZE_SSCANF

pattern TB_ERR_RESIZE_WRITE :: Tb_error
pattern TB_ERR_RESIZE_WRITE <- ((== Tb_error _TB_ERR_RESIZE_WRITE) -> True)
  where
    TB_ERR_RESIZE_WRITE = Tb_error _TB_ERR_RESIZE_WRITE

pattern TB_ERR_TCGETATTR :: Tb_error
pattern TB_ERR_TCGETATTR <- ((== Tb_error _TB_ERR_TCGETATTR) -> True)
  where
    TB_ERR_TCGETATTR = Tb_error _TB_ERR_TCGETATTR

pattern TB_ERR_TCSETATTR :: Tb_error
pattern TB_ERR_TCSETATTR <- ((== Tb_error _TB_ERR_TCSETATTR) -> True)
  where
    TB_ERR_TCSETATTR = Tb_error _TB_ERR_TCSETATTR

pattern TB_ERR_UNSUPPORTED_TERM :: Tb_error
pattern TB_ERR_UNSUPPORTED_TERM <- ((== Tb_error _TB_ERR_UNSUPPORTED_TERM) -> True)
  where
    TB_ERR_UNSUPPORTED_TERM = Tb_error _TB_ERR_UNSUPPORTED_TERM

{-# COMPLETE
  TB_ERR,
  TB_ERR_CAP_COLLISION,
  TB_ERR_INIT_ALREADY,
  TB_ERR_INIT_OPEN,
  TB_ERR_MEM,
  TB_ERR_NEED_MORE,
  TB_ERR_NOT_INIT,
  TB_ERR_NO_EVENT,
  TB_ERR_NO_TERM,
  TB_ERR_OUT_OF_BOUNDS,
  TB_ERR_POLL,
  TB_ERR_READ,
  TB_ERR_RESIZE_IOCTL,
  TB_ERR_RESIZE_PIPE,
  TB_ERR_RESIZE_POLL,
  TB_ERR_RESIZE_READ,
  TB_ERR_RESIZE_SIGACTION,
  TB_ERR_RESIZE_SSCANF,
  TB_ERR_RESIZE_WRITE,
  TB_ERR_TCGETATTR,
  TB_ERR_TCSETATTR,
  TB_ERR_UNSUPPORTED_TERM
  #-}

module Termbox.Image
  ( Image,
    foreground,
    background,
    string,
    hcat,
    vcat,
    zcat,
    draw,
  )
where

import qualified Data.Char as Char
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Maybe
import Foreign.C.Types (CInt (CInt), CWchar (CWchar))
import Termbox.Attr
import qualified Termbox.Bindings

data Image
  = ImageEmpty
  | ImageNonEmpty NonEmptyImage

data NonEmptyImage = NonEmptyImage
  { width :: {-# UNPACK #-} !Int,
    height :: {-# UNPACK #-} !Int,
    content :: !Content
  }

data Content
  = ContentText (List.NonEmpty Char) -- Invariant: all 1-width
  | ContentFg Attr Content
  | ContentBg Attr Content
  | ContentHcat NonEmptyImage NonEmptyImage
  | ContentVcat NonEmptyImage NonEmptyImage
  | ContentZcat NonEmptyImage NonEmptyImage

foreground :: Attr -> Image -> Image
foreground attr = \case
  ImageEmpty -> ImageEmpty
  ImageNonEmpty NonEmptyImage {width, height, content} ->
    ImageNonEmpty NonEmptyImage {width, height, content = ContentFg attr content}

background :: Attr -> Image -> Image
background attr = \case
  ImageEmpty -> ImageEmpty
  ImageNonEmpty NonEmptyImage {width, height, content} ->
    ImageNonEmpty NonEmptyImage {width, height, content = ContentBg attr content}

string :: String -> Image
string s0 =
  case List.NonEmpty.nonEmpty (keepWidth1 s0) of
    Nothing -> ImageEmpty
    Just s ->
      ImageNonEmpty
        NonEmptyImage
          { width = length s,
            height = 1,
            content = ContentText s
          }

-- Keep only characters with width 1.
keepWidth1 :: [Char] -> [Char]
keepWidth1 =
  mapMaybe \c ->
    case wcwidth (charToCWchar c) of
      1 -> Just c
      _ -> Nothing

charToCWchar :: Char -> CWchar
charToCWchar =
  fromIntegral . Char.ord
{-# INLINE charToCWchar #-}

hcat :: Image -> Image -> Image
hcat image00 image01 =
  case image00 of
    ImageEmpty -> image01
    ImageNonEmpty image0@NonEmptyImage {width = w0, height = h0} ->
      case image01 of
        ImageEmpty -> image00
        ImageNonEmpty image1@NonEmptyImage {width = w1, height = h1} ->
          ImageNonEmpty
            $! NonEmptyImage
              { width = w0 + w1,
                height = max h0 h1,
                content = ContentHcat image0 image1
              }

vcat :: Image -> Image -> Image
vcat image00 image01 =
  case image00 of
    ImageEmpty -> image01
    ImageNonEmpty image0@NonEmptyImage {width = w0, height = h0} ->
      case image01 of
        ImageEmpty -> image00
        ImageNonEmpty image1@NonEmptyImage {width = w1, height = h1} ->
          ImageNonEmpty
            $! NonEmptyImage
              { width = max w0 w1,
                height = h0 + h1,
                content = ContentVcat image0 image1
              }

zcat :: Image -> Image -> Image
zcat image00 image01 =
  case image00 of
    ImageEmpty -> image01
    ImageNonEmpty image0@NonEmptyImage {width = w0, height = h0} ->
      case image01 of
        ImageEmpty -> image00
        ImageNonEmpty image1@NonEmptyImage {width = w1, height = h1} ->
          ImageNonEmpty
            $! NonEmptyImage
              { width = max w0 w1,
                height = max h0 h1,
                content = ContentZcat image0 image1
              }

draw :: Int -> Int -> Image -> IO ()
draw rx ry = \case
  ImageEmpty -> pure ()
  ImageNonEmpty image ->
    draw1
      R
        { rx,
          ry,
          rfg = Nothing,
          rbg = Nothing
        }
      image

data R = R
  { rx :: {-# UNPACK #-} !Int,
    ry :: {-# UNPACK #-} !Int,
    rfg :: !(Maybe Attr),
    rbg :: !(Maybe Attr)
  }

draw1 :: R -> NonEmptyImage -> IO ()
draw1 r@R {rx, ry, rfg, rbg} NonEmptyImage {width, height, content = content0} =
  case content0 of
    ContentBg bg content ->
      let r' =
            case rbg of
              Nothing -> r {rbg = Just bg}
              Just _ -> r
       in draw1 r' NonEmptyImage {width, height, content}
    ContentFg fg content ->
      let r' =
            case rfg of
              Nothing -> r {rfg = Just fg}
              Just _ -> r
       in draw1 r' NonEmptyImage {width, height, content}
    ContentText cs ->
      for_ (zip [rx ..] (List.NonEmpty.toList cs)) \(x, c) ->
        drawc r {rx = x} c
    ContentHcat image0@NonEmptyImage {width = w0} image1 -> do
      draw1 r image0
      draw1 r {rx = rx + w0} image1
    ContentVcat image0@NonEmptyImage {height = h0} image1 -> do
      draw1 r image0
      draw1 r {ry = ry + h0} image1
    ContentZcat image0 image1 -> do
      draw1 r image0
      draw1 r image1

drawc :: R -> Char -> IO ()
drawc R {rx, ry, rfg, rbg} c =
  Termbox.Bindings.tb_change_cell rx ry c fg bg
  where
    fg =
      case rfg of
        Nothing -> 0
        Just (Attr n) -> n
    bg =
      case rbg of
        Nothing -> 0
        Just (Attr n) -> n

foreign import capi unsafe "wchar.h wcwidth"
  wcwidth :: CWchar -> CInt

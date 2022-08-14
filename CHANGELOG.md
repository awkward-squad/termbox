## [0.4.0] - Unreleased
- Support GHC 9.4.1
- Fix off-by-one error in named colors (black, etc) (thanks @seagreen)

## [0.3.0] - 2020-09-20
- Add `Cells` and `Cursor` types
- Export `Termbox.Internal` module that roughly corresponds to the C library
- Add a few arguments to the action provided to `run`
- Make `run` throw `InitError`s as IO exceptions
- Reset output mode to "normal" on shutdown to work around a small bug in termbox.c that retains the output mode across
  separate invocations of init/shutdown
- Change type of `set` to construct a `Cells` rather than an `IO ()`
- Change a few keys into pattern synonyms because they overlap
- Remove the alt modifier field from `KeyEvent`
- Remove `setCursor`, `hideCursor`, `clear`, `flush`, `getCells`, `getSize`, `poll`, `run_`
- Remove `InputMode`, `MouseMode`, and `OutputMode`, providing sane defaults instead
- Remove build dependency on `c2hs`
- Remove support for GHC < 8.2

## [0.2.0.1] - 2020-06-27
- Bump `base` upper bound

## [0.2.0] - 2019-06-21
- Add `getCells` function
- Add `run` function
- Rename `size` to `getSize`
- Rename `main` to `run_` and return errors as an `Either` instead of throwing.
- Make `Attr`'s `Semigroup` instance right-biased instead of left-biased.
- Make `Attr`'s `Num` instance total.
- Remove `buffer` function

## [0.1.0] - 2018-07-18
- Initial release

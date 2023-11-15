## [1.0.0] - Unreleased

- Rework `Tb_attr` and `Tb_color` into `Tb_color_and_attrs`
- Make `mod` field of `Tb_event` a `Maybe Tb_event_mod` rather than a `Tb_event_mod`
- Make `Show` instance of `Tb_key` not call `error` on unknown keys
- Hide representation of `Tb_event_mod`
- Improve `Show` instance of `Tb_event_mod`
- Delete `tb_attr`

## [0.1.1] - November 5, 2023

- Add `Monoid` instance for `Tb_attr`

## [0.1.0] - October 25, 2022

- Initial release

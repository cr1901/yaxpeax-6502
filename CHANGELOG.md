# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.0.2] - 2021-10-23
### Added
- `std` feature, to enable using yaxpeax-6502 in either `std` or `no_std`
  environments.

### Changed
- Crate now compiles for `no_std` by default, and nominally does not require
  allocation, even in `fmt::Display` impls.
- Target [yaxpeax-arch] version 0.2.4, and export their `StandardDecodeError`
  type as `DecodeError`.

### Removed
- [take_mut] was deemed unnecessary for the decoder implementation.

## [0.0.1] - 2021-05-07
### Added
- Initial release with working NMOS 6502 decoder that implements [yaxpeax-arch]
  traits.

[yaxpeax-arch]: https://git.iximeow.net/yaxpeax-arch/
[take_mut]: https://github.com/Sgeo/take_mut

[Unreleased]: https://github.com/cr1901/yaxpeax-6502/compare/v0.0.2...HEAD
[0.0.2]: https://github.com/cr1901/yaxpeax-6502/compare/v0.0.1...v0.0.2
[0.0.1]: https://github.com/cr1901/yaxpeax-6502/releases/tag/v0.0.1

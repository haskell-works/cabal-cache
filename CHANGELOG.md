# Changelog

## [Unreleased](https://github.com/haskell-works/cabal-cache/tree/HEAD)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.1.0.0...HEAD)

**Closed issues:**

- sync-from-archive does not consider ${archive\_version}/${store\_hash} subfolder if syncing from \(local\) archive directory [\#236](https://github.com/haskell-works/cabal-cache/issues/236)
- Wrong region parsing [\#230](https://github.com/haskell-works/cabal-cache/issues/230)

**Merged pull requests:**

- Issue 236 make sync from archive consider store hash folder [\#238](https://github.com/haskell-works/cabal-cache/pull/238) ([newhoggy](https://github.com/newhoggy))
- CHG: handle NotFound in readFirstAvailableResource to make sure that … [\#237](https://github.com/haskell-works/cabal-cache/pull/237) ([snetramo](https://github.com/snetramo))
- Update upper bounds [\#234](https://github.com/haskell-works/cabal-cache/pull/234) ([newhoggy](https://github.com/newhoggy))

## [v1.1.0.0](https://github.com/haskell-works/cabal-cache/tree/v1.1.0.0) (2023-08-09)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.6.1...v1.1.0.0)

**Merged pull requests:**

- Support newer ghcs [\#233](https://github.com/haskell-works/cabal-cache/pull/233) ([newhoggy](https://github.com/newhoggy))
- Upgrade to `amazonka-2` [\#232](https://github.com/haskell-works/cabal-cache/pull/232) ([newhoggy](https://github.com/newhoggy))
- Upgrade to haskell/actions/setup@v2 [\#229](https://github.com/haskell-works/cabal-cache/pull/229) ([newhoggy](https://github.com/newhoggy))
- Upgrade to haskell/actions/setup@v2 [\#228](https://github.com/haskell-works/cabal-cache/pull/228) ([newhoggy](https://github.com/newhoggy))
- Fix set-output warnings in CI [\#225](https://github.com/haskell-works/cabal-cache/pull/225) ([newhoggy](https://github.com/newhoggy))
- Ignore packages cli option [\#224](https://github.com/haskell-works/cabal-cache/pull/224) ([newhoggy](https://github.com/newhoggy))
- Upgrade oops [\#223](https://github.com/haskell-works/cabal-cache/pull/223) ([newhoggy](https://github.com/newhoggy))
- Split AppError type [\#220](https://github.com/haskell-works/cabal-cache/pull/220) ([newhoggy](https://github.com/newhoggy))
- Move S3 functions to own module.  Drop antiope dependency [\#219](https://github.com/haskell-works/cabal-cache/pull/219) ([newhoggy](https://github.com/newhoggy))
- New cp command for debugging purposes [\#218](https://github.com/haskell-works/cabal-cache/pull/218) ([newhoggy](https://github.com/newhoggy))
- Remove Github Actions environment [\#217](https://github.com/haskell-works/cabal-cache/pull/217) ([newhoggy](https://github.com/newhoggy))
- Break apart app error [\#216](https://github.com/haskell-works/cabal-cache/pull/216) ([newhoggy](https://github.com/newhoggy))
- Remove project.sh [\#215](https://github.com/haskell-works/cabal-cache/pull/215) ([newhoggy](https://github.com/newhoggy))
- Remove tasks.json from git tracking [\#214](https://github.com/haskell-works/cabal-cache/pull/214) ([newhoggy](https://github.com/newhoggy))
- Use oops for error handling [\#213](https://github.com/haskell-works/cabal-cache/pull/213) ([newhoggy](https://github.com/newhoggy))
- Update copyright [\#212](https://github.com/haskell-works/cabal-cache/pull/212) ([newhoggy](https://github.com/newhoggy))
- Delete unused code [\#209](https://github.com/haskell-works/cabal-cache/pull/209) ([newhoggy](https://github.com/newhoggy))
- Tidy up cabal file [\#208](https://github.com/haskell-works/cabal-cache/pull/208) ([newhoggy](https://github.com/newhoggy))
- Fix sync-to-archive [\#206](https://github.com/haskell-works/cabal-cache/pull/206) ([hasufell](https://github.com/hasufell))

## [v1.0.6.1](https://github.com/haskell-works/cabal-cache/tree/v1.0.6.1) (2023-02-04)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.6.0...v1.0.6.1)

## [v1.0.6.0](https://github.com/haskell-works/cabal-cache/tree/v1.0.6.0) (2023-02-04)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.5.5...v1.0.6.0)

**Closed issues:**

- Collaboration [\#211](https://github.com/haskell-works/cabal-cache/issues/211)

## [v1.0.5.5](https://github.com/haskell-works/cabal-cache/tree/v1.0.5.5) (2022-12-30)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.5.4...v1.0.5.5)

**Closed issues:**

- cabal-cache exits with "thread blocked indefinitely" [\#76](https://github.com/haskell-works/cabal-cache/issues/76)

**Merged pull requests:**

- Restrictive retry [\#205](https://github.com/haskell-works/cabal-cache/pull/205) ([hasufell](https://github.com/hasufell))

## [v1.0.5.4](https://github.com/haskell-works/cabal-cache/tree/v1.0.5.4) (2022-12-21)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.5.2...v1.0.5.4)

## [v1.0.5.2](https://github.com/haskell-works/cabal-cache/tree/v1.0.5.2) (2022-12-21)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.5.1...v1.0.5.2)

**Closed issues:**

- Should cabal-cache retry failed downloads/uploads? [\#191](https://github.com/haskell-works/cabal-cache/issues/191)
- Trying to use root of the S3 bucket results in `/` key. [\#152](https://github.com/haskell-works/cabal-cache/issues/152)

**Merged pull requests:**

- Catch log and rethrow exceptions during download. [\#198](https://github.com/haskell-works/cabal-cache/pull/198) ([newhoggy](https://github.com/newhoggy))
- Try to fix STM bug wrt \#76 [\#196](https://github.com/haskell-works/cabal-cache/pull/196) ([newhoggy](https://github.com/newhoggy))
- Fix versioned compiler detection on windows [\#195](https://github.com/haskell-works/cabal-cache/pull/195) ([hasufell](https://github.com/hasufell))
- More robust store dir detection [\#194](https://github.com/haskell-works/cabal-cache/pull/194) ([hasufell](https://github.com/hasufell))
- Retry s3 upload/download, fixes \#191 [\#193](https://github.com/haskell-works/cabal-cache/pull/193) ([hasufell](https://github.com/hasufell))
- Bump upper bounds [\#185](https://github.com/haskell-works/cabal-cache/pull/185) ([newhoggy](https://github.com/newhoggy))
- Fix typos [\#184](https://github.com/haskell-works/cabal-cache/pull/184) ([newhoggy](https://github.com/newhoggy))
- New CI to sync with Backblaze s3 provider [\#183](https://github.com/haskell-works/cabal-cache/pull/183) ([newhoggy](https://github.com/newhoggy))
- Remove unused function anchor [\#106](https://github.com/haskell-works/cabal-cache/pull/106) ([newhoggy](https://github.com/newhoggy))

## [v1.0.5.1](https://github.com/haskell-works/cabal-cache/tree/v1.0.5.1) (2022-12-01)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.5.0...v1.0.5.1)

**Merged pull requests:**

- Fix bug with build-path [\#182](https://github.com/haskell-works/cabal-cache/pull/182) ([newhoggy](https://github.com/newhoggy))

## [v1.0.5.0](https://github.com/haskell-works/cabal-cache/tree/v1.0.5.0) (2022-12-01)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.4.0...v1.0.5.0)

**Merged pull requests:**

- New --path CLI option [\#181](https://github.com/haskell-works/cabal-cache/pull/181) ([newhoggy](https://github.com/newhoggy))
- More concrete types [\#178](https://github.com/haskell-works/cabal-cache/pull/178) ([newhoggy](https://github.com/newhoggy))
- Replace boolean [\#177](https://github.com/haskell-works/cabal-cache/pull/177) ([newhoggy](https://github.com/newhoggy))
- Update dependencies [\#175](https://github.com/haskell-works/cabal-cache/pull/175) ([newhoggy](https://github.com/newhoggy))
- Remove failing tests [\#172](https://github.com/haskell-works/cabal-cache/pull/172) ([newhoggy](https://github.com/newhoggy))
- Switch to use cabal-cache-s3 [\#170](https://github.com/haskell-works/cabal-cache/pull/170) ([newhoggy](https://github.com/newhoggy))
- Remove CircleCI support [\#146](https://github.com/haskell-works/cabal-cache/pull/146) ([newhoggy](https://github.com/newhoggy))

## [v1.0.4.0](https://github.com/haskell-works/cabal-cache/tree/v1.0.4.0) (2022-03-15)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.3.0...v1.0.4.0)

**Closed issues:**

- support multicloud [\#162](https://github.com/haskell-works/cabal-cache/issues/162)

**Merged pull requests:**

- Add support for https [\#169](https://github.com/haskell-works/cabal-cache/pull/169) ([newhoggy](https://github.com/newhoggy))
- Fix syncing from archive with multicloud [\#168](https://github.com/haskell-works/cabal-cache/pull/168) ([newhoggy](https://github.com/newhoggy))
- Fix syncing from archive with multicloud [\#167](https://github.com/haskell-works/cabal-cache/pull/167) ([hasufell](https://github.com/hasufell))
- Support multicloud, fixes \#162 [\#165](https://github.com/haskell-works/cabal-cache/pull/165) ([newhoggy](https://github.com/newhoggy))
- Support multicloud, fixes \#162 [\#163](https://github.com/haskell-works/cabal-cache/pull/163) ([hasufell](https://github.com/hasufell))
- Upgrade to ghc-8.10.7 and ghc-9.0.1 [\#161](https://github.com/haskell-works/cabal-cache/pull/161) ([newhoggy](https://github.com/newhoggy))
- Support ghc-8.10.4 [\#160](https://github.com/haskell-works/cabal-cache/pull/160) ([newhoggy](https://github.com/newhoggy))
- Fix warnings [\#159](https://github.com/haskell-works/cabal-cache/pull/159) ([newhoggy](https://github.com/newhoggy))
- Unify URI types [\#157](https://github.com/haskell-works/cabal-cache/pull/157) ([newhoggy](https://github.com/newhoggy))

## [v1.0.3.0](https://github.com/haskell-works/cabal-cache/tree/v1.0.3.0) (2021-03-14)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.2.2...v1.0.3.0)

**Merged pull requests:**

- New plan command [\#156](https://github.com/haskell-works/cabal-cache/pull/156) ([newhoggy](https://github.com/newhoggy))
- Avoid set-env in Github Actions [\#154](https://github.com/haskell-works/cabal-cache/pull/154) ([newhoggy](https://github.com/newhoggy))
- Publish releases [\#150](https://github.com/haskell-works/cabal-cache/pull/150) ([newhoggy](https://github.com/newhoggy))
- Fix warnings [\#148](https://github.com/haskell-works/cabal-cache/pull/148) ([newhoggy](https://github.com/newhoggy))
- Tweak caching [\#145](https://github.com/haskell-works/cabal-cache/pull/145) ([newhoggy](https://github.com/newhoggy))
- Remove unnecessary dependencies [\#142](https://github.com/haskell-works/cabal-cache/pull/142) ([newhoggy](https://github.com/newhoggy))

## [v1.0.2.2](https://github.com/haskell-works/cabal-cache/tree/v1.0.2.2) (2020-10-25)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.2.1...v1.0.2.2)

## [v1.0.2.1](https://github.com/haskell-works/cabal-cache/tree/v1.0.2.1) (2020-09-29)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.2.0...v1.0.2.1)

**Merged pull requests:**

- Fix slash handling on Windows [\#141](https://github.com/haskell-works/cabal-cache/pull/141) ([newhoggy](https://github.com/newhoggy))

## [v1.0.2.0](https://github.com/haskell-works/cabal-cache/tree/v1.0.2.0) (2020-09-29)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.1.9...v1.0.2.0)

**Merged pull requests:**

- Fix macos builds [\#138](https://github.com/haskell-works/cabal-cache/pull/138) ([newhoggy](https://github.com/newhoggy))
- Add --build-path [\#137](https://github.com/haskell-works/cabal-cache/pull/137) ([newhoggy](https://github.com/newhoggy))
- Use cabal cache [\#136](https://github.com/haskell-works/cabal-cache/pull/136) ([newhoggy](https://github.com/newhoggy))

## [v1.0.1.9](https://github.com/haskell-works/cabal-cache/tree/v1.0.1.9) (2020-09-19)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.1.8...v1.0.1.9)

**Closed issues:**

- Windows Builds [\#129](https://github.com/haskell-works/cabal-cache/issues/129)

**Merged pull requests:**

- Upgrade macos executor [\#134](https://github.com/haskell-works/cabal-cache/pull/134) ([newhoggy](https://github.com/newhoggy))
- Implement GitHub actions [\#133](https://github.com/haskell-works/cabal-cache/pull/133) ([newhoggy](https://github.com/newhoggy))
- Implement Github Actions [\#132](https://github.com/haskell-works/cabal-cache/pull/132) ([hazelweakly](https://github.com/hazelweakly))
- Win32 support [\#130](https://github.com/haskell-works/cabal-cache/pull/130) ([newhoggy](https://github.com/newhoggy))
- Upgrade to haskell-build@4.1.8 [\#128](https://github.com/haskell-works/cabal-cache/pull/128) ([newhoggy](https://github.com/newhoggy))
- Upgrade to optparse-applicative-0.16 [\#127](https://github.com/haskell-works/cabal-cache/pull/127) ([newhoggy](https://github.com/newhoggy))
- Fix macos builds [\#125](https://github.com/haskell-works/cabal-cache/pull/125) ([newhoggy](https://github.com/newhoggy))
- Use official cache http end-point [\#124](https://github.com/haskell-works/cabal-cache/pull/124) ([newhoggy](https://github.com/newhoggy))
- Upgrade to orb hackage@1.4.2 [\#123](https://github.com/haskell-works/cabal-cache/pull/123) ([newhoggy](https://github.com/newhoggy))
- Fix build [\#122](https://github.com/haskell-works/cabal-cache/pull/122) ([newhoggy](https://github.com/newhoggy))
- Fix hlint [\#121](https://github.com/haskell-works/cabal-cache/pull/121) ([newhoggy](https://github.com/newhoggy))
- Upgrade generic-lens [\#120](https://github.com/haskell-works/cabal-cache/pull/120) ([newhoggy](https://github.com/newhoggy))
- Disable parallel garbage colllector in CCI tests [\#119](https://github.com/haskell-works/cabal-cache/pull/119) ([newhoggy](https://github.com/newhoggy))
- Remove unused imports [\#118](https://github.com/haskell-works/cabal-cache/pull/118) ([newhoggy](https://github.com/newhoggy))
- Upgrade to hackage@1.4.1 [\#117](https://github.com/haskell-works/cabal-cache/pull/117) ([newhoggy](https://github.com/newhoggy))
- Upgrade to github-release@1.3.3 [\#116](https://github.com/haskell-works/cabal-cache/pull/116) ([newhoggy](https://github.com/newhoggy))
- Upgrade to haskell-build-4.1.7 [\#115](https://github.com/haskell-works/cabal-cache/pull/115) ([newhoggy](https://github.com/newhoggy))

## [v1.0.1.8](https://github.com/haskell-works/cabal-cache/tree/v1.0.1.8) (2020-03-24)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.1.7...v1.0.1.8)

**Merged pull requests:**

- Use token [\#113](https://github.com/haskell-works/cabal-cache/pull/113) ([newhoggy](https://github.com/newhoggy))
- Convert i386 build to use binary cache [\#112](https://github.com/haskell-works/cabal-cache/pull/112) ([newhoggy](https://github.com/newhoggy))

## [v1.0.1.7](https://github.com/haskell-works/cabal-cache/tree/v1.0.1.7) (2020-03-24)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.1.6...v1.0.1.7)

## [v1.0.1.6](https://github.com/haskell-works/cabal-cache/tree/v1.0.1.6) (2020-03-24)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.1.5.a...v1.0.1.6)

**Merged pull requests:**

- i386 build [\#111](https://github.com/haskell-works/cabal-cache/pull/111) ([newhoggy](https://github.com/newhoggy))

## [v1.0.1.5.a](https://github.com/haskell-works/cabal-cache/tree/v1.0.1.5.a) (2020-01-25)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.1.5...v1.0.1.5.a)

## [v1.0.1.5](https://github.com/haskell-works/cabal-cache/tree/v1.0.1.5) (2020-01-25)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.1.4...v1.0.1.5)

**Merged pull requests:**

- Upgrade to haskell-build@4.0.6 [\#105](https://github.com/haskell-works/cabal-cache/pull/105) ([newhoggy](https://github.com/newhoggy))
- Upgrade to haskell-build@4.0.6 [\#104](https://github.com/haskell-works/cabal-cache/pull/104) ([newhoggy](https://github.com/newhoggy))

## [v1.0.1.4](https://github.com/haskell-works/cabal-cache/tree/v1.0.1.4) (2020-01-25)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.1.3...v1.0.1.4)

**Closed issues:**

- ghc-pkg invocation doesn't honour current compiler [\#94](https://github.com/haskell-works/cabal-cache/issues/94)

**Merged pull requests:**

- Fix macos build [\#103](https://github.com/haskell-works/cabal-cache/pull/103) ([newhoggy](https://github.com/newhoggy))
- Upgrade to ghc-8.8.2 in CI [\#102](https://github.com/haskell-works/cabal-cache/pull/102) ([newhoggy](https://github.com/newhoggy))
- Support multiple archive-uris [\#101](https://github.com/haskell-works/cabal-cache/pull/101) ([newhoggy](https://github.com/newhoggy))
- Jky azure [\#100](https://github.com/haskell-works/cabal-cache/pull/100) ([newhoggy](https://github.com/newhoggy))
- Fix development build files [\#99](https://github.com/haskell-works/cabal-cache/pull/99) ([newhoggy](https://github.com/newhoggy))
- Tidy up cabal file [\#98](https://github.com/haskell-works/cabal-cache/pull/98) ([newhoggy](https://github.com/newhoggy))

## [v1.0.1.3](https://github.com/haskell-works/cabal-cache/tree/v1.0.1.3) (2019-12-06)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.1.2...v1.0.1.3)

**Merged pull requests:**

- Use same ghc-pkg version as specified by GHC version in plan.json [\#97](https://github.com/haskell-works/cabal-cache/pull/97) ([newhoggy](https://github.com/newhoggy))
- Move command parsers to command modules [\#96](https://github.com/haskell-works/cabal-cache/pull/96) ([newhoggy](https://github.com/newhoggy))
- Direct test output and generate environment files [\#93](https://github.com/haskell-works/cabal-cache/pull/93) ([newhoggy](https://github.com/newhoggy))

## [v1.0.1.2](https://github.com/haskell-works/cabal-cache/tree/v1.0.1.2) (2019-10-26)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.1.1...v1.0.1.2)

**Closed issues:**

- Allow use of non-AWS clouds [\#83](https://github.com/haskell-works/cabal-cache/issues/83)

**Merged pull requests:**

- Additional brew update to avoid ruby syntax issues [\#91](https://github.com/haskell-works/cabal-cache/pull/91) ([newhoggy](https://github.com/newhoggy))
- Additional brew update to avoid ruby syntax issues [\#90](https://github.com/haskell-works/cabal-cache/pull/90) ([newhoggy](https://github.com/newhoggy))
- CI for ghc-8.8.1 [\#88](https://github.com/haskell-works/cabal-cache/pull/88) ([newhoggy](https://github.com/newhoggy))
- Fix for ghc-8.8.1 [\#87](https://github.com/haskell-works/cabal-cache/pull/87) ([newhoggy](https://github.com/newhoggy))
- Upgrade generic-lens version [\#86](https://github.com/haskell-works/cabal-cache/pull/86) ([newhoggy](https://github.com/newhoggy))
- Bump upper-bound of optparse-applicative [\#84](https://github.com/haskell-works/cabal-cache/pull/84) ([newhoggy](https://github.com/newhoggy))
- Upgrade to haskell-build-4.0.2 [\#82](https://github.com/haskell-works/cabal-cache/pull/82) ([newhoggy](https://github.com/newhoggy))
- Upgrade haskell build orb version [\#81](https://github.com/haskell-works/cabal-cache/pull/81) ([newhoggy](https://github.com/newhoggy))
- Upgrade haskell-build orb version [\#78](https://github.com/haskell-works/cabal-cache/pull/78) ([newhoggy](https://github.com/newhoggy))

## [v1.0.1.1](https://github.com/haskell-works/cabal-cache/tree/v1.0.1.1) (2019-07-21)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.1.0...v1.0.1.1)

**Closed issues:**

- renamePath errors [\#73](https://github.com/haskell-works/cabal-cache/issues/73)
- Doesn't work [\#72](https://github.com/haskell-works/cabal-cache/issues/72)

**Merged pull requests:**

- Fix getLibFiles if directory does not exist wrt \#72 [\#75](https://github.com/haskell-works/cabal-cache/pull/75) ([hasufell](https://github.com/hasufell))
- Fix renamePath errors wrt \#73 [\#74](https://github.com/haskell-works/cabal-cache/pull/74) ([hasufell](https://github.com/hasufell))

## [v1.0.1.0](https://github.com/haskell-works/cabal-cache/tree/v1.0.1.0) (2019-07-03)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.0.12...v1.0.1.0)

**Merged pull requests:**

- use ubuntu:16.04 for max compatibility [\#71](https://github.com/haskell-works/cabal-cache/pull/71) ([dsturnbull](https://github.com/dsturnbull))

## [v1.0.0.12](https://github.com/haskell-works/cabal-cache/tree/v1.0.0.12) (2019-05-29)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.0.11...v1.0.0.12)

**Merged pull requests:**

- Check for target file existence [\#69](https://github.com/haskell-works/cabal-cache/pull/69) ([AlexeyRaga](https://github.com/AlexeyRaga))

## [v1.0.0.11](https://github.com/haskell-works/cabal-cache/tree/v1.0.0.11) (2019-05-29)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.0.10...v1.0.0.11)

**Merged pull requests:**

- Upgrade archive to v2 [\#68](https://github.com/haskell-works/cabal-cache/pull/68) ([newhoggy](https://github.com/newhoggy))
- Append metadata instead of prepend [\#67](https://github.com/haskell-works/cabal-cache/pull/67) ([newhoggy](https://github.com/newhoggy))
- Use relation package instead [\#66](https://github.com/haskell-works/cabal-cache/pull/66) ([newhoggy](https://github.com/newhoggy))

## [v1.0.0.10](https://github.com/haskell-works/cabal-cache/tree/v1.0.0.10) (2019-05-12)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.0.9...v1.0.0.10)

**Merged pull requests:**

- Smaller footprint [\#65](https://github.com/haskell-works/cabal-cache/pull/65) ([AlexeyRaga](https://github.com/AlexeyRaga))
- Upgrade to haskell-build@2.0.2 [\#64](https://github.com/haskell-works/cabal-cache/pull/64) ([newhoggy](https://github.com/newhoggy))
- Upgrade to haskell-build@2.0.1 [\#63](https://github.com/haskell-works/cabal-cache/pull/63) ([newhoggy](https://github.com/newhoggy))
- Build OSX binaries [\#62](https://github.com/haskell-works/cabal-cache/pull/62) ([AlexeyRaga](https://github.com/AlexeyRaga))

## [v1.0.0.9](https://github.com/haskell-works/cabal-cache/tree/v1.0.0.9) (2019-05-12)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.0.8...v1.0.0.9)

**Merged pull requests:**

- Add missing cases in display app error [\#61](https://github.com/haskell-works/cabal-cache/pull/61) ([newhoggy](https://github.com/newhoggy))
- Use latest cabal-cache again [\#60](https://github.com/haskell-works/cabal-cache/pull/60) ([newhoggy](https://github.com/newhoggy))

## [v1.0.0.8](https://github.com/haskell-works/cabal-cache/tree/v1.0.0.8) (2019-05-12)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.0.7...v1.0.0.8)

**Merged pull requests:**

- Do not fail in cleanup [\#59](https://github.com/haskell-works/cabal-cache/pull/59) ([newhoggy](https://github.com/newhoggy))
- Enable read for binary cache in forks [\#58](https://github.com/haskell-works/cabal-cache/pull/58) ([newhoggy](https://github.com/newhoggy))

## [v1.0.0.7](https://github.com/haskell-works/cabal-cache/tree/v1.0.0.7) (2019-05-11)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.0.6...v1.0.0.7)

**Merged pull requests:**

- Reduce number of queries in download [\#57](https://github.com/haskell-works/cabal-cache/pull/57) ([newhoggy](https://github.com/newhoggy))
- Remove duplicate download [\#56](https://github.com/haskell-works/cabal-cache/pull/56) ([newhoggy](https://github.com/newhoggy))
- New readFirstAvailableResource function [\#55](https://github.com/haskell-works/cabal-cache/pull/55) ([newhoggy](https://github.com/newhoggy))
- Use Either instead of Maybe during downloads [\#54](https://github.com/haskell-works/cabal-cache/pull/54) ([newhoggy](https://github.com/newhoggy))

## [v1.0.0.6](https://github.com/haskell-works/cabal-cache/tree/v1.0.0.6) (2019-05-11)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.0.5...v1.0.0.6)

**Merged pull requests:**

- Upgrade to haskell-works/haskell-build@2.0.0 [\#53](https://github.com/haskell-works/cabal-cache/pull/53) ([newhoggy](https://github.com/newhoggy))
- Upgrade to haskell-build-2@1.6.18 [\#52](https://github.com/haskell-works/cabal-cache/pull/52) ([newhoggy](https://github.com/newhoggy))

## [v1.0.0.5](https://github.com/haskell-works/cabal-cache/tree/v1.0.0.5) (2019-05-10)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.0.4...v1.0.0.5)

## [v1.0.0.4](https://github.com/haskell-works/cabal-cache/tree/v1.0.0.4) (2019-05-10)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.0.3...v1.0.0.4)

**Merged pull requests:**

- HTTP support [\#50](https://github.com/haskell-works/cabal-cache/pull/50) ([newhoggy](https://github.com/newhoggy))
- Do not fail if no access to bucket [\#49](https://github.com/haskell-works/cabal-cache/pull/49) ([newhoggy](https://github.com/newhoggy))

## [v1.0.0.3](https://github.com/haskell-works/cabal-cache/tree/v1.0.0.3) (2019-05-07)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.0.2...v1.0.0.3)

**Merged pull requests:**

- Implement safe download [\#47](https://github.com/haskell-works/cabal-cache/pull/47) ([newhoggy](https://github.com/newhoggy))

## [v1.0.0.2](https://github.com/haskell-works/cabal-cache/tree/v1.0.0.2) (2019-05-03)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.0.1...v1.0.0.2)

**Merged pull requests:**

- AWS logging [\#45](https://github.com/haskell-works/cabal-cache/pull/45) ([newhoggy](https://github.com/newhoggy))
- Log AWS errors [\#43](https://github.com/haskell-works/cabal-cache/pull/43) ([newhoggy](https://github.com/newhoggy))
- Download queue [\#42](https://github.com/haskell-works/cabal-cache/pull/42) ([newhoggy](https://github.com/newhoggy))
- New Relation type [\#41](https://github.com/haskell-works/cabal-cache/pull/41) ([newhoggy](https://github.com/newhoggy))
- Fix test module names [\#40](https://github.com/haskell-works/cabal-cache/pull/40) ([newhoggy](https://github.com/newhoggy))
- Add depends field to Package type [\#39](https://github.com/haskell-works/cabal-cache/pull/39) ([newhoggy](https://github.com/newhoggy))
- Rename modules from Ci.Assist to CabalCache [\#38](https://github.com/haskell-works/cabal-cache/pull/38) ([newhoggy](https://github.com/newhoggy))
- New stm dependency [\#37](https://github.com/haskell-works/cabal-cache/pull/37) ([newhoggy](https://github.com/newhoggy))

## [v1.0.0.1](https://github.com/haskell-works/cabal-cache/tree/v1.0.0.1) (2019-04-30)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v1.0.0.0...v1.0.0.1)

**Merged pull requests:**

- Retry copy resource [\#36](https://github.com/haskell-works/cabal-cache/pull/36) ([newhoggy](https://github.com/newhoggy))

## [v1.0.0.0](https://github.com/haskell-works/cabal-cache/tree/v1.0.0.0) (2019-04-26)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v0.2.0.2...v1.0.0.0)

**Merged pull requests:**

- Add metadata [\#34](https://github.com/haskell-works/cabal-cache/pull/34) ([AlexeyRaga](https://github.com/AlexeyRaga))
- Restore from matching store-path-hash preferentially [\#33](https://github.com/haskell-works/cabal-cache/pull/33) ([newhoggy](https://github.com/newhoggy))
- Hashed store path in archived packages [\#32](https://github.com/haskell-works/cabal-cache/pull/32) ([newhoggy](https://github.com/newhoggy))
- DRY versioning handling [\#31](https://github.com/haskell-works/cabal-cache/pull/31) ([newhoggy](https://github.com/newhoggy))
- Add versioning to archive [\#30](https://github.com/haskell-works/cabal-cache/pull/30) ([newhoggy](https://github.com/newhoggy))
- Upgrade to haskell-build-2@1.6.7 [\#28](https://github.com/haskell-works/cabal-cache/pull/28) ([newhoggy](https://github.com/newhoggy))
- Typo in readme [\#27](https://github.com/haskell-works/cabal-cache/pull/27) ([ekmett](https://github.com/ekmett))

## [v0.2.0.2](https://github.com/haskell-works/cabal-cache/tree/v0.2.0.2) (2019-04-25)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v0.2.0.1...v0.2.0.2)

## [v0.2.0.1](https://github.com/haskell-works/cabal-cache/tree/v0.2.0.1) (2019-04-20)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v0.2.0.0...v0.2.0.1)

## [v0.2.0.0](https://github.com/haskell-works/cabal-cache/tree/v0.2.0.0) (2019-04-20)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v0.1.0.9...v0.2.0.0)

**Merged pull requests:**

- Create archive using tar cli [\#25](https://github.com/haskell-works/cabal-cache/pull/25) ([newhoggy](https://github.com/newhoggy))
- Introduce new Presence and Tagged datatypes to convey richer informat… [\#21](https://github.com/haskell-works/cabal-cache/pull/21) ([newhoggy](https://github.com/newhoggy))

## [v0.1.0.9](https://github.com/haskell-works/cabal-cache/tree/v0.1.0.9) (2019-04-20)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v0.1.0.8...v0.1.0.9)

## [v0.1.0.8](https://github.com/haskell-works/cabal-cache/tree/v0.1.0.8) (2019-04-19)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v0.1.0.7...v0.1.0.8)

## [v0.1.0.7](https://github.com/haskell-works/cabal-cache/tree/v0.1.0.7) (2019-04-19)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v0.1.0.5...v0.1.0.7)

## [v0.1.0.5](https://github.com/haskell-works/cabal-cache/tree/v0.1.0.5) (2019-04-15)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v0.1.0.4...v0.1.0.5)

## [v0.1.0.4](https://github.com/haskell-works/cabal-cache/tree/v0.1.0.4) (2019-04-15)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v0.1.0.3...v0.1.0.4)

**Merged pull requests:**

- Make all paths platform-independent, some cleanup [\#19](https://github.com/haskell-works/cabal-cache/pull/19) ([AlexeyRaga](https://github.com/AlexeyRaga))
- Create missing directories [\#18](https://github.com/haskell-works/cabal-cache/pull/18) ([newhoggy](https://github.com/newhoggy))
- Cleanup paths [\#17](https://github.com/haskell-works/cabal-cache/pull/17) ([newhoggy](https://github.com/newhoggy))
- More logging [\#16](https://github.com/haskell-works/cabal-cache/pull/16) ([newhoggy](https://github.com/newhoggy))

## [v0.1.0.3](https://github.com/haskell-works/cabal-cache/tree/v0.1.0.3) (2019-04-13)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v0.1.0.2...v0.1.0.3)

**Merged pull requests:**

- Auto create Package DB in sync-from-archive command [\#15](https://github.com/haskell-works/cabal-cache/pull/15) ([newhoggy](https://github.com/newhoggy))

## [v0.1.0.2](https://github.com/haskell-works/cabal-cache/tree/v0.1.0.2) (2019-04-13)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v0.1.0.1...v0.1.0.2)

**Merged pull requests:**

- Auto create Package DB [\#14](https://github.com/haskell-works/cabal-cache/pull/14) ([newhoggy](https://github.com/newhoggy))

## [v0.1.0.1](https://github.com/haskell-works/cabal-cache/tree/v0.1.0.1) (2019-04-12)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/v0.1.0.0...v0.1.0.1)

**Merged pull requests:**

- Add region options [\#12](https://github.com/haskell-works/cabal-cache/pull/12) ([newhoggy](https://github.com/newhoggy))
- Threadsafe logging [\#11](https://github.com/haskell-works/cabal-cache/pull/11) ([newhoggy](https://github.com/newhoggy))
- Add threads [\#10](https://github.com/haskell-works/cabal-cache/pull/10) ([newhoggy](https://github.com/newhoggy))
- Recache package db after sync from archive [\#9](https://github.com/haskell-works/cabal-cache/pull/9) ([newhoggy](https://github.com/newhoggy))
- Archive dylibs [\#8](https://github.com/haskell-works/cabal-cache/pull/8) ([newhoggy](https://github.com/newhoggy))
- Static link [\#7](https://github.com/haskell-works/cabal-cache/pull/7) ([AlexeyRaga](https://github.com/AlexeyRaga))
- Cleanup tar entries [\#6](https://github.com/haskell-works/cabal-cache/pull/6) ([AlexeyRaga](https://github.com/AlexeyRaga))
- Pass more package information [\#5](https://github.com/haskell-works/cabal-cache/pull/5) ([newhoggy](https://github.com/newhoggy))
- Fix pred [\#4](https://github.com/haskell-works/cabal-cache/pull/4) ([AlexeyRaga](https://github.com/AlexeyRaga))
- Template conf [\#3](https://github.com/haskell-works/cabal-cache/pull/3) ([AlexeyRaga](https://github.com/AlexeyRaga))
- Make conf files optional [\#2](https://github.com/haskell-works/cabal-cache/pull/2) ([newhoggy](https://github.com/newhoggy))
- Include conf [\#1](https://github.com/haskell-works/cabal-cache/pull/1) ([AlexeyRaga](https://github.com/AlexeyRaga))

## [v0.1.0.0](https://github.com/haskell-works/cabal-cache/tree/v0.1.0.0) (2019-04-12)

[Full Changelog](https://github.com/haskell-works/cabal-cache/compare/faf6083ca5b4d084f1e2e41d22db6a6a33b78a75...v0.1.0.0)



\* *This Changelog was automatically generated by [github_changelog_generator](https://github.com/github-changelog-generator/github-changelog-generator)*

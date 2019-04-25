# cabal-cache
[![master](https://circleci.com/gh/haskell-works/cabal-cache/tree/master.svg?style=svg)](https://circleci.com/gh/haskell-works/cabal-cache/tree/master)

Tool for caching built cabal new-build packages.

The tool is useful in development when you want to share your build haskell package dependencies of
of a particular project with another developer and also in CI where caching is useful for reducing
build times.

`cabal-cache` supports syncing to an archive directory or to an S3 bucket.

## Installation

Several installation methods are available.

### From source

```bash
git new-install cabal-cache
```

### Using Homebrew on Mac OS X

```bash
brew tap haskell-works/homebrew-haskell-works git@github.com:haskell-works/homebrew-haskell-works.git
brew update
brew install cabal-cache
```

## Example usage

Syncing built packages with S3 requires you have an S3 bucket with AWS
credentials stored in the `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` environent variables.
You should also know the AWS region the bucket was created in.

### Sync to S3

Change into your project directory.

Build the project with `cabal v2-build`.  This will ensure your dependencies are built and
will product a `plan.json` file that is required for the `cabal-cache` tool to know which built
packages to sync up.

Run the following command.

```bash
hw-ci-assist sync-from-archive --threads 16 --archive-uri s3://my-cabal-cache-bucket/archive --region Sydney
```

### Sync from S3

Change into your project directory.

Build the project with `cabal v2-configure`.  This will product a `plan.json` file that is required
for the `cabal-cache` tool to know which built packages to sync down.

Run the following command.

```bash
hw-ci-assist sync-from-archive --threads 16 --archive-uri s3://my-cabal-cache-bucket/archive --region Sydney
```

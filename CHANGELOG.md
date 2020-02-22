# Unreleased

- Remove support for Haxl-style automatic parallelisation
  * Remove strategy parameter from `runTask`
  * Add `MonadBaseControl`, which allows manual parallelisation using e.g. lifted-async
  * Remove `Sequential` type
- Use `IORef`s instead of `MVar`s
- Add `trackM` function
- Remove `invalidateReverseDependencies` in favour of `reachableReverseDependencies`
- Generalise `verifyTraces` to verify using user-supplied data

# 0.2.0.0

- Stop using hashes when verifying traces (gets rid of the `Rock.HashTag` and `Rock.Hashed` modules)
- Add reverse dependency tracking

# 0.1.0.1

- Fix base-4.12 compatibility

# 0.1.0.0

- Initial release

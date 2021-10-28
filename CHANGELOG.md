# Change Log
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased] 2015-11-21
### Added
- Introduce new functions `ponos:add_load_generator/{3,4}` for adding a
  single load generator with the required arguments as part of the
  function signature.

## [1.5.2] 2017-10-10
### Changed
- Fix stats for low CPS generators.


## [1.5.1] 2017-03-23
### Changed
- Fixed type specs for `start/2` callback.

## [1.5.0] 2015-11-02
### Added
- Add accessors for `max_concurrent` option which allows for dynamic
  configuration.

## [1.4.0] - 2015-09-07
### Added
- Add the `max_concurrent` option to the type spec of load generator
  options.
- Extend the `ponos_task_runner_callbacks` behaviour with a new
  callback for reporting hitting the maximum concurrency limit.

### Changed
- Full history in change log.
- Improved scheduling of tasks:
    - A bug has been fixed that caused a load generator never to be
      able to raise its intensity after reporting 0 intensity.
    - The load generator process can catch up under high load from
      scheduling delays.

## [1.3.0] - 2014-11-20
### Added
- Introduce a `max_concurrent` option for load generators limiting how
many concurrently ongoing tasks the load generators may spawn.
- README.md, add links for resources.

## [1.2.2] - 2014-11-17
### Added
- README.md, add historical context for the name *ponos*.

## [1.2.1] - 2014-11-13
### Added
- License information in test suites.

### Changed
- Rename internal modules.

### Removed
- Remove release script.

## [1.2.0] - 2014-11-12
### Added
- `ptop:pp/0` exported.

## [1.1.0] - 2014-11-11
### Changed
- Allow operations on single load generators in `ponos.erl`.

## [1.0.0] - 2014-11-07
- Initial release.

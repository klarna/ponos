# Change Log
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [1.4.0] - 2015-09-07
### Added
- Add the `max_concurrent` option to the type spec of load generator
  options.
- Extend the `ponos_task_runner_callbacks` behaviour with a new
  callback for reporting hitting the maximum concurrency limit.

### Changed
- Full history in change log.
- Improved scheduling of tasks:
    - A bug has beend fixed that caused a load generator never to be
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

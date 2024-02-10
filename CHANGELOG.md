# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- Global minor mode for running commands.
- Running watched tasks on file save.
- Show command output on failure, hide it on success.
- Commands for creating, deleting, editing, watching and unwatching of
  commands.
- Mode-line lighter for buffers in projects with commands.
- Lighter highlights when command is running.
- Load and save of project commands.
- Automatic loading of commands in projects on discovery.
- Configurable positioning of the output buffer.
- Trigger commands when enabled interactively, unless called with
  prefix argument.
- Deletes command buffer when disabling tasks.
- Annotate command completions with watch state and arguments.

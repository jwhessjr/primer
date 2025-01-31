# Developing Primer

## With Nix

We use [Nix flakes](https://nixos.wiki/wiki/Flakes) to develop Primer,
so most of our developer documentation assumes you've got a working
Nix installation with flakes support enabled, and that you're running
any given commands in the project's `nix develop` shell.

Our flake supports both `x86_64-linux` and `aarch64-darwin` systems.
Adding support for other common architectures should be
straightforward, but we don't currently have the CI resources to
support them. Both `x86_64-darwin` and `aarch64-linux` have been known
to work in the past. Please let us know if you'd like to contribute,
but your development platform of choice is not included in our flake.

## Without Nix

You *can* choose to forego Nix and build the provided Cabal projects
without it, assuming you have supported versions of GHC and Cabal in
your `PATH`. However, in order to run some of Primer's tests, you'll
need additional bespoke tools that are provided automatically by the
`nix develop` shell. These tests will likely fail if they're run
locally but not from within the Nix shell.

Additionally, the scripts we use for out-of-band [database
operations](database.md), such as database creation and migration, may
require specific versions of third-party tools like
[Sqitch](https://sqitch.org), and must be configured with
project-specific settings and schemas in any case, all of which is
handled automatically by Nix.

We'll make a best-effort attempt to ensure that the most common
development workflows are supported without Nix, or at least
documented, but please note that we don't have the maintainer
resources to provide much support if you run into problems in non-Nix
environments.

## On Windows

We've never attempted to build or run this implementation of Primer on
any version of Windows, and we don't have any plans to support it. You
may have some success with [NixOS on
WSL](https://github.com/nix-community/NixOS-WSL), but we don't have
the resources to provide any support for this environment.

## `Makefile` targets

For interactive development workflows, both with and without Nix, we
provide some convenient `Makefile` targets from the repository's top
level directory:

* `make` runs `cabal configure` followed by `cabal build` across all
  projects.

* `make test` runs `cabal test` across all projects.

* `make bench` runs `cabal bench` across all projects.

Because running the API server for local development involves a few
different moving parts, we don't provide `make` targets for running
the service, and instead recommend that you use `nix run` for this
purpose, as described below.

## Local development with SQLite

Running the API server against a local SQLite database is
straightforward, so long as you have a working Nix flakes setup. Just
run the following command from the repo's top-level directory:

```sh
nix run .#run-primer-sqlite
```

By default, this command will:

1. Deploy a SQLite database named `primer.sqlite3` in the current
   working directory. It will create a new empty database if one
   doesn't already exist.
2. Run `primer-service` on your host machine and configure it to
   listen on TCP port `8081` on all network interfaces.

This command uses the same script to launch the service as our
production Docker container uses, and therefore, in typical Docker
entrypoint style, it takes no command-line arguments. Instead, you can
override the server's default configuration by setting any of the
following environment variables (shown along with their default
values):

| Environment variable |      Purpose                                          |  Default         |
|----------------------|-------------------------------------------------------|------------------|
| `SQLITE_DB`          | Filesystem path to the SQLite database                | `primer.sqlite3` |
| `SERVICE_PORT`       | TCP port on which the service listens for connections | `8081`           |

Note that the script will actually create *two* database files: one
named `primer.sqlite3`, and the other named `sqitch.sqlite3`. The
latter contains metadata required by
[Sqitch](https://sqitch.org/docs/manual/sqitchtutorial-sqlite/), which
we use to manage [database schemas](database.md).

If you want to start over with a new database, either set `SQLITE_DB`
to a new path, or remove both the `primer.sqlite3` and
`sqitch.sqlite3` database files.

If you don't want to use the `run-primer-sqlite` command for some
reason, you can also run `primer-service` directly via a Nix flake
app. For usage, run:

```sh
nix run .#primer-service serve -- --help
```

However, in this scenario, you'll need to have first created and/or
migrated the local SQLite database by running `primer-sqitch`
yourself. `primer-service` doesn't know how to deploy or migrate the
database on its own. See the
[`primer-service-entrypoint.sh`](../nix/pkgs/scripts/primer-service-entrypoint.sh)
shell script for the commands that the Docker entrypoint runs.

## Local development with PostgreSQL

In addition to SQLite, `primer-service` also supports PostgreSQL for
its program store. Though we recommend using SQLite for local
development due to its much simpler setup, we do provide a collection
of Nix-based tooling to make local PostgreSQL development as easy as
we can. See the [PostgreSQL development documentation](postgresql.md)
for details.

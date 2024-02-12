# Instructions

While I used nix flakes via [`devenv`](https://devenv.sh/) to setup HLS, but the project can be built with plain `cabal`.

## Using `engineering-test`

### Build

```sh
$ cabal v2-build
```

### Run Executor

```sh
$ cabal v2-run engineering-test -- --limit 5
```

#### Executor Options

```sh
$ cabal v2-run engineering-test -- --help
Executor Program

Usage: engineering-test --limit INT

Available options:
  -h,--help                Show this help text
  --limit INT              Max number of concurrent tasks per executor
```

### REPL

```sh
cabal v2-repl
```

## Initial Project Setup

Recently I’ve been trying out `devenv` for quickly spinning up simple projects.

Per [Using With Flakes](https://devenv.sh/guides/using-with-flakes/#getting-started), I ran the following (which ran for several minutes):

```sh
# Init flake repo and setup nix-direnv shell-switching
$ nix flake init --template github:cachix/devenv
$ echo .devenv >> .gitignore
$ echo .direnv >> .gitignore
```

Per [devenv.nix](https://devenv.sh/reference/options/), I choose a GHC compiler version and enabled HLS.

```nix
languages.haskell.enable = true;
languages.haskell = {
	package = pkgs.ghc;
	languageServer = pkgs.haskell-language-server;
	stack = null;  # use cabal by default
};
```

Then, I initialized a haskell project template with `cabal``.

```sh
$ cabal init --interactive
...
$ echo dist-newstyle >> .gitignore
```

Finally, I enabled `nix-direnv` for automatically loading development shell.

```sh
# Setup nix-direnv shell-switching
direnv allow
```

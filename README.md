# IDE and Tooling Research Platform

There is nothing here yet - this is not even alpha level yet. There is only documentation on how to use it by myself.

---

Electron needs to be installed, a pre-built binary can be found [here](https://github.com/atom/electron/releases).

[Neovim](https://github.com/neovim/neovim) needs to be installed too. It can be done by executing the `make_neovim` script.

Important sbt commands:

- `backend/reStart` - Launches server application. After that the `run_electron` executable can be run.
- `;scalacPlugin/clean;scalacPlugin/test` - Runs the scalac plugin.
- `firefoxPlugin/genFirefoxPlugin` - Generates the firefox plugin. It can be tested with `run_firefox_plugin`.


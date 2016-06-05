# IDE and Tooling Research Platform

There is nothing here yet - this is not even alpha level yet. There is only documentation on how to use it by myself.

---

Setup notes:

- Electron needs to be installed, a pre-built binary can be found [here](https://github.com/atom/electron/releases).

- [Neovim](https://github.com/neovim/neovim) needs to be installed too. It can be done by executing the `make_neovim` script.

- The URL `amora.central` should redirect to localhost. Add the following entry to `/etc/hosts`:

  ```
  127.0.0.1       amora.central
  ```

  This way, we can use URLs to `amora.central` in our code and don't need to worry about Internet loookups.

Important sbt commands:

- `backend/reStart` - Launches server application. After that the `run_electron` executable can be run.
- `;scalacPlugin/assembly;scalacPlugin/test` - Runs the scalac plugin.
- `firefoxPlugin/genFirefoxPlugin` - Generates the firefox plugin. It can be tested with `run_firefox_plugin`.


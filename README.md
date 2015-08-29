# IDE and tooling research platform

There is nothing here yet - this is not even alpha level yet. There is only documentation on how to use it by myself.

---

Electron needs to be installed, a pre-built binary can be found [here](https://github.com/atom/electron/releases).

[Neovim](https://github.com/neovim/neovim) needs to be installed too. It needs to be started with `NVIM_LISTEN_ADDRESS=127.0.0.1:6666 nvim`.

Furthermore, the following package installations were required on my system:

- nodejs
- phantomjs

The application can be launched by starting sbt and typing `backend/preStart`. After that the `run` executable can be run.


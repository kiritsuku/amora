# Amora

[![Join the chat at https://gitter.im/sschaef/amora](https://badges.gitter.im/sschaef/amora.svg)](https://gitter.im/sschaef/amora?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Amora is a semantic search engine where anyone can define anything anywhere - a technical prowess that is achieved by relying on semantic web technologies. Everything in Amora is [Linked Data](https://en.wikipedia.org/wiki/Linked_data) or in other words a distributed key-value store where every key-value pair is associated with an unique identifier in form of an URI. Amora was created to provide an IDE and tooling research platform whose ambitious goal it is to supersede existing tools by a new being.

So far, there is nothing here yet - this is not even alpha level yet. There is only documentation on how to use it by myself.

---

Setup notes:

- Electron needs to be installed, a pre-built binary can be found [here](https://github.com/atom/electron/releases).

- [Neovim](https://github.com/neovim/neovim) needs to be installed too. It can be done by executing the `make_neovim` script.

- The URL `amora.center` should redirect to localhost. Add the following entry to `/etc/hosts`:

  ```
  127.0.0.1       amora.center
  ```

  This way, we can use URLs to `amora.center` in our code and don't need to worry about Internet loookups. Furthermore, we also want to specify a port redirect from port 80 to the port where the server is listening to. This can be achieved by running the script `./configure_ip_tables` as root.

Important sbt commands:

- `backend/reStart` - Launches server application. After that the `run_electron` executable can be run.
- `;scalacPlugin/assembly;scalacPlugin/test` - Runs the scalac plugin.
- `firefoxPlugin/genFirefoxPlugin` - Generates the firefox plugin. It can be tested with `run_firefox_plugin`.


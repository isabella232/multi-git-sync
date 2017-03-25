# multi-git-sync

[![Circle CI](https://circleci.com/gh/jml/multi-git-sync/tree/master.svg?style=shield)](https://circleci.com/gh/jml/multi-git-sync/tree/master)

Sidecar to keep multiple git repositories in sync

## What it is

Synchronises multiple git repositories to an area on local disk, based on
configuration in a YAML file.

## Why you might want it

Excellent question!

## How to use it

Create a configuration YAML file like this:

```yaml
root: /var/lib/data/git-repositories
interval: 30  # pull every 30 seconds
repos:
  dotfiles:
    url: git@github.com:jml/dotfiles.git
  emacs-configuration:
    url: git@github.com:jml/emacs-configuration.git
```

And store it in a path like `/etc/multi-git-sync.yaml` (the exact path doesn't
matter).

### Natively

Build and install the code with `stack install` and then run with:

    multi-git-sync --port 8080 --config-file=/etc/multi-git-sync.yaml --ghc-metrics +RTS -N -T -RTS

This will start a server that you can reach at http://localhost:8080/

You can browse to that server to see the status of the Git repositories that
have been pulled.

### With Docker

[untested]

Create a Docker image with:

    make image

The last line of successful `make` output will be the name of the image, e.g.
`multi-git-sync:master-1a2b3cd`.

You can then run the image like so:

    docker run -p 8080:80 multi-git-sync:master-1a2b3cd --port 80 +RTS -N

And you can reach the server at http://localhost:8080/ if you are running
Docker natively. If you're on a Mac and
using [Docker Machine](https://docs.docker.com/machine/), you can run:

    open http://$(docker-machine ip):8080/

To browse to the running server.

## TODO

### Service

- [ ] Serve the synced repos as static web pages
- [ ] Include last time synced in repo information
- [ ] Include last time config change in config information
- [ ] Serve current system time in config information

### Git

- [ ] If repo sync successfully but working tree fails, retry working tree creation
- [ ] Support handling URL change
- [ ] Make sure Git commands are async exception safe

### Users

- [ ] Confirm the README instructions for Docker

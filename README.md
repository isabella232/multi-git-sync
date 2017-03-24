# multi-git-sync

[![Circle CI](https://circleci.com/gh/jml/multi-git-sync/tree/master.svg?style=shield)](https://circleci.com/gh/jml/multi-git-sync/tree/master)

Sidecar to keep multiple git repositories in sync

## What it is

## Why you might want it

## How to use it

### Natively

Build and install the code with `stack install` and then run with:

    multi-git-sync --port 8080 +RTS -N

This will start a server that you can reach at http://localhost:8080/

### With Docker

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

- [ ] Retry sync on exception
- [ ] If repo sync successfully but working tree fails, retry working tree creation
- [ ] Serve the synced repos as static web pages
- [ ] Include last time synced in repo information
- [ ] Include last time config change in config information
- [ ] Serve current system time in config information
- [ ] Support handling URL change
- [ ] Confirm the README instructions

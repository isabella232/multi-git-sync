# multi-git-sync

[![Circle CI](https://circleci.com/gh/jml/multi-git-sync/tree/master.svg?style=shield)](https://circleci.com/gh/jml/multi-git-sync/tree/master)

Sidecar to keep multiple git repositories in sync

Contains:

* multi-git-sync-api -- API definition for multi-git-sync
* multi-git-sync-server -- Server implementation of the multi-git-sync API

## What it is

## Why you might want it

## How to use it

### Natively

Build and install the code with `stack install` and then run with:

    multi-git-sync --port 8080

This will start a server that you can reach at http://localhost:8080/

### With Docker

Create a Docker image with:

    make image

The last line of successful `make` output will be the name of the image, e.g.
`multi-git-sync:master-1a2b3cd`.

You can then run the image like so:

    docker run -p 8080:80 multi-git-sync:master-1a2b3cd --port 80

And you can reach the server at http://localhost:8080/ if you are running
Docker natively. If you're on a Mac and
using [Docker Machine](https://docs.docker.com/machine/), you can run:

    open http://$(docker-machine ip):8080/

To browse to the running server.

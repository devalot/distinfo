# Example distributed-process + snap application

A peer-based distributed application written in Haskell with an
[embedded web server][snap].

This example application is different than a traditional
[distributed-process][] application because it doesn't use a
master/slave configuration.  Instead, all peers are equal and notify
one another of their existence.

## Features

Not many.  It's intentionally very minimal.

  * After starting, the application will discover peers on the local
    network and request from each a reply containing basic information
    (hostname, OS, etc.).

  * When a node receives a request for information it records the
    existence of the requesting peer in a mutable map.  It also begins
    monitoring the peer for failure.

  * If a node dies or terminates peers will remove the deceased node
    from their node maps.

  * Nodes that were started with the `-w` flag include a web server on
    port 8000 (use `-W` to use another port.)  Browsing to that port
    on localhost will show an automatically updating list of peers.

## Limitations

  * When running nodes on different machines you must use the `-H`
    flag to set your public IP address so other peers can see you.  It
    would be nice if this was automatically set.

  * On the other hand, when running more than one node on the *same*
    machine you need to use the `-n` flag to specify a port number so
    that each node has a unique port.

  * I've stripped out all use of Template Haskell from this
    application, including the remotable table.  Therefore you can't
    spawn processes on remote nodes.

## Building and Installing

  1. Install the [Haskell Platform][].

  2. Install [cabal-dev][]:

        $ cabal update
        $ cabal install cabal-dev

  3. Build `distinfo`:

        $ cd distinfo
        $ cabal-dev install

  4. Play with it:

        $ cabal-dev/bin/distinfo --help

[snap]: http://snapframework.com/
[distributed-process]: http://haskell-distributed.github.io/
[haskell platform]: http://www.haskell.org/platform/
[cabal-dev]: http://hackage.haskell.org/package/cabal-dev

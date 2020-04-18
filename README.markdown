[![Hackage](https://img.shields.io/hackage/v/co-log-concurrent.svg)](https://hackage.haskell.org/package/co-log-concurrent)
[![Stackage LTS](http://stackage.org/package/co-log-concurrent/badge/lts)](http://stackage.org/lts/package/co-log-concurrent)
[![Stackage Nightly](http://stackage.org/package/co-log-concurrent/badge/nightly)](http://stackage.org/nightly/package/co-log-concurrent)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/qnikst/co-log-concurrent/blob/master/LICENSE)
[![Build status](https://secure.travis-ci.org/qnikst/co-log-concurrent.svg)](https://travis-ci.org/qnikst/co-log-concurrent)

# co-log-concurrent

`co-log-concurrent` is an asynchronous backend for the [co-log] library. It may be useful
for many applications. The core idea of co-log-concurrent is that you may easily make your
logs asynchronous by adding few lines of code to the program that is using `co-log`. However
this library is designed in the way that it can be injected into complex logs pipelines,
so if there are some additional requirements that program has this library doesn't prevent
that. 

## When do you need co-log-concurrent?

## How to use?

### Simple case

### Advanced usage

## Other implementations.

Comparison with other libraries may be outdated, as other libraries are improving and may
have solved the issued described below.

 * [fast-logger]() — fast logger implements concurrent approach to storing logs, it's very
   effective. Basically it has a buffer for each capability that each thread can append
   messages to the buffer without locking, however that approach comes at some costs, in
   pathological cases if thread is rescheduled to another capability logs from that thread
   may be reordered. Even if a case is pathological that had happened in real world applications.
   Current implementation guarantees no logs reordering by the cost of using `stm` channels.
   Another scenario that may be a downside of using the library is that it tooks over timer
   control it uses [auto-update]() package that updates timer every second, in order to 
   reduce syscall count, it means that precision of the timer is 1s, that may not be suitable
   for some applications. `co-log-concurrent` is abstracted over the timer, so user can use
   any approach that suits it well, including not to give any timestamp to the message at all.

 * [katip]() - katip uses the approach that is very similar to `co-log-concurrent` and has
   the same runtime properties. However `co-log-concurrent` allows to use any type of messages,
   timers and doesn't restrict user to use particular ones. It makes co-log-concurrent more flexible.


[co-log]()


# concurrent-phasers

This package is a Haskell implementation of the "Phaser," a barrier-like 
synchronization object. Its construction and behavior is inspired
by the object of the same name that is part of the 
[Habanero Project](https://habenero.rice.edu).

# What's a Phaser?

The `Phaser` is basically a generalized [barrier](https://en.wikipedia.org/wiki/Barrier_(computer_science)).
It differs in a few respects:

1. A `Phaser` is re-usable, and can block as many times as is needed.

2. A `Phaser` has a notion of some phase (for this package, any `Enum`), which is advanced every time all 
parties/threads leave the `Phaser`.

3. In a barrier, all parties that arrive on the barrier block until all remaining parties arrive. 
In `Phaser` terms, these threads both `Signal` (their arrival is necessary for the barrier to advance) and `Wait` (block until the barrier does advance). However, with a `Phaser`, a thread can use the `Phaser` in a `Signal`-ling
capacity, a `Wait`ing capacity, or both (`SignalWait`). This means you can have threads which contribute towards unblocking
others, even if they are not blocked by the phaser... or threads that do not contribute towards unblocking
the phaser, but will still be blocked by it.

# Usage

## Modules

This package includes both an `MVar`-based implementation (`IOPhaser`) and a `STM`-based implementation (`STMPhaser`). The `IOPhaser` is used by default when you import `Control.Concurrent.Phaser`, but `Control.Concurrent.Phaser.STM` has the alternate `STMPhaser` option.

## Using a Phaser

### Creation

A `Phaser` is declared with a 'phase' and initial number of threads that will use it:
```haskell
ph <- newIOPhaser 0 4 -- Starting with phase 0, 4 parties.
```

The number of parties can be dynamically adjusted with the `register` and `unregister` functions.

### Synchronization

Synchronizing work on a `Phaser` is fairly straightforward -- in each thread, state which phasers
you want to synchronize with, what your relationship (Signal/Wait) is with them, and what you want to do:

```haskell
-- Single phaser, Signal/Wait mode:

runPhased ph SignalWait $ do
  ... something useful ...

-- Or even with multiple Phasers
runMultiPhased [(ph1, Signal), (ph2, Wait)] $ do
  ... something useful...
```

Any barrier-like blocking occurs _after_ the the action specified on the phaser has been run.

In the case of `runMultiPhased`, we may signal or wait at phasers in an order 
different than the one specified in order to avoid deadlock.

# vs. Java Phasers

The `Phaser` type is semantically different from, but not entirely unlike, Java's
[java.util.concurrent.Phaser](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/Phaser.html) class.


# Future Features

Some features which are currently unimplemented, but may eventually be added:

* Asynchronous exception safety

* Tree-structuring of Phasers to eliminate livelock for large numbers of
processes



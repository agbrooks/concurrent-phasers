# concurrent-phasers

## What's this?

This package is a Haskell implementation of the "Phaser," a barrier-like, 
point-to-point synchronization object. Its construction and behavior is inspired
by the object of the same name that is part of the 
[Habanero Project](https://habenero.rice.edu).

The `Phaser` type is semantically different from, but not entirely unlike, Java's
[java.util.concurrent.Phaser](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/Phaser.html) class.

## Future Features

* Asynchronous exception safety

* Tree-structuring of Phasers to eliminate livelock for large numbers of
processes



# F18A Emulator

This is a simple F18A emulator. It can run a code on a single core and has some untested support for simulating all 144 cores and the communication between them. It does not emulate any of the chip's IO facilities, however.

It was originally written to implement a superoptimizer based on [Stochastic Superoptimization][1]; however, it's useful as a standalone program as well.

[1]: http://cs.stanford.edu/people/eschkufz/research/asplos291-schkufza.pdf

## Installation

`array-forth` is now up on [Hackage](http://hackage.haskell.org/package/array-forth). You can just install it with:

    cabal install array-forth
    
This creates two executables: `array-forth`, which runs the interpreter interactively and `mcmc-demo` that runs an example using the superoptimizer. 

If you want to try optimizing your own programs, you will either have to modify `Main.hs` or write your own program using `Language.ArrayForth.Synthesis`. This is a bit of a pain, but it also allows you to play around with different scoring functions and mutations. If anyone's actually interested, I'd be happy to extract this into a usable executable, but right now it's too slow to be terribly useful. (That said, it should scale well to multiple machines, so if you have a nice cluster...)

If anyone has any thoughts about speeding up the synthesizer, I would *really* love to hear them!

## Features

### Emulator

The emulator has two distinct parts: a library which makes it easy to work with F18A programs and a frontend that includes a nice REPL for playing around with F18A code.

You can get to the REPL just by running `array-forth`. 

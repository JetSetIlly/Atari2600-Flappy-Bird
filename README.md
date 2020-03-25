# Atari2600 - Flappy Bird Clone

This is a very rudimentary implementation of flappy bird for the Atari 2600. It
represents the first work I did with the console and shows inexperience with
the hardware. But it's fun and plays quite well.

## Requirements

The game was developed with DASM and expects the presence of the "standard"
vcs.h and macro.h files during assembly.

## Reflection

I was trying to get more work out of the TIA by using flickering effects but
it's not successful in all places. The background "trees" in particular aren't
very appealing, mainly I think, because they are implemented with playfield
graphics. I like the tree "foliage" however and the swamp water at the bottom
of the screen. The splash and sinking bird death-animation is particularly
effective I think.

I was also overly ambitious with the construction of the code. The header files
in the project are an attempt to abstract away non-game code but it's probably
more confusing than anything else. I would probably construct the code
differently if I was to write it again.


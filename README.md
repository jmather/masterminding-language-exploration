What you're missing if you're only doing PHP
============================================

Code from the talk

Now before you start, yes, this is not a full mastermind implementation. The nuances would be fun to add,
however I wanted demoing the game running to go quickly under pressure, so the rules are a little simpler.

Assumptions the game makes that aren't in the rules:
- The game master will always only use one peg of the same color.
- The game master's analysis of your guess will always be in the same order as your guess.
- We're offering, in general, more guesses and less variety in pegs, than is typically given.

The game: Mastermind
--------------------

Simple game, only has a few rules:

1. At the start, the game selects 4 of 6 colors (Red, White, Blue, Green, Yellow and Tan) to use.
2. Game places 4 colors in a specific order in a hidden area.
3. Player has 10 chances to guess the color and order. After each guess, the player:
  a. receives an X for pegs which are the correct color and position
  b. receives an * for pegs which are the correct color but wrong position
  c. receives a _ for pegs which meet neither condition

What you're missing if you're only doing PHP
============================================

Code from the talk

The game: Mastermind
--------------------

Simple game, only has a few rules:

1. At the start, the game selects 4 of 6 colors (Red, White, Blue, Green, Yellow and Tan) to use.
2. Game places 4 colors in a specific order in a hidden area.
3. Player has 10 chances to guess the color and order. After each guess, the player:
  a. receives an X for pegs which are the correct color and position
  b. receives an * for pegs which are the correct color but wrong position
  c. receives a _ for pegs which meet neither condition

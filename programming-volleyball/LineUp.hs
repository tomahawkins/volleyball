module LineUp where

{-

One challenge I have as a coach is to quickly produce a lineup for
each set.  It's just six numbers, but producing those numbers and
in the right order, takes considerable mental effort, especially with
the noise of the crowd and the two minute time limit imposed by the game.

It would be nice if we could just list the players by name
in their positions and specify the starting rotation and then
automatically get the lineup numbers.
Let's write a Haskell program to do just that!

We could specify player's names as string, e.g. "Abby", but the problem
is if we mistype a name or if we reference a player who is not
on the roster, the Haskell type checker won't catch the error.
Therefore it is better to create a new datatype specifically
for our players.  That way if we mess up, the type checker will
catch the problem.

We call the new datatype 'Player' and then list off all the players
on JV.  The players are possible values for the datatype 'Player'.
'Abby', 'Abi', 'Bailey', etc., these are called constructors in
Haskell jargon.

-}

data Player
  = Abby
  | Abi
  | Bailey
  | Gwen
  | Indie
  | Josiah
  | Layla
  | Merritt
  | Prudence
  | Sadie
  | Sofia
  | Stephanie
  | Wren
  deriving (Show)

{-

With this as a starting point, we open up the Haskell interpreter (ghci),
load this file, and start doing things with it.  This is what it would look like:

    Prelude> :load LineUp.hs
    [1 of 1] Compiling LineUp           ( LineUp.hs, interpreted )
    Ok, one module loaded.

Whenever we make a change to the LineUp.hs file, we can reload it
with the :reload command (or just :r):

    *LineUp> :reload
    [1 of 1] Compiling LineUp           ( LineUp.hs, interpreted )
    Ok, one module loaded.

One thing we can do is type a player on the prompt to see what happens.
Haskell reads the prompt, evaluates it, and then prints the result.
In this case it just prints exactly what we just typed:

    *LineUp> Prudence
    Prudence

But notice what happens if we misspell Prudence's name:

    *LineUp> Prudince

    <interactive>:7:1: error:
        • Data constructor not in scope: Prudince
        • Perhaps you meant ‘Prudence’ (line 36)

Not only did Haskell see the error, it made a suggestion on how to fix it.
That's pretty smart!

There's lots of useful things you can do in the interpreter
(type :help to see a full list), but one thing is to check the type
of an expression using :type, or just :t.  See what happens if
we check the type on 'Prudence':

    *LineUp> :type Prudence
    Prudence :: Player

It tells us that 'Prudence' is a 'Player'.  The '::' symbol in Haskell
is a way to specify the type of a value.  We can use this on the prompt:

    *LineUp> Prudence :: Player
    Prudence

But look what happens if we say the Prudence is some other type:

    *LineUp> Prudence :: Integer

    <interactive>:12:1: error:
        • Couldn't match expected type ‘Integer’ with actual type ‘Player’
        • In the expression: Prudence :: Integer
          In an equation for ‘it’: it = Prudence :: Integer

Haskell catches the type problem and gives some hints on how to fix it.

I've talked a lot with Prudence and one thing I known for sure is
she is not an Integer.  Apparently Haskell is smart enough to
know this too.

Moving along, it would be really nice to know the players numbers.
Or rather given a player, to get their number automatically.
We do exactly this by writing a function, which we call 'rosterNumber':

-}

rosterNumber :: Player -> Int
rosterNumber player = case player of
  Abby -> 3
  Abi -> 4
  Bailey -> 6
  Gwen -> 13
  Indie -> 2
  Josiah -> 1
  Layla -> 12
  Merritt -> 10
  Prudence -> 5
  Sadie -> 9
  Sofia -> 7
  Stephanie -> 8
  Wren -> 11

{-

Note the type on 'rosterNumber':

    rosterNumber :: Player -> Int

The '->' symbol means function type, which takes two arguments:
the input type of the function, in this case 'Player', and the output type of the function, 'Int'.
An 'Int' is just a common integer.  And the interpreter confirms this for us:

    *LineUp> :type rosterNumber
    rosterNumber :: Player -> Int

What can we do with a function?  We can call it.  Meaning
we can pass a value into it and it will return a result.
For 'rosterNumber' this means passing in a 'Player'
and getting that player's number.  In Haskell, we call functions
by referencing the function's name followed by the argument for the function
(the thing we want to pass into it).
We can do this on the interpreter prompt:

    *LineUp> rosterNumber Bailey
    6

Yep!  Bailey is number 6!

    *LineUp> :type rosterNumber
    rosterNumber :: Player -> Int

An interesting thing about Haskell, and any functional programming language,
is that functions are treated as regular values.  This means
functions themselves can be arguments to other functions.
Likewise, functions can return functions as results.

For example, 'map' is a function that takes a function as an input argument along
with a list of values.
It applies this function across the list of values producing a new list of values.
That might sound complicated, but it is very useful.  Let's say
we have a list of players and we want a corresponding list of their roster numbers.
We can use 'map':

    *LineUp> map rosterNumber [Merritt, Sadie, Wren, Indie, Gwen]
    [10,9,11,2,13]

This should give us some clues as how to proceed with the design of our program.
At some point we just need to arrive at a list of players for the lineup
serve order, then 'map' and 'rosterNumber' will do the rest.

The type of 'map' shows the following:

    *LineUp> :type map
    map :: (a -> b) -> [a] -> [b]

This says the first argument to 'map' (a -> b) is a function that goes from type 'a' to type 'b',
the second argument [a] is a list of 'a', and it returns [b] which is list of 'b', where
'a' and 'b' can be any types.

-}

lineup :: Int -> Player -> Player -> Player -> Player -> Player -> Player -> [Int]
lineup startingRotation setter oh1 mb2 rs oh2 mb1 = map rosterNumber serveOrder
  where
    serveSequenceFromSetter :: [Player]
    serveSequenceFromSetter = concat (repeat [setter, oh1, mb2, rs, oh2, mb1])
    serveOrder = take 6 (drop (startingRotation - 1) elaborated)

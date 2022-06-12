module Vertical
  ( main
  ) where

{-

--------------------------------------------------------------------------------
Volleyball, Trigonometry, and the Value of Increasing Our Vertical Jump
--------------------------------------------------------------------------------

Hi Team!  I do a lot of software development for work as well as for fun.
I use a functional programming language called Haskell.  For those of you
interested in functional programming, I'm always happy to talk about it.

Today I want to use Haskell to reason about a volleyball question,
specifically: How does vertical jump improve attack capabilities?

With hard work, each of us can increase our vertical by at least a foot over
the course of the season.  But that will take work and effort.  Competitive
teams are efficient teams, even when it comes to practice.  We need to
understand where best to put are efforts to yield the biggest returns,
i.e. game wins.  The question is will working on our vertical yield
tangible results?  Is 12 inches of additional reach really that beneficial?

This question comes down to a problem in trigonometry.  Given a vertical,
the height at which we can hit the ball projects a partial view of the
opposing court, the rest is obscured by the net.  This area of court
available to us is called the "attack surface".  The greater the area of
the attack surface, the more opportunities we have to score.

Excuse my lack of ascii art ability, but I'll try to draw a picture of
what I mean:



    hitting height

        *
           .
              .
              |  .
              |     .
       net    |        .
              |           .
              |              .    flight path
              .                 .
              .                    .
              .                       .
              .                          .
              .                             .
              .                                .    attack angle
              .                                 / .
              .                                |     .
    floor  ---+-----------------------------------------=============
                    area of court blocked by net        attack surface

              |................. opposing court ....................|



Now the same court but with a higher hitting height and steeper attack angle:

    higher hitting height

        *
          .
            . 
              .
              | .
              |   .
       net    |     .
              |       .
              |         .    steeper flight path
              .           .
              .             .
              .               .
              .                 .
              .                   .
              .                     .    greater attack angle
              .                     / .
              .                    |    .
    floor  ---+---------------------------===========================
                                             greater attack surface

              |................. opposing court ....................|


Now let's start building a program to compute the attack surface
given a hitting location.

First we need the court and net dimensions:
-}


{-
One side of a volleyball court is 9 x 9 m  or 29.5 x 29.5 ft.  We will stay
with stick with imperial units.  In Haskell, a "::" denotes the type
of values.  Here we are saying "courtWidth" is a "Double", which is a
double precision floating point number.
-}
courtWidth :: Double
courtWidth = 29.5


{-
Each side court is square, so the length is the same as the width.
-}
courtLength :: Double
courtLength = courtWidth


{-
The official height of a women's net is 7' 4" and 1/8".
We will convert inches and fractional inches into decimal feet in one step:
-}
netHeight :: Double
netHeight = 7 + 4 / 12 + 1 / 8 / 12


{-
Now we specify how far the ball is set off the net.  Though a bit tight, a
ball set 1 foot off the net can be a lot of fun, especially with no
opposing blockers.
-}
setOffNet :: Double
setOffNet = 1


{-
Let's pick our initial ball hitting height (vertical).  I think the average
reach for our team today is 3" above the net.  We will use that as a
starting point.
-}
initialHittingHeight :: Double
initialHittingHeight = netHeight + 3 / 12


{-
And our 1 foot improved hitting height after we put in the work.
-}
improvedHittingHeight :: Double
improvedHittingHeight = initialHittingHeight + 1


{-
Now we need a function to compute attack angle given a ball hitting height.
Here's where trigonometry comes into play.
For those who have taken trig, you'll remember that:


      |\
      |  \
      |    \
      |      \
   y  |        \
      |          \
      |            \
      |           a  \
      |________________\
              x

    tan(a) = y/x

To solve for angle 'a', we take the atan of both sides:

    a = atan(y/x)

For our problem, 'y' is the ball hitting height minus the net height,
and 'x' is the distance the ball is set off the net.

So here is a function that returns the attack angle for a given hitting height.
Functions in Haskell are things that compute a new value from another.
Here we have a function that takes a "Double" as an input and returns a
"Double" as a result, note the "Double -> Double" syntax to specify the 
function type for "attackAngle":
-}
attackAngle :: Double -> Double
attackAngle hittingHeight = atan (y / x)
 where
  x = setOffNet
  y = hittingHeight - netHeight


{-
Given the attack angle we can compute the length of the court obscured by
the net.  This is another tangent problem: this time 'x' is the length of
court obscured by the net, 'y' is the net height, and 'a' is the attack angle.


    tan(a) = y/x

To solve for 'x', we multiply both sides by 'x' and divide
both sides by 'tan(a)':

    x = y / tan(a)

We will call this function "lengthObscured":
-}
lengthObscured :: Double -> Double
lengthObscured attackAngle' = netHeight / tan attackAngle'


{-
With everything else in place we can now write a single function to compute
attack surface given a hitting height.  We get the attack angle from the
hitting height, the length obscured is computed from the attack angle, and
the attack surface is the (court length - the length obscured) times the
court width.
-}
attackSurface :: Double -> Double
attackSurface hittingHeight =
  (courtLength - lengthObscured (attackAngle hittingHeight)) * courtWidth


{-
Finally, compute the attack surface for initial and improved
hitting heights and divide for total improvement.
Print out the results.
-}
main :: IO ()
main = do
  print
    $ attackSurface improvedHittingHeight
    / attackSurface initialHittingHeight


{-
Run the program:

    $ runhaskell Vertical.hs
    189.0
 
WOW!  Improving our vertical by 12" increases our attack surface by 189 TIMES!!!

YES!  Let's put in the effort!!!
-}



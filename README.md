haste-deck [![Build Status](https://travis-ci.org/valderman/haste-deck.svg?branch=master)](https://travis-ci.org/valderman/haste-deck.svg?branch=master)
==========

Slideshow/layout library for use with the
[Haste compiler](http://haste-lang.org).
Create presentations and slideshows and embed them into web applications, or
embed web applications into your presentations and slideshows.

Usage
-----

The library builds on the concepts of *slides* and *decks*.
A slide is a list of elements placed according to some layout, and a deck is a
list of slides compiled into the DOM elements, CSS, and executable code
required to display a slideshow.

A simple example of a slide would be the Hello World slide.
Note that `OverloadedStrings` is used to create text slides from string
literals; purists may want to avoid this and instead use the `markup` function
from `Haste.Deck`.

    {-# LANGUAGE OverloadedStrings #-}
    import Haste.DOM (appendChild, documentBody)
    import Haste.Deck

    one :: Slide
    one = "Hello, world!"

We can add some formatting to make it more interesting:

    two :: Slide
    two = centered . color "blue" . fontSize (Pt 48) $ one

We can also place the slide in relation to other elements:

    three :: Slide
    three = two `above` image "hello.jpg"

Add some Markdown (or rather, a subset of Markdown) to spice it up further:

    four :: Slide
    four = "This text is to the *left*!" `leftOf` three

Since we're all the way at the end of our presentation, maybe the presenter
wants an easy way to skip back to a previous slide? We can do this by using
a hyperlink to the slide we want to jump to. Of course, we could jump forward
as well.

    five :: Slide
    five = "Go back to [slide three](#3)!"

Sometimes, we don't want all sub-slides to take up equal space. For instance,
we may want to reserve 90% of the available screen space for content and use
the remaining 10% for a heading.

    six :: Slide
    six = two `above` sized 0.9 three

When we are done, we can create a deck out of our slides and display it.
In this example we're using the default configuration plus the `pan` transition
animation, but this is user configurable.

    main :: IO ()
    main = present_ def {transition = pan} [one, two, three, four, five, six]

Arrow keys, page up/down and left/right swipe gestures switch between slides;
home and end keys go to the first and the last slide respectively.

Of course, custom slide controls are available as well. Run `cabal haddock` and
see for yourself.

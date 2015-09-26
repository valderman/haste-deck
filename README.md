haste-deck
==========

Slideshow/layout library for use with the
[Haste compiler](http://haste-lang.org).

Usage
-----

The library builds on the concepts of *slides* and *decks*.
A slide is a list of elements placed according to some layout, and a deck is a
list of slides compiled into the DOM elements, CSS, and executable code
required to display a slideshow.

A simple example of a slide would be the Hello World slide.
Note that `OverloadedStrings` is used to create text slides from string
literals; purists may want to avoid this and instead use the `text` function
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

When we are done, we can create a deck out of our slides and display it.
In this example we're using the `pan` transition, but this is user configurable:

    main :: IO ()
    main = do
      deck <- createDeck pan [one, two, three, four]
      appendChild documentBody deck
      enableDeck deck

Arrow keys and page up/down switch between slides.
Custom input configuration and per slide transitions are not yet implemented.

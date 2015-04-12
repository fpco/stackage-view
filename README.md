stackage-view
=====

View Haskell codebases

## Introduction

This tool can be run in the root directory of a Cabal project which
provides a way to look around the source modules.

![Stackage-view screenshot](http://i.imgur.com/qrd9gYI.png)

On the left are modules of the project and on the right is the source
of the module.

## Starting

In a Cabal project directory, run `stackage-view` and you should see:

    $ stackage-view
    Point your browser to http://localhost:3000/

If you go to that URL in your browser, you will see:

![Target choosing screen](http://i.imgur.com/Fmj1PPZ.png)

Pick the targets you want to check out and then hit `Done`.

## Current Features

* Go to definition: You can double-click any identifier to jump
  straight to the definition.
* Type information of any expression or pattern binding. Just click
  any identifier to see its type. Alternatively, you can select an
  expression to see the type of that. Hitting the `<->` expansion
  button will expand the selection to a parent node.

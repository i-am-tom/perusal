# Perusal [![Build Status](https://travis-ci.org/i-am-tom/perusal.svg?branch=master)](https://travis-ci.org/i-am-tom/perusal)
A PureScript library for making (_very_ simple) in-browser presentations. More a teaching aid than a practically-useful project.

## How to?

We'll need to install the PureScript toolkit, then install the dependencies for this particular project, and then build the whole thing to regular JavaScript. The `-O` flag is for optimised (read: dead code-eliminated) output.

```
$ npm install -g purescript bower pulp
$ bower install
$ pulp browserify -O --to dist.js
```

## What do?

At its simplest, imagine a file like this:

```html
<div>
  <section>Hello</section>
  <section>Secondly</section>
  <section>Third of All</section>
</div>
<script src="dist.js"></script>
```

It's a file with some `section` tags in it, and a `script` link to the compiled file we just made. What does the script do? If you open this up in a browser, you'll see everything this library currently does: it hides all but the first `section` tag, and then moves that styling as you press the "left" and "right" arrow keys on your keyboard. Super simple!

## Why though?

Next up is **animation via keyframes**, when this library will require some actual input from its users. At that point, this will all look a bit more impressive. Right now, though, it's a relatively straightforward PureScript project using @paf31's fancy `purescript-behaviors` library (along with @garyb's `purescript-dom` bindings) to achieve a simple, practical goal. Isn't that neat?

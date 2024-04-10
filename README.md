# Binheap - A Common Lisp binary heap

This library is a simple implementation of a binary heap using
an adjustable array.

## Installing

There may be better ways to do this with ASDF, but this is how I
installed this locally on my system. First, I went into my lisp
system (SBCL) and did: `asdf:*central-registry*`

Since I am using Quicklsp, it said it was `/home/mark/quicklisp/quicklisp`.

So, I created a symbolic link from binheap.asd to `/home/mark/quicklisp/quicklisp`:

```shell
ln -s /home/mark/lispprogs/binheap/binheap.asd /home/mark/quicklisp/quicklisp/binheap.asd
```

Then from within SBCL I can load it with:

```lisp
(asdf:operate 'asdf:load-op :binheap)
```

## Usage

I usually do `(use-package :com.wutka.binheap)` to put the binheap
functions into my local namespace.

### make-binheap

To create a new binary heap, use `make-binheap`. For example:

```lisp
(setf myheap (make-binheap))
```

Because the binary heap uses an adjustable array, if you have an
idea how many items you might need to store, you can specify an
initial capacity:

```lisp
(setf myheap (make-binheap :initial-capacity 1000))
```

You can also use the `:comp` keyword to specify a comparison function.
The default is `#'<`. The
comparison function should take two items and return true if the
first item should be the parent of the second item. Since `#'<` is the
default comparison function, the heap defaults to having the smallest
item at the top. If you are just storing numbers and want the heap
to have the largest number on the top, you can supply `#'>` as the
comparison func:

```lisp
(setf myheap (make-binheap :comp #'>))
```

Finally, if you have a sequence of objects to store in the heap,
you can specify them with the `:initial-contents` keyword:

```lisp
(setf myheap (make-binheap :initial-contents '(5 1 4 3 2)))
```

### binheap-push

To add a new item to the heap, use `binheap-push` _heap_ _new-item_:

```lisp
(binheap-push myheap 7)
```

### binheap-pop

To pop the item off the top of the heap, use `binheap-pop` _heap_:

```lisp
(binheap-pop myheap)
```

If the heap is empty, the function returns `nil`.

### binheap-empty

The `binheap-empty` function returns true if the heap is empty.

### Example

Here is an example that creates an empty heap, populates it, and
then pops off the values:

```lisp
* (setf myheap (make-binheap))
#S(COM.WUTKA.BINHEAP::BH :TREE #() :COMP #<FUNCTION <>)
* (binheap-push myheap 7)
NIL
* (binheap-push myheap 3)
NIL
* (binheap-push myheap 12)
NIL
* (binheap-push myheap 8)
NIL
* (binheap-pop myheap)
3
* (binheap-pop myheap)
7
* (binheap-pop myheap)
8
* (binheap-pop myheap)
12
* (binheap-pop myheap)
NIL
* (binheap-empty myheap)
T
*
```

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
initial size:

```lisp
(setf myheap (make-binheap :initial-size 1000))
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

## Updating values

Sometimes you need to change the value of an item in the heap, for example, if
you are using the heap to store nodes for a shortest-path algorithm, you might
need to update the weight of a node because it is in the heap but you found
a shorter path to it.

There are different ways to approach the problem, such as allowing for a key
lookup in the heap, but instead, this library allows you to specify a function
that saves the current index of an item. When the item is inserted
or moved, the key update function is invoked with the item and its new index.
If you change the value of an item, you can call binheap-rebalance on the heap
and give the index value of the item.

For example, suppose we define a struct that we might want to change:
```lisp
(defstruct heapable-item
  (name "")
  (key -1))
```

The `key` field will contain the item's current index in the heap, and the
`name` field is the field that determines the sort order. We can define a
comparison function:
```lisp
(defun compare-heapables (a b)
  (string< (heapable-item-name a) (heapable-item-name b)))
```

And then we also define a function to set the key:
```lisp
(defun update-heapable (a n)
  (setf (heapable-item-key a) n))
```

Now, lets create a heapable-item struct named `quux`:
```lisp
CL-USER> (setf quux (make-heapable-item :name "quux"))
#S(HEAPABLE-ITEM :NAME "quux" :KEY -1)
CL-USER> 
```

Next, we'll create a heap, add a few other values to it and
then add `quux`:
```lisp
CL-USER> (setf heap (make-binheap :comp #'compare-heapables :key-update #'update-heapable))
#S(COM.WUTKA.BINHEAP::BH
   :TREE #()
   :COMP #<FUNCTION COMPARE-HEAPABLES>
   :KEY-UPDATE #<FUNCTION UPDATE-HEAPABLE>)
CL-USER> (binheap-push heap (make-heapable-item :name "foo"))
NIL
CL-USER> (binheap-push heap (make-heapable-item :name "bar"))
NIL
CL-USER> (binheap-push heap (make-heapable-item :name "baz"))
NIL
CL-USER> (binheap-push heap quux)
NIL
CL-USER> heap
#S(COM.WUTKA.BINHEAP::BH
   :TREE #(#S(HEAPABLE-ITEM :NAME "bar" :KEY 0)
           #S(HEAPABLE-ITEM :NAME "foo" :KEY 1)
           #S(HEAPABLE-ITEM :NAME "baz" :KEY 2)
           #S(HEAPABLE-ITEM :NAME "quux" :KEY 3))
   :COMP #<FUNCTION COMPARE-HEAPABLES>
   :KEY-UPDATE #<FUNCTION UPDATE-HEAPABLE>)
CL-USER> 
```

At this point, the "bar" item is at the root of the tree, with left children "foo" and "baz",
and then "quux" would be a left child of foo. Now, we want to update the name in `quux` so that
it would need to become the root, so we set the value, and then tell the heap to update at
the position given by the `key` value in `quux`:
```lisp
CL-USER> (setf (heapable-item-name quux) "argle")
"argle"
CL-USER> heap
#S(COM.WUTKA.BINHEAP::BH
   :TREE #(#S(HEAPABLE-ITEM :NAME "bar" :KEY 0)
           #S(HEAPABLE-ITEM :NAME "foo" :KEY 1)
           #S(HEAPABLE-ITEM :NAME "baz" :KEY 2)
           #S(HEAPABLE-ITEM :NAME "argle" :KEY 3))
   :COMP #<FUNCTION COMPARE-HEAPABLES>
   :KEY-UPDATE #<FUNCTION UPDATE-HEAPABLE>)
CL-USER> (binheap-rebalance-at heap (heapable-item-key quux))
NIL
CL-USER> heap
#S(COM.WUTKA.BINHEAP::BH
   :TREE #(#S(HEAPABLE-ITEM :NAME "argle" :KEY 0)
           #S(HEAPABLE-ITEM :NAME "bar" :KEY 1)
           #S(HEAPABLE-ITEM :NAME "baz" :KEY 2)
           #S(HEAPABLE-ITEM :NAME "foo" :KEY 3))
   :COMP #<FUNCTION COMPARE-HEAPABLES>
   :KEY-UPDATE #<FUNCTION UPDATE-HEAPABLE>)
CL-USER> quux
#S(HEAPABLE-ITEM :NAME "argle" :KEY 0)
CL-USER> 
```

The binary heap has been rebalanced to that the `quux` item, whose name is now
"argle" has moved to the root and its key value has been changed, as have some
of the other heap keys. When we pop the values off the heap they are in the correct
sorted order:
```lisp
CL-USER> (binheap-pop heap)
#S(HEAPABLE-ITEM :NAME "argle" :KEY 0)
CL-USER> (binheap-pop heap)
#S(HEAPABLE-ITEM :NAME "bar" :KEY 0)
CL-USER> (binheap-pop heap)
#S(HEAPABLE-ITEM :NAME "baz" :KEY 2)
CL-USER> (binheap-pop heap)
#S(HEAPABLE-ITEM :NAME "foo" :KEY 1)
CL-USER> (binheap-pop heap)
NIL
CL-USER> 
```

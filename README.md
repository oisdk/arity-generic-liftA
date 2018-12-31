[![Hackage](https://img.shields.io/hackage/v/arity-generic-liftA.svg)](http://hackage.haskell.org/package/arity-generic-liftA)

# arity-generic-liftA

There's a family of functions in
[Control.Applicative](https://hackage.haskell.org/package/base-4.11.0.0/docs/Control-Applicative.html)
which follow the pattern `liftA2`, `liftA3`, etc. Using some
tricks from Richard Eisenberg's thesis we can write them all at once. This
package does exactly that, providing a function (`lift`) which is an
arity-generic version of the `liftAn`.

```haskell
>>> lift (\x y z -> x ++ y ++ z) (Just "a") (Just "b") (Just "c")
Just "abc"
```

[Eisenberg, Richard A. “Dependent Types in Haskell: Theory and Practice.” University of Pennsylvania, 2016.](https://github.com/goldfirere/thesis/raw/master/built/thesis.pdf)

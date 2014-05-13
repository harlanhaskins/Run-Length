# Run Length

An implementation of run-length encoding in Haskell.

`runlength` will [run-length encode] a list. This is useful for lossless compression algorithms.

`express` will turn run-length encoded input into human readable text.

## Examples

```haskell
*Main> runlengthInt 223333333333333333333333244544
[(2,2),(22,3),(1,2),(2,4),(1,5),(2,4)]
*Main> expressInt 223333333333333333333333244544
"two 2s. 22 3s. one 2. two 4s. one 5. two 4s."
```

# Calculate
Need to calculate something? Here we go! This is a CLI calculator and it is planned to be improved with small new features along the way.

## Using it
It's possible to run its main interface by running main function inside the app.hs file. Doing this is easy as:
```
runhaskell app.hs -e EXPRESSION [--prefix, --postfix]
```
So, just insert your expression between double quotes and tell its notation, as we can see in some examples below:
```
runhaskell app.hs -e "50 23.2 - 10 *" --postfix
runhaskell app.hs -e "* - 50 23.2 10" --prefix
```

The first execution receives a expression written in a postfix notation and prints its result. The second execution calculates the same, but in a prefix notation.

## Operations supported
Currently there are some operations implemented:
  * Sum (+)
  * Subtraction (-)
  * Product (*)
  * Division (/)
  * Potentiation (^)

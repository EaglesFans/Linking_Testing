set.seed(1)
rnorm(5)

## Generating Random Numbers from a Linear Model
set.seed(20)
x <- rnorm(100)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2 * x + e

set.seed(1)
sample(1:100, 5)

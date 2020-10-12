# ElectSym

Project to provide a library with electoral computations to do different simulations.

## Divisors Methods

One electoral method is that of [Divisors](https://en.wikipedia.org/wiki/Highest_averages_method).

It needs a strictly increasing sequence of real numbers:

$f:\mathbb{N}\to\mathbb{R}$ such that $n > m \implies f(n) > f(m)$.

## Quota Methods

The second electoral method is that of [Quotas](https://en.wikipedia.org/wiki/Largest_remainder_method).

It computes in a first pass an apportionment between $L$ elements of a number of seats $S$ less than the total
number of seats $T$; such that $T-S < L$.

As a second pass, it uses the remaining votes (those not used in the first pass) to distribute the remaining $T-S$ seats.

## Simulation

We also provide the possibility to create a random votes and their results.
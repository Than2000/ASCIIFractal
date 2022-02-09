# ASCII Fractal Generator

The ASCII Fractal Generator is a fully terminal-based generator written in Haskell, which I originally built to teach myself the basics of functional programming. Currently, it can generate the Mandelbrot set and filled Julia sets for $f_c(z)=z^2+c$. Translating, scaling, and changing the iteration limit are supported as well.

## Resolution

The default resolution is 100x30 characters. Zooming out in the terminal window itself allows for higher resolution. Maximum zoom-out in VSCode allowed me to reach a resolution of 675x150 characters.

## Fractal Definitions

The ASCII Fractal Generator currently supports the Mandelbrot set and certain filled Julia sets. These are defined and calculated as follows.

### Filled Julia sets

A filled Julia set for a complex function $f(z)$ is the set of all $z$ such that iterating $z$ over the function $f(z)$ has a finite bound. Informally, plugging $z$ into the function, then $f(0)$, then $f(f(0))$ and so on will either stay small or explode to infinity; if they stay small, $z$ is in the set.

There are infinitely many filled Julia sets, one for every possible function $f$, but the ASCII Fractal Generator currently only supports functions of the form $f(z)=z^2+c$.

### The Mandelbrot set

The Mandelbrot set is the set of all complex numbers $c$ such that iterating $0$ over the function $f_c(z)=z^2+c$ has a finite bound. Informally, it's like the reverse of a filled Julia set: rather than having a fixed $c$ and graphing all values of $z$, the Mandelbrot set has $z_0=0$ and graphs all values of $c$.

## Challenges for the inquisitive explorer

- The Mandelbrot set seems to have some interesting shapes in it. What shapes are they?
- Can you find a small copy of the Mandelbrot set hidden within it? Is it exactly the same as the whole thing?
- See of you notice any connections between the Mandelbrot set at a given complex number, and the filled Julia set for that same number. Try this with lots of different numbers. Why might this be?
- What happens to the Julia set when you change the number just slightly in a particular direction?
- What "special points" can you find with particularly interesting Julia sets? Some good places to start might be `0:+0`, `0:+1`, or `0.25:+0.5`.

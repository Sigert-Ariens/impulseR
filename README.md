# impulseR

An R package around the creation and visualization of impulse response functions for one-dimensional dynamic regression models.

## Installation

To install the package, you can use the `remotes` package:

```{r, eval = FALSE}
remotes::install_gitlab("u0133721/impulseR", host = "gitlab.kuleuven.be")
```

To use the package, use `library` 

```{r, eval = FALSE}
library(impulseR)
```

## Functionality

This package allows users to compute and visualize the model-expected response to a given impulse. For a detailed explanation on how to use the package, we refer the reader to the [Documentation](https://impulser-5cae6f.pages.gitlab.kuleuven.be/reference/index.html). In the documentation, one can find a [Getting started page](https://impulser-5cae6f.pages.gitlab.kuleuven.be/articles/getting_started.html) for the package, as well as detailed documentation on its functionality, specifically about the way in which [impulse responses should be computed](https://impulser-5cae6f.pages.gitlab.kuleuven.be/articles/irf_generator.html), about how one [can estimate parameters and compute the impulse responses in one go](https://impulser-5cae6f.pages.gitlab.kuleuven.be/articles/irf_empirical.html), and how [one can visualize these system responses](https://impulser-5cae6f.pages.gitlab.kuleuven.be/articles/plotting.html).

## Getting help

If you encounter a bug or need help getting a function to run, please file an issue with a minimal reproducible example on [Gitlab](https://gitlab.kuleuven.be/u0133721/impulseR/-/issues).

## License

This project is distributed under a GNU GPL-3 license. For details, please see the [License](https://gitlab.kuleuven.be/u0133721/impulseR/-/blob/main/LICENSE.md)
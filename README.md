# impulseR

An R package around the creation and visualization of impulse response functions for one-dimensional dynamic regression models. Specifically, TO DO

## Installation

To install the package, you can use the `remotes` package:

```{r, eval = FALSE}
remotes::install_gitlab("u0133721/impulseR", host = "gitlab.kuleuven.be/")
```

To use the package, use `library` 

```{r, echo = FALSE, include = FALSE}
devtools::install()
```

```{r}
library(impulseR)
```

## Functionality

This package allows users to compute and visualize the model-expected response to a given impulse. For a detailed explanation on how to use the package, we refer the reader to the [Documentation](https://ppw-okpiv.pages.gitlab.kuleuven.be/u0133721/impulseR/reference/index.html). In the documentation, one can find the [theoretical background](https://ppw-okpiv.pages.gitlab.kuleuven.be/u0133721/impulseR/reference/impulse_response.html) of the package, the way in which [impulse responses should be computed](https://ppw-okpiv.pages.gitlab.kuleuven.be/u0133721/impulseR/reference/getting_started.html), and the way in which [they should be visualized](https://ppw-okpiv.pages.gitlab.kuleuven.be/u0133721/impulseR/reference/plotting.html).

## Getting help

If you encounter a bug or need help getting a function to run, please file an issue with a minimal reproducible example on [Gitlab](https://gitlab.kuleuven.be/u0133721/impulseR/-/issues).

## License

This project is distributed under a GNU GPL-3 license. For details, please see the [License](https://ppw-okpiv.pages.gitlab.kuleuven.be/u0133721/impulseR/LICENSE.md)
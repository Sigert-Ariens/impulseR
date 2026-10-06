# impulseR

A user friendly R package for the calculation and visualization of
system response functions for one-dimensional dynamic regression models.

## Installation

To install the package, you can use the `remotes` package:

    remotes::install_github("Sigert-Ariens/impulseR")

To use the package, use `library`

    library(impulseR)

## Functionality

This package allows users to compute and visualize the model-implied
response to a given time series of inputs. For a detailed explanation on
how to use the package, we refer the reader to the
[Documentation](https://Sigert-Ariens.github.io/impulseR/reference/index.html).
In the documentation, one can find a [Getting started
page](https://Sigert-Ariens.github.io/impulseR/articles/getting_started.html)
for the package, as well as detailed documentation on its functionality,
specifically about the way in which [impulse responses should be
computed](https://Sigert-Ariens.github.io/impulseR/articles/irf_generator.html),
about how one [can estimate parameters and compute the impulse responses
in one
go](https://Sigert-Ariens.github.io/impulseR/articles/irf_empirical.html),
and how [one can visualize these system
responses](https://Sigert-Ariens.github.io/impulseR/articles/plotting.html).

## Getting help

If you encounter a bug or need help getting a function to run, please
file an issue with a minimal reproducible example on
[Github](https://github.com/Sigert-Ariens/impulseR/issues).

## License

This project is distributed under a GNU GPL-3 license. For details,
please see the
[License](https://Sigert-Ariens.github.io/impulseR/main/LICENSE.md)

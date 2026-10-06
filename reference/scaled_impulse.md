# Create single scaled impulse

Create single scaled impulse

## Usage

``` r
scaled_impulse(N, index = 1, scale = 1)
```

## Arguments

- N:

  Integer denoting the size of the impulse vector

- index:

  Integer denoting at which index in the vector to add the single
  impulse. Defaults to `1`.

- scale:

  Numeric denoting the value the impulse should be given. Defaults to
  `1`

## Value

Numeric vector containing a singular impulse of value `scale`.

## Examples

``` r
scaled_impulse(
  10,
  index = 1,
  scale = 2
)
#>  [1] 2 0 0 0 0 0 0 0 0 0
```

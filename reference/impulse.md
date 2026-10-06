# Create single unit impulse

Create single unit impulse

## Usage

``` r
impulse(N, index = 1)
```

## Arguments

- N:

  Integer denoting the size of the impulse vector

- index:

  Integer denoting at which index in the vector to add the single
  impulse. Defaults to `1`.

## Value

Numeric vector containing a singular impulse of value `1`.

## Examples

``` r
impulse(
  10,
  index = 1
)
#>  [1] 1 0 0 0 0 0 0 0 0 0
```

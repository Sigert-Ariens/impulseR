# Analysis

In this vignette, we discuss how the `irf_generator` function can be
used to generate the different types of system responses discussed in
the manuscript.

## Arguments

`irf_generator` has two main sets of arguments. The first set are the
model parameters. The second set are the input vectors we supply the
system with. Different system responses will be returned depending on
which input vectors are supplied. The `burnin` argument is discussed
afterwards.

### Parameters

The different types of system responses can be calculated by supplying
parameter values and input vectors to the `irf_generator` function. We
distinguish the autoregressive (AR) parameters
$`\{ \phi_{1}, \phi_{2},...\}`$ from covariate parameters,
$`\{\beta_{x}, \beta_{Lx},\beta_{L^{2}x},...\}`$, and the intercept
parameter $`\alpha`$. Note that the values of the parameters can be
hypothetical or estimated. They are supplied using the following
arguments:

- `intercept`: A single numeric value that denotes the value of the
  intercept of the model;
- `x_params`: A numeric vector of length length $`1 + q`$ containing the
  values of the covariate parameters
  $`\{ \beta_{x}, \beta_{Lx},\beta_{L^{2}x}, \dots,\beta_{L^{q}x} \}`$.
  Importantly, $`q`$ is the number of free lagged covariate effects. The
  first element of the vector is taken to be the contemporaneous effect
  of the covariate, $`\beta_{x}`$;
- `ar_params`: A numeric vector of length $`p`$ containing the values of
  the AR parameters, $`\{ \phi_{1}, \phi_{2}, \dots,\phi_{p} \}`$.

Although one can supply arbitrary parameter values, certain restrictions
on the values of the AR parameters are required for stability.
Specifically, one may expect to see unstable (i.e., exploding) system
responses when some of the roots of the lag polynomial
$`\phi(L): 1 - \sum_{i=1}^{p}\phi_{i}L^{i}`$ are inside the complex unit
circle. A warning will be returned if this is the case. We advise
against using this package for the study of systems which are not
stable, since stability is assumed in the derivations.

### Input vectors

The values of the covariate, $`\{ x_{0}, x_{1},...,x_{t} \}`$, and the
values of the innovations, $`\{ v_{0}, v_{1},...,v_{t} \}`$ are supplied
as vector arguments. The input vector for the intercept is a unit vector
which is supplied automatically. Different system responses will be
returned depending on which input vectors are supplied. To make this
concrete, let us consider the following LCV model with two AR
parameters:

``` math
y_{t} = 2 + 0.7 y_{t-1} -0.5 y_{t-2} + 2 x_{t} + v_{t}
```

#### Impulse response

Say we want to perturb the covariate by a unit, and trace the effect of
this perturbation over time. An impulse response will be returned if a
vector of the form $`\{1,0,0,...,0\}`$ is supplied as input vector. To
only retrieve this impulse response, we set the intercept to 0 (or,
equally, leave the intercept argument unspecified), and all other input
vectors to zero vectors. We call the `irf_generator` function as
follows:

``` r

data <- irf_generator(
  x_params = 2,
  ar_params = c(0.7, -0.5),
  x = c(1, 0, 0, 0, 0, 0, 0, 0, 0)
)
```

The output is a list containing the following entries:

- `intercept`, `ar_params`, `x_params`: The values for the parameters of
  the model;
- `irf`: A data.frame containing the computed system responses.

The data.frame contains the results we are interested in, and itself
contains the following columns:

- `time`: The time index, starting at $`t = 0`$;
- `irf`: The total response;
- `irf_intercept`: The cumulative response towards the unit vector;
- `irf_x`: The response towards the covariate, which in this case is of
  the form $`\{1,0,0,0,...,0 \}`$ and therefore equal to the impulse
  response function ($`h_{x}(s)`$);
- `irf_v`: The response towards the innovations, which in this case is
  $`0`$;
- `x`: The covariate input vector;
- `innovations`: The innovation input vector.

``` r

data$irf
```

    ##   time         irf irf_intercept       irf_x irf_v x innovations
    ## 1    0  2.00000000             0  2.00000000     0 1           0
    ## 2    1  1.40000000             0  1.40000000     0 0           0
    ## 3    2 -0.02000000             0 -0.02000000     0 0           0
    ## 4    3 -0.71400000             0 -0.71400000     0 0           0
    ## 5    4 -0.48980000             0 -0.48980000     0 0           0
    ## 6    5  0.01414000             0  0.01414000     0 0           0
    ## 7    6  0.25479800             0  0.25479800     0 0           0
    ## 8    7  0.17128860             0  0.17128860     0 0           0
    ## 9    8 -0.00749698             0 -0.00749698     0 0           0

If we had supplied any other vector than $`\{1,0,0,0,...,0 \}`$, the
column `irf_x` would contain the appropriate system response.

To visualize the impulse response $`h_{x}(s)`$, we can provide the
`irf_plot` function with the data.frame of system responses:

``` r

irf_plot(data$irf)
```

![One sees a plot with three lines, namely a black dotted line that is
overlayed on a red solid line, and a green horizontal line. These lines
represent the system responses to a single impulse at time \$t = 0\$,
represented by a red lollipop, where the black line represents the
system's total response, the red line represents the system's response
to the impulse from the covariate, and the green line represents the
system's response to the impulse from the innovations. While the green
line remains \$0\$ across time, the red line starts out high and
decreases rapidly, overshooting the value \$0\$ only to increase again,
showing a sawtooth pattern that converges to \$0\$.
](irf_generator_files/figure-html/unnamed-chunk-4-1.png)

*Visualization of an impulse response function for an LCV model with
$`\beta_i = 2`$, and $`\phi_i = \{0.7, -0.5\}`$. A single unit impulse
is given by the covariate at time $`t = 0`$.*

We observe the impulse response towards the covariate, $`h_{x}(s)`$ in
red. Note that the plotting function will automatically denote the time
index by $`s`$ and use appropriate legends if the input vectors are of
the form $`\{1,0,0,0,...,0 \}`$ (impulse responses). The plotting
function will also automatically plot the model-implied total response
$`y_{t}`$ as a black dotted line. In this case, since we did not present
any innovations, the total response is simply `irf_x`, so the black
dotted line and the red line overlap (this may be a bit hard to see with
the default colors).

The plotting function allows one to select which responses to visualize,
and customize other features (see the vignette on
[Plotting](https://impulser-5cae6f.pages.gitlab.kuleuven.be/articles/plotting.html)).

#### Scaled impulse response

A scaled impulse response will be returned if a vector of the form
$`\{a,0,0,...,0\}`$ is supplied as input vector, where $`a`$ is a
scalar. A useful scalar can be a scalar indicating a standard, or
typical deviation in the input variable. For example, if we know that
$`v_{t} \overset{iid}{\sim} N(0, 2^2)`$, then it might be insightful to
assess the system response towards a standard deviation increase in the
innovations. We supply the function with this information by supplying
the appropriate input vector:

``` r

data <- irf_generator(
  x_params = 2,
  ar_params = c(0.7, -0.5),
  innovations = c(2, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)
```

The output is again a data frame with the same columns as before. The
scaled impulse response $`h_{v}(s)\sigma_{v}`$ is in the column `irf_v`,
and now the column `innovations` contains a $`2`$ at $`t = 0`$:

``` r

data$irf
```

    ##    time         irf irf_intercept irf_x       irf_v x innovations
    ## 1     0  2.00000000             0     0  2.00000000 0           2
    ## 2     1  1.40000000             0     0  1.40000000 0           0
    ## 3     2 -0.02000000             0     0 -0.02000000 0           0
    ## 4     3 -0.71400000             0     0 -0.71400000 0           0
    ## 5     4 -0.48980000             0     0 -0.48980000 0           0
    ## 6     5  0.01414000             0     0  0.01414000 0           0
    ## 7     6  0.25479800             0     0  0.25479800 0           0
    ## 8     7  0.17128860             0     0  0.17128860 0           0
    ## 9     8 -0.00749698             0     0 -0.00749698 0           0
    ## 10    9 -0.09089219             0     0 -0.09089219 0           0

Applying the `irf_plot` function yields a visualization of the scaled
impulse response:

``` r

irf_plot(data$irf)
```

![One again sees a plot with three lines, namely a black dotted line
that is overlayed on a green solid line, and a red horizontal line.
These lines again represent the system responses to a single impulse at
time \$t = 0\$, represented by a green lollipop, where the black line
represents the system's total response, the red line represents the
system's response to the impulse from the covariate, and the green line
represents the system's response to the impulse from the innovations.
While the red line remains \$0\$ across time, the green line starts out
high and decreases rapidly, overshooting the value \$0\$ only to
increase again, showing a sawtooth pattern that converges to \$0\$.
](irf_generator_files/figure-html/unnamed-chunk-7-1.png)

*Visualization of a scaled impulse response function for an LCV model
with $`\beta_i = 2`$, and $`\phi_i = \{0.7, -0.5\}`$. A single scaled
impulse of size $`2`$ is given by the innovations at time $`t = 0`$.*

We see that now an innovation of magnitude $`2`$ was supplied (green
lollipop). The scaled impulse response $`h_{v}(s)\sigma_{v}`$ is drawn
as a green line. The legend denotes this response as $`h_{v}(s) v_0`$,
where $`v_0`$ signifies that the curve no longer represents the impulse
response, but the response when supplied by a non-unit input variable.
Throughout this documentation, red will typically be reserved for
covariate related objects and green for innovation related objects
(though this can be customized by the user).

#### Cumulative responses

A cumulative response will be returned if the input vector contains
non-zero values for arguments beyond the first argument. Let us supply
two consecutive unit innovations, followed by a number of $`0`$ values
to show how the system relaxes:

``` r

data <- irf_generator(
  x_params = 2,
  ar_params = c(0.7, -0.5),
  innovations = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
)
```

The output has the same structure. The cumulative response towards the
innovations $`(h_{v} * v)_t`$ is again given under the column `irf_v`.
We see that the column `irf_v` will always contain the system response
towards the innovations.

``` r

data$irf
```

    ##    time         irf irf_intercept irf_x       irf_v x innovations
    ## 1     0  1.00000000             0     0  1.00000000 0           1
    ## 2     1  1.70000000             0     0  1.70000000 0           1
    ## 3     2  0.69000000             0     0  0.69000000 0           0
    ## 4     3 -0.36700000             0     0 -0.36700000 0           0
    ## 5     4 -0.60190000             0     0 -0.60190000 0           0
    ## 6     5 -0.23783000             0     0 -0.23783000 0           0
    ## 7     6  0.13446900             0     0  0.13446900 0           0
    ## 8     7  0.21304330             0     0  0.21304330 0           0
    ## 9     8  0.08189581             0     0  0.08189581 0           0
    ## 10    9 -0.04919458             0     0 -0.04919458 0           0

Applying the `irf_plot` function yields a trajectory plot:

``` r

irf_plot(data$irf)
```

![This plot resembles the previous one, albeit with two green lollipops
instead of one, representing more than one impulse that has been given
to the system (specifically at times \$t = 0\$ and \$t = 1\$). The
overall pattern remains the same, although the green line is pushed
further up at \$t = 1\$ due to the second impulse, only to relax after
this timepoint.
](irf_generator_files/figure-html/unnamed-chunk-10-1.png)

*Visualization of a cumulative response function for an LCV model with
$`\beta_i = 2`$, and $`\phi_i = \{0.7, -0.5\}`$. Two unit impulses are
given by the innovations at times $`t = 0`$ and $`t = 1`$.*

We see that the time index has changed to $`t`$, and the legend denotes
the cumulative response towards the innovations by $`(h_{v} * v)_t`$.

#### Total response

As an example of a total response, we will supply a unit covariate input
and a unit innovation at the same time. We supply the `irf_generator`
function with both input vectors and visualize the trajectory:

``` r

data <- irf_generator(
  x_params = 2,
  ar_params = c(0.7, -0.5),
  x = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  innovations = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)

irf_plot(data$irf)
```

![This plot resembles the previous ones, but now a single unit impulse
has been provided by both the covariate and the innovations at time \$t
= 1\$. Besides the original three lines, a fourth, gray horizontal line
has appeared at \$0\$, representing the effect of the intercept. The
overall pattern remains the same as before; All three lines show a
sawtooth pattern of convergence to \$0\$.
](irf_generator_files/figure-html/unnamed-chunk-11-1.png)

*Visualization of the total response as the sum of the covariate and
innovation responses functions for an LCV model with $`\beta_i = 2`$,
and $`\phi_i = \{0.7, -0.5\}`$. Two unit impulses are given at time
$`t = 0`$; One through the covariate and one through the innovations.*

## Intercept parameters

To include an intercept parameter in the visualization of system
responses, one can supply the intercept parameter value to the
`irf_generator` function:

``` r

data <- irf_generator(
  intercept = 2,
  x_params = 2,
  ar_params = c(0.7, -0.5),
  x = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  innovations = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)

irf_plot(data$irf)
```

![This plot resembles the previous one, but now the gray horizontal line
(intercept response) and the black dashed line (total response) have
been moved up vertically with \$2\$ units.
](irf_generator_files/figure-html/unnamed-chunk-12-1.png)

*Visualization of the effect of the intercept on the total response. An
LCV model was used with $`\alpha = 2`$, $`\beta_i = 2`$, and
$`\phi_i = \{0.7, -0.5\}`$. As in the previous figure, two unit impulses
are given at time $`t = 0`$; One through the covariate and one through
the innovations.*

By default, the `irf_generator` will ‘burn in’ the part of the system
response due to the intercept parameter analytically, in effect fixing
it to its model-implied time limit. This allows one to interpret the
other system responses relative to the hypothetical equilibrium state of
the system.

However, one can override this default behavior by setting the argument
`burnin = FALSE`:

``` r

data <- irf_generator(
  intercept = 2,
  x_params = 2,
  ar_params = c(0.7, -0.5),
  x = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  innovations = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  burnin = FALSE
)

irf_plot(data$irf)
```

![This plot resembles the previous one, but now the gray line (intercept
response) also shows sawtooth-like behavior.
](irf_generator_files/figure-html/unnamed-chunk-13-1.png)

*Visualization of the effect of the intercept on the total response when
`burnin = FALSE`, that is when the system is not in equilibrium. As one
can see, the impulses provided through the intercept show a sawtooth
pattern, showing behavior that is dependent on the initial value.*

By setting the argument `burnin = FALSE`, the behavior of the system is
dependent on its initial value while the system approaches its
equilibrium state.

## General dynamic regression models

In the manuscript we focused on lag-1 dynamic regression models for
accessibility. Here, we provide some examples of how dynamic regression
models with more general lag structures can be investigated using the
`irf_generator` function. The following table shows how different
parameter settings can be supplied to the `irf_generator` function,
together with the associated model:

| **Abbreviation** | **`intercept`** | **`ar_params`** | **`x_params`** | **Model** |
|----|----|----|----|----|
| LR | `0` | `0` | `1` | $`y_{t} = x_{t} + v_{t}`$ |
| AR(1) | `1` | `0.7` | `0` | $`y_{t} = 1 + 0.7 y_{t-1} + v_{t}`$ |
| LCV(1) | `0` | `0.7` | `1` | $`y_{t} = 0.7 y_{t-1} + x_{t} + v_{t}`$ |
| CF(2) | `0` | `c(0.5, -0.5)` | `c(2, -1, 1)` | $`y_{t} = 0.5 y_{t-1} - 0.5 y_{t-2} + 2 x_{t} - x_{t-1} + x_{t-2}+ v_{t}`$ |
| ADL(2, 2) | `3` | `c(0.7, 0.2)` | `c(2, 0.5, 1)` | $`y_{t} = 3+ 0.7 y_{t-1} + 0.2 y_{t-2} + 2 x_{t} + 0.5 x_{t-1} + x_{t-2}+ v_{t}`$ |

Parentheses indicate the number of AR parameters $`p`$ and lagged
covariate parameters $`q`$. For example, the ADL(2,2) allows for p = 2
AR parameters ($`\phi_{1}`$ and $`\phi_{2}`$) and for q = 2 lagged
covariate parameters ($`\beta_{Lx}, \beta_{L^{2}x}`$). Note that the CF
model only has a single value in parenthesis, because p must equal q for
the CF restrictions to hold. The CF restrictions are satisfied if
$`\beta_{L^{j}x} = - \phi_{j} \beta_{x}`$ for all j in $`\{ 1,...,q \}`$
(see Appendix A2 in Ariens et al., submitted). Similarly, the LCV model
only has a single value in parenthesis, because the order of a LCV model
is determined by the number of free AR parameters, that is by $`p`$.
Indeed, for the LCV model all lagged covariate parameters are fixed to
0. We could refer to a LCV model as an ADL(p, 0) model. Finally, the LR
model never requires parentheses, because all lagged parameters are
fixed to 0.

To illustrate how the `irf_generator` function can be used to
investigate models with more general lag structures, let us visualize
the impulse responses of the CF(2) model in the above table. We know
from the manuscript that for any CF model, the effect of the covariate
will be purely contemporaneous. We can verify that this is the case for
our CF(2) model by calculating the impulse response functions:

``` r

data <- irf_generator(
  intercept = 0,
  x_params = c(2, -1, 1),
  ar_params = c(0.5, -0.5),
  x = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  innovations = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)
```

We call the `irf_plot` function:

``` r

irf_plot(data$irf)
```

![This plot shows the familiar four lines, as well as a lollipop
representing the covariate and innovation impulses. After the covariate
impulse (covariate response), the red line initially starts out high but
then immediately falls to and remains at \$0\$ for the remainder of the
time. The green line (innovation response), on the other hand, shows a
sawtooth-like pattern, which is nicely followed and overlayed by the
black dashed line (total response).
](irf_generator_files/figure-html/unnamed-chunk-15-1.png)

*Visualization of the impulse response function of a CV model with
$`\alpha = 0`$, $`\beta_i = \{2, -1, 1 \}`$, and
$`\phi_i = \{0.5, -0.5 \}`$. As one can see, the covariate impulse dies
out immediately while the innovation impulse persists across time.*

Indeed, the impulse response $`h_{x}(s)`$ decays immediately following a
perturbation.

# Calculate system responses

Compute system responses based on a particular set of parameters of the
general \\ADL(p, q)\\ model.

## Usage

``` r
irf_generator(
  intercept = 0,
  ar_params = 0,
  x_params = 0,
  x = NULL,
  innovations = NULL,
  data = NULL,
  cols = c("y", "x"),
  burnin = TRUE,
  na_action = "listwise",
  estimated = FALSE
)
```

## Arguments

- intercept:

  Numeric denoting the intercept, \\\alpha\\. Defaults to `0`.

- ar_params:

  Numeric vector denoting the autoregressive parameters (the \\\phi\\)
  parameters of the model. Parameters need to be given in order of
  increasing lags (i.e., first element for \\\phi\_{1}\\, second element
  for \\\phi\_{2}\\,...). Defaults to `0`.

- x_params:

  Numeric vector denoting the covariate parameters of the model (the
  \\\beta\\ parameters). Parameters again need to be given in order of
  increasing lags(i.e., first element for \\\beta\_{x}\\, second element
  for \\\beta\_{Lx}\\,...). Defaults to `0`, indicating that there are
  no covariate parameters.

- x:

  Numeric vector denoting the values of covariate at each time point.
  Depending on the input vectors supplied, different system responses
  will be returned. Defaults to `NULL`, which returns an empty vector of
  the same length as `innovations` (if provided).

- innovations:

  Numeric vector denoting the values of the innovations at each time
  point. Depending on the input vectors supplied, different system
  responses will be returned. Defaults to `NULL`, which returns an empty
  vector of the same length as `x` (if provided).

- data:

  Dataframe containing the variables of interest. Defaults to `NULL`,
  meaning there are no data attached

- cols:

  Character vector denoting the columns containing the variables of
  interest. First character should denote the dependent variable, the
  second one the covariate of interest. Defaults to `c("y", "x")`.

- burnin:

  Logical specifying if the part of the system response due to the
  intercept should be burned in analytically. Defaults to `TRUE`, the
  system responses responses will be displayed relative to the
  hypothetical equilibrium state of the system. If set to `FALSE`,
  initial value dependent behavior will be present.

- na_action:

  Character denoting how `NA`s should be removed from the data. Either
  `"listwise"`, `"casewise"`, `"pairwise"`, or `"partial"` (see
  [`estimate`](https://Sigert-Ariens.github.io/impulseR/reference/estimate.md)
  for more information). Ignored whenever innovations do not need to be
  computed. Defaults to `"listwise"`.

- estimated:

  Logical denoting whether to provide the types of the innovations in a
  separate column of the resulting `data.frame`. Defaults to `TRUE` when
  `x` and `innovations` are left unspecified, and to `FALSE` otherwise.
  It is recommended to not change this argument for the sake of
  interpretation of the output.

## Value

List containing the parameters that were used for the generation of the
system responses (under `"intercept"`, `"x_params"`, and `"ar_params"`,
and a data.frame containing the model-implied total responses over the
observation period (under `"irf"`). Within the data.frame, column
`"time"` contains the time index starting at 0. The columns
`"irf_intercept"`, `"irf_x"`, and `"irf_v"` contain the cumulative
responses towards the unit vector, covariate, and innovations
respectively. These partial responses sum up to the total response
\\y\_{t}\\, which is provided in the `"irf"` column. The columns `"x"`
and `"innovations"` contain the values of the covariate and the
innovations. Finally, the column `"innovation_type"` denotes whether the
values in `"innovations"` represent initial conditions or residuals of
an estimation procedure. Note that this column is only present when you
do not provide the values for `innovations` yourself, as in this case
the `innovations` are taken as known.

## Details

This function calculates system responses for an \\ADL(p, q)\\ model,
formalized as:

\$\$y_t = \alpha + \sum\_{i = 1}^p \phi\_{i}y\_{t-i} + \beta\_{x}
x\_{t} + \sum\_{j = 1}^q \beta\_{L^{j}x} x\_{t - j} + v\_{t}\$\$.

One should provide an a priori chosen set of parameters through the
arguments `intercept`, `x_params`, and `ar_params`.

Additionally, one also needs to provide the impulses to the system. One
can achieve this in two ways. First, one can specify the impulses to the
covariates through `x` and/or the impulses to the innovations through
`innovations`. Second, one can provide a data set to the argument
`data`, which is then used to derive the (cumulative) impulses of `x`
and `innovations` automatically. In this case, it is recommended to put
`burnin` to `FALSE`.

When both options are specified, the data takes precedence.

## Examples

``` r
# Create parameters of an ADL(2, 1), meaning having two lags in the residuals
# and one lag in the values of x. These will be used for all examples.
params <- list(
  "intercept" = 1,
  "autoregression" = c(0.5, 0.1),
  "slopes" = c(2, 0.5)
)



########################
# PRE-SPECIFIED IMPULSES

# Use with single impulse of x in the beginning of the study, with burnin
irf_generator(
  params$intercept,
  params$autoregression,
  params$slopes,
  x = c(1, rep(0, 9))
)
#> $fit
#> [1] NA
#> 
#> $intercept
#> [1] 1
#> 
#> $x_params
#> [1] 2.0 0.5
#> 
#> $ar_params
#> [1] 0.5 0.1
#> 
#> $irf
#>    time      irf irf_intercept      irf_x irf_v x innovations
#> 1     0 4.500000           2.5 2.00000000     0 1           0
#> 2     1 4.000000           2.5 1.50000000     0 0           0
#> 3     2 3.450000           2.5 0.95000000     0 0           0
#> 4     3 3.125000           2.5 0.62500000     0 0           0
#> 5     4 2.907500           2.5 0.40750000     0 0           0
#> 6     5 2.766250           2.5 0.26625000     0 0           0
#> 7     6 2.673875           2.5 0.17387500     0 0           0
#> 8     7 2.613563           2.5 0.11356250     0 0           0
#> 9     8 2.574169           2.5 0.07416875     0 0           0
#> 10    9 2.548441           2.5 0.04844063     0 0           0
#> 

# Use with single impulse of x in the beginning of the study, without burnin
irf_generator(
  params$intercept,
  params$autoregression,
  params$slopes,
  x = c(1, rep(0, 9)),
  burnin = FALSE
)
#> $fit
#> [1] NA
#> 
#> $intercept
#> [1] 1
#> 
#> $x_params
#> [1] 2.0 0.5
#> 
#> $ar_params
#> [1] 0.5 0.1
#> 
#> $irf
#>    time      irf irf_intercept      irf_x irf_v x innovations
#> 1     0 3.000000      1.000000 2.00000000     0 1           0
#> 2     1 3.000000      1.500000 1.50000000     0 0           0
#> 3     2 2.800000      1.850000 0.95000000     0 0           0
#> 4     3 2.700000      2.075000 0.62500000     0 0           0
#> 5     4 2.630000      2.222500 0.40750000     0 0           0
#> 6     5 2.585000      2.318750 0.26625000     0 0           0
#> 7     6 2.555500      2.381625 0.17387500     0 0           0
#> 8     7 2.536250      2.422687 0.11356250     0 0           0
#> 9     8 2.523675      2.449506 0.07416875     0 0           0
#> 10    9 2.515462      2.467022 0.04844063     0 0           0
#> 

# Use with multiple values for the innovations, with burnin
irf_generator(
  params$intercept,
  params$autoregression,
  params$slopes,
  innovations = rnorm(10)
)
#> $fit
#> [1] NA
#> 
#> $intercept
#> [1] 1
#> 
#> $x_params
#> [1] 2.0 0.5
#> 
#> $ar_params
#> [1] 0.5 0.1
#> 
#> $irf
#>    time        irf irf_intercept irf_x      irf_v x innovations
#> 1     0  1.0201657           2.5     0 -1.4798343 0 -1.47983426
#> 2     1  2.7884294           2.5     0  0.2884294 0  1.02834657
#> 3     2  0.2751802           2.5     0 -2.2248198 0 -2.22105108
#> 4     3 -0.2221246           2.5     0 -2.7221246 0 -1.63855763
#> 5     4  1.2736952           2.5     0 -1.2263048 0  0.35723943
#> 6     5  1.6409188           2.5     0 -0.8590812 0  0.02628372
#> 7     6  1.8001422           2.5     0 -0.6998578 0 -0.14768672
#> 8     7  2.8722748           2.5     0  0.3722748 0  0.80811178
#> 9     8  2.0420176           2.5     0 -0.4579824 0 -0.57413399
#> 10    9  2.8906459           2.5     0  0.3906459 0  0.58240957
#> 

# Use with multiple values for the innovations, without burnin
irf_generator(
  params$intercept,
  params$autoregression,
  params$slopes,
  innovations = rnorm(10),
  burnin = FALSE
)
#> $fit
#> [1] NA
#> 
#> $intercept
#> [1] 1
#> 
#> $x_params
#> [1] 2.0 0.5
#> 
#> $ar_params
#> [1] 0.5 0.1
#> 
#> $irf
#>    time        irf irf_intercept irf_x      irf_v x innovations
#> 1     0 0.02794973      1.000000     0 -0.9720503 0  -0.9720503
#> 2     1 2.85093822      1.500000     0  1.3509382 0   1.8369634
#> 3     2 3.62912665      1.850000     0  1.7791266 0   1.2008626
#> 4     3 4.16783796      2.075000     0  2.0928380 0   1.0681808
#> 5     4 4.08189185      2.222500     0  1.8593919 0   0.6350602
#> 6     5 4.74461684      2.318750     0  2.4258668 0   1.2868871
#> 7     6 4.40224809      2.381625     0  2.0206231 0   0.6217505
#> 8     7 4.34076109      2.422687     0  1.9180736 0   0.6651754
#> 9     8 0.98749564      2.449506     0 -1.4620106 0  -2.6231097
#> 10    9 1.32672891      2.467022     0 -1.1402930 0  -0.6010950
#> 



########################
# IMPULSES BASED ON DATA

# Generate data
set.seed(1)
data <- irf_generator(
  intercept = 1,
  x_params = c(1, 2, -0.5),
  ar_params = c(0.9, -0.1, 0.25),
  x = rnorm(100),
  innovations = rnorm(100)
)$irf
#> Warning: The AR parameters imply a nonstationary process; Some of the roots of phi(L) are inside the complex unit circle

# Use the data to determine the values for x and y.
#
# Using "irf" as the dependent variable and "x" as the independent variable
# in the original data set.
irf_generator(
  params$intercept,
  params$autoregression,
  params$slopes,
  data = data,
  cols = c("irf", "x"),
  burnin = FALSE
)
#> $fit
#> [1] NA
#> 
#> $intercept
#> [1] 1
#> 
#> $x_params
#> [1] 2.0 0.5
#> 
#> $ar_params
#> [1] 0.5 0.1
#> 
#> $irf
#>     time         irf irf_intercept       irf_x        irf_v            x
#> 1      0 -21.2468205      1.000000 -1.25290762 -20.99391287 -0.626453811
#> 2      1 -22.1492869      1.500000 -0.57239407 -23.07689280  0.183643324
#> 3      2 -22.8757128      1.850000 -1.99092336 -22.73478948 -0.835628612
#> 4      3 -22.6946873      2.075000  1.72004621 -26.48973351  1.595280802
#> 5      4 -19.3916700      2.222500  2.11758671 -23.73175668  0.329507772
#> 6      5 -19.0937684      2.318750 -0.24538490 -21.16713351 -0.820468384
#> 7      6 -20.5204505      2.381625  0.65369013 -23.55576566  0.487429052
#> 8      7 -17.3733549      2.422687  2.02267051 -21.81871291  0.738324705
#> 9      8 -15.1645149      2.449506  2.59742933 -20.21145044  0.575781352
#> 10     9 -13.8816525      2.467022  1.17809562 -17.52676997 -0.305388387
#> 11    10 -14.3429972      2.478462  3.71965888 -20.54111765  1.511781168
#> 12    11 -11.2072059      2.485933  3.51321606 -17.20635494  0.389843236
#> 13    12 -10.2877612      2.490813  1.08101438 -13.85958819 -0.621240581
#> 14    13 -15.0268128      2.494000 -3.84819127 -13.67262113 -2.214699887
#> 15    14 -17.4983862      2.496081 -0.67348231 -19.32098495  1.124930918
#> 16    15 -12.8983363      2.497440 -0.24896204 -15.14681479 -0.044933609
#> 17    16 -13.6038831      2.498328 -0.24667658 -15.85553486 -0.016190263
#> 18    17 -13.6734485      2.498908  1.73134280 -17.90369953  0.943836211
#> 19    18  -9.9591223      2.499287  2.95536424 -15.41377353  0.821221195
#> 20    19  -8.4097409      2.499534  3.24922964 -14.15850485  0.593901321
#> 21    20  -7.8010048      2.499696  4.05505665 -14.35575725  0.918977372
#> 22    21  -4.0035316      2.499801  4.37621257 -10.87954549  0.782136301
#> 23    22  -2.9607437      2.499870  3.13381007  -8.59442400  0.074564983
#> 24    23  -5.6254137      2.499915 -1.93689461  -6.18843441 -1.989351696
#> 25    24  -8.2640318      2.499945 -0.41009065 -10.35388578  0.619825748
#> 26    25  -3.7244082      2.499964 -0.20107939  -6.02329270 -0.056128740
#> 27    26  -3.5834479      2.499976 -0.48120414  -5.60222018 -0.155795507
#> 28    27  -5.7105835      2.499985 -3.28011253  -4.93045551 -1.470752384
#> 29    28  -8.7356999      2.499990 -3.37985298  -7.85583687 -0.478150055
#> 30    29  -7.3141862      2.499993 -1.42112965  -8.39304997  0.417941560
#> 31    30  -3.6430453      2.499996  1.87777976  -8.02082078  1.358679552
#> 32    31  -1.9145410      2.499997  1.27054124  -5.68507946 -0.102787727
#> 33    32  -2.1530764      2.499998  1.54699795  -6.20007249  0.387671612
#> 34    33  -2.4025380      2.499999  0.98677882  -5.88931562 -0.053805041
#> 35    34  -2.7975594      2.499999 -2.13293243  -3.16462619 -1.377059557
#> 36    35  -6.4944797      2.499999 -2.48630724  -6.50817199 -0.414994563
#> 37    36  -6.0026357      2.500000 -2.45252405  -6.05011137 -0.394289954
#> 38    37  -5.6209900      2.500000 -1.79066452  -6.33032524 -0.059313397
#> 39    38  -4.5557986      2.500000  1.02980938  -8.08560780  1.100025372
#> 40    39  -1.1027922      2.500000  2.41220242  -6.01499456  0.763175748
#> 41    40  -2.0447249      2.500000  1.36162283  -5.90634763 -0.164523596
#> 42    41  -1.6563362      2.500000  0.33304650  -4.48938269 -0.253361680
#> 43    42  -1.9543988      2.500000  1.56993144  -6.02433022  0.696963375
#> 44    43   0.5092339      2.500000  2.28007846  -4.27084456  0.556663199
#> 45    44   0.1998352      2.500000  0.19785258  -2.49801735 -0.688755695
#> 46    45  -2.4738285      2.500000 -1.43243402  -3.54139450 -0.707495157
#> 47    46   0.2620153      2.500000 -0.32101541  -1.91696928  0.364581962
#> 48    47   3.4019955      2.500000  1.41560572  -0.51361024  0.768532925
#> 49    48   3.3732654      2.500000  0.83527536   0.03799004 -0.112346212
#> 50    49   2.3927864      2.500000  2.26524060  -2.37245416  0.881107726
#> 51    50   6.3333617      2.500000  2.45291346   1.38044821  0.398105880
#> 52    51   7.0291349      2.500000  0.42798094   4.10115394 -0.612026393
#> 53    52   5.8910274      2.500000  0.83550800   2.55551942  0.341119691
#> 54    53   6.1118789      2.500000 -1.62761425   5.23949320 -1.129363096
#> 55    54   5.1851494      2.500000  1.57110953   1.11403985  1.433023702
#> 56    55  10.8641400      2.500000  5.30010499   3.06403497  1.980399899
#> 57    56  15.6642760      2.500000  3.06292044  10.10135559 -0.367221476
#> 58    57  11.9176776      2.500000 -0.21040927   9.62808683 -1.044134626
#> 59    58  10.1561515      2.500000  0.81845935   6.83769210  0.569719627
#> 60    59  16.2605801      2.500000  0.40293935  13.35764079 -0.135054604
#> 61    60  19.8700755      2.500000  5.01902383  12.35105166  2.401617761
#> 62    61  24.3889235      2.500000  3.67213473  18.21678879 -0.039240003
#> 63    62  25.4971022      2.500000  3.69782847  19.29927370  0.689739362
#> 64    63  28.7895420      2.500000  2.61700171  23.67254031  0.028002159
#> 65    64  28.8067269      2.500000  0.20573836  26.10098849 -0.743273209
#> 66    65  31.3157228      2.500000  0.37051735  28.44520543  0.188792300
#> 67    66  32.1900989      2.500000 -3.30968860  32.99978746 -1.804958629
#> 68    67  30.3779452      2.500000  0.41083784  27.46710737  1.465554862
#> 69    68  36.7925143      2.500000  0.91373417  33.37878011  0.153253338
#> 70    69  41.0768723      2.500000  4.91980088  33.65707142  2.172611670
#> 71    70  48.9365045      2.500000  4.58859875  41.84790579  0.475509529
#> 72    71  49.3938646      2.500000  1.60414137  45.28972322 -0.709946431
#> 73    72  50.2401233      2.500000  2.12741005  45.61271323  0.610726353
#> 74    73  54.0760260      2.500000 -0.33871293  51.91473891 -0.934097632
#> 75    74  53.2316845      2.500000 -2.93093107  53.66261560 -1.253633400
#> 76    75  54.2774465      2.500000 -1.54326106  53.32070757  0.291446236
#> 77    76  59.5995968      2.500000 -1.80558427  58.90518108 -0.443291873
#> 78    77  63.5638571      2.500000 -1.27655347  62.34041058  0.001105352
#> 79    78  67.1424637      2.500000 -0.66959984  65.31206358  0.074341324
#> 80    79  70.7382483      2.500000 -1.60432650  69.84257479 -0.589520946
#> 81    80  70.8249367      2.500000 -2.30122117  70.62615782 -0.568668733
#> 82    81  74.4603741      2.500000 -1.86573483  73.82610889 -0.135178615
#> 83    82  80.0283940      2.500000  1.12559515  76.40279884  1.178086997
#> 84    83  82.7186978      2.500000 -2.08186601  82.30056383 -1.523566800
#> 85    84  83.5378740      2.500000 -0.50226451  81.54013850  0.593946188
#> 86    85  90.0431868      2.500000  0.50355498  87.03963187  0.332950371
#> 87    86  97.2613700      2.500000  2.49422590  92.26714412  1.063099837
#> 88    87 101.3048414      2.500000  1.22065052  97.58419088 -0.304183924
#> 89    88 103.7589063      2.500000  1.44769351  99.81121275  0.370018810
#> 90    89 108.8009929      2.500000  1.56511879 104.73587408  0.267098791
#> 91    90 113.5007775      2.500000 -0.02416192 111.02493941 -0.542520031
#> 92    91 118.6016171      2.500000  2.28890652 113.81271063  1.207867806
#> 93    92 126.7072760      2.500000  4.06677620 120.14049977  1.160402616
#> 94    93 134.7990392      2.500000  4.24290736 128.05613182  0.700213650
#> 95    94 140.4977886      2.500000  6.05190503 131.94588357  1.586833455
#> 96    95 147.9789869      2.500000  5.36063283 140.11835409  0.558486426
#> 97    96 154.3191908      2.500000  1.01156571 150.80762507 -1.276592208
#> 98    97 155.7922797      2.500000 -0.74298079 154.03526045 -0.573265414
#> 99    98 161.4550067      2.500000 -3.00619176 161.96119848 -1.224612615
#> 100   99 166.2930066      2.500000 -3.13650154 166.92950810 -0.473400636
#>     innovations
#> 1   -20.9939129
#> 2   -12.5799364
#> 3    -9.0969518
#> 4   -12.8146495
#> 5    -8.2134110
#> 6    -6.6522818
#> 7   -10.5990232
#> 8    -7.9241167
#> 9    -6.9465174
#> 10   -5.2391735
#> 11   -9.7565876
#> 12   -5.1831191
#> 13   -3.2022990
#> 14   -5.0221915
#> 15  -11.0987156
#> 16   -4.1190602
#> 17   -6.3500290
#> 18   -8.4612506
#> 19   -4.8763703
#> 20   -4.6612481
#> 21   -5.7351275
#> 22   -2.2858164
#> 23   -1.7190755
#> 24   -0.8032679
#> 25   -6.4002262
#> 26   -0.2275064
#> 27   -1.5551853
#> 28   -1.5270161
#> 29   -4.8303871
#> 30   -3.9720860
#> 31   -3.0387121
#> 32   -0.8353641
#> 33   -2.5554507
#> 34   -2.2207714
#> 35    0.4000389
#> 36   -4.3369273
#> 37   -2.4795628
#> 38   -2.6544524
#> 39   -4.3154340
#> 40   -1.3391581
#> 41   -2.0902896
#> 42   -0.9347094
#> 43   -3.1890041
#> 44   -0.8097412
#> 45    0.2398380
#> 46   -1.8653014
#> 47    0.1035297
#> 48    0.7990139
#> 49    0.4864921
#> 50   -2.3400882
#> 51    2.5628763
#> 52    3.6481752
#> 53    0.3668976
#> 54    3.5516181
#> 55   -1.7612587
#> 56    1.9830657
#> 57    8.4579341
#> 58    4.2710055
#> 59    1.0135131
#> 60    8.9759861
#> 61    4.9884621
#> 62   10.7054989
#> 63    8.9557741
#> 64   12.2012246
#> 65   12.3347910
#> 66   13.0274572
#> 67   16.1670859
#> 68    8.1226931
#> 69   16.3452477
#> 70   14.2209706
#> 71   21.6814921
#> 72   21.0000632
#> 73   18.7830610
#> 74   24.5794100
#> 75   23.1439748
#> 76   21.2979259
#> 77   26.8785657
#> 78   27.5557493
#> 79   28.2513402
#> 80   30.9525019
#> 81   29.1736641
#> 82   31.5287725
#> 83   32.4271286
#> 84   36.7165535
#> 85   32.7495767
#> 86   38.0395062
#> 87   40.5933143
#> 88   42.7466556
#> 89   41.7924029
#> 90   45.0718486
#> 91   48.6758811
#> 92   47.8266535
#> 93   52.1316505
#> 94   56.6046109
#> 95   55.9037677
#> 96   61.3397991
#> 97   67.5538597
#> 98   64.6196125
#> 99   69.8628057
#> 100  70.5453828
#> 
```

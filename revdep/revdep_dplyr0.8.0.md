# could not find function "n"

This is the main problem. Previous versions of `dplyr` gave `n()`
a special magic treatment. This however was a source of 
[subtle bugs](https://github.com/tidyverse/dplyr/issues/3861). 

Fixing the behaviour was part of the hybrid evaluation re implementation, 
now `n()` is just like any other function, when using it in packages and
scripts. 

## in package code

If you get the error `could not find function "n"` from a function in your 
package, it means that you need to import the `n` function, there are 
several ways to do that: 
 
 - selectively import it, using an `importFrom` directive in your
   namespace, or if you use `roxygen2` a `#' @importFrom dplyr n`. 
   
 - import `dplyr` as a whole, with the namespace directive `import(dplyr)`
   or with the roxygen2 comment `#' @import dplyr`
   
 - prefix the function, i.e. call it `dplyr::n()`
 
## in scripts, examples

If you get the error in scripts, the easiest way to fix it is to load
dplyr with `library(dplyr)`, otherwise you can prefix the function 
with `dplyr::n()`

## which packages

```
AMR
binneR
carpenter
cleanNLP
CollapseLevels
comperank
comperes
dat
ddpcr
descriptr
distrr
docxtools
echarts4r
episheet
extdplyr
gaiah
GENESIS
geomnet
GerminaR
getTBinR
ggfan
ggsolvencyii
heemod
IncucyteDRC          although the problem is with row_number() instead of n()
MIAmaxent
mousetrap            row_number()
mplot
naniar
phylopath
pixiedust
portalr
prophet
ptstem
quanteda
rcv
rubias
seplyr
seqCAT
seqcombo
SeqVarTools
simglm
sjPlot
srvyr                 row_number()
stplanr               first()
tidytransit
visdat
vqtl
zFactor
```

# Empty groups

As describe in the [pre release article](https://www.tidyverse.org/articles/2018/12/dplyr-0-8-0-release-candidate/), 
dplyr 0.8.0 is more respectful of factor levels and therefore may create empty groups. 

This might be causing problems for packages: 

```
apa
bayesplot
BubbleTree
cofeatureR
comperes
dabestr
dbplyr
fuzzyjoin
GetHFData
ggeffects
lmeresampler
mudata2
```

# Corrupt data frames

dplyr 0.8.0 is stricter about what is a grouped data frame

```
naniar
psychmeta

radiant.model     seems to be making old structure for grouped data frames
rPref             seems to asume old strcuture
```

# sample_n

The `sample_n()` generic gained a `...` argument. This gives a warning for packages: 

```
bupaR
dplyr.teradata
dtplyr            (not reallt a ... problem, but related to sample_n)
tidygraph
```

# filter(.preserve)

`filter()` gained a `.preserve` argument. This causes problems for packages: 

```
chunked
ggvis
plotly
poplite
treeplyr
```

# calls to internet API problems

```
alphavantager
```

# Other problems 

Not sure what is happening

```
annotatr
bayesdfa
congressbr
corrr
crplyr
DeLorean
dlookr
dsr
edeaR
evaluator
eyetrackingR
ezsummary
FindMyFriends
forwards
freqweights
GenomicDataCommons
INDperform
metaviz
mnreadR
modeldb
Organism.dplyr
pammtools
pivottabler
PKNCA
processcheckR
purrrlyr
qwraps2
randomForestExplainer
rtrek                          uses slice() instead of filter() 
rzeit2
simTool
SingleCaseES
skimr
spbabel
spdplyr
SWMPrExtension
tbrf                          error message no longer contain "Evaluation error:"
tibbletime                    error message mismatch in the tests
tidystats
TPP
trelliscopejs
understandBPMN
valr
VWPre
WRTDStidal
```

# Stack overflow

`easyalluvial` : not sure what happens, dplyr 0.8.0 might be trying to generate too many groups in the example

# Attributes of data frames

```
egor
surveydata
tsibble
```


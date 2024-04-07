source("profexample.R")

# Classic
Rprof()
f()
Rprof(NULL)
summaryRprof()

# Visual
library(profvis)
prof <- profvis({
  f()
})
print(prof)

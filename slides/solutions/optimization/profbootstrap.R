source("bootstrap.R")

library(profvis)

# Also check that all implementations give the same result.

# impl_1 ----

profile1 <- profvis({
  set.seed(1337) 
  result1 <- impl_1(population)
})
print(profile1)

# impl_2 ----

profile2 <- profvis({
  set.seed(1337) 
  result2 <- impl_2(population)
})
print(profile2)

all.equal(result1, result2)

# impl_3 ----

profile3 <- profvis({
  set.seed(1337) 
  result3 <- impl_3(population)
})
print(profile3)

all.equal(result2, result3)

# impl_4 ----

profile4 <- profvis({
  set.seed(1337) 
  result4 <- impl_4(population)
})
print(profile4)
# Here we don't see much anymore - just that all the work happens inside
# the boot() call.

# does not work anymore here:
# all.equal(result3, result4)

# Instead compare e.g. quantiles of the bootstrap result distributions:
summary(result3$computed_output)
summary(result4$computed_output)
# In this case using boot does not speed up the computations, because
# it does more things internally than we need!
# Same for parallelization here, because each task is very small, the overhead
# of setting up the workers is larger than the benefit.

# impl_5 ----

profile5 <- profvis({
  set.seed(1337) 
  result5 <- impl_5(population)
})
print(profile5)
# Here we don't see anything anymore.
# But we could further speed up the computation - now it is about 10x faster 
# than the previously fastest impl_3.

# This looks good:
summary(result3$computed_output)
summary(result5$computed_output)

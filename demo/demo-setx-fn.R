##----- See thread: https://groups.google.com/forum/#!topic/zelig-statistical-software/1ohCNA5S_0A

library(Zelig)
z5 <- zls$new()
z5$zelig(Fertility ~ Agriculture + Education, data = swiss)
z5$setx(Education = 5, fn = list(numeric = Mode))
# setx:
#   (Intercept) Agriculture Education
# 1           1        84.6         5
z5$setx(Education = 5, fn = list(numeric = mode)) # same: function 'mode' added for backward compatibility
z5$setx(Education = 5, fn = list(numeric = median))
# setx:
#   (Intercept) Agriculture Education
# 1           1        54.1         5
z5$setx(Education = 5, fn = list(numeric = function(x) 1, other = function(x) 2))
# setx:
#   (Intercept) Agriculture Education
# 1           1           1         5
z5$setx1(Education = 10, fn = list(numeric = mode))
# setx1:
#   (Intercept) Agriculture Education
# 1           1       50.66        10
z5$setx(Education = 5)
z5$setx1(Education = 10)
# model summary
summary(z5)
z5$sim()
# model summary
summary(z5)

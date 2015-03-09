library(Zelig)

z5 <- zlogit$new()
z5$feedback()
# z5$finalize() # Not clear how to trigger it with q()
# quitting R triggers call to finalize method

z6 <- zls$new()
z6$feedback()


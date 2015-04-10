#' Zelig reference class
#'
#' Zelig reference class: \url{http://zeligproject.org/}
#'
#' @import methods
#' @export Zelig
#' @exportClass Zelig
#' 
#' @field fn R function to call to wrap
#' @field formula Zelig formula
#' @field weights forthcoming
#' @field name name of the Zelig model
#' @field data data frame or matrix
#' @field by split the data by factors
#' @field mi work with imputed dataset
#' @field idx model index
#' @field zelig.call Zelig function call
#' @field model.call wrapped function call
#' @field zelig.out estimated zelig model(s)
#' @field setx.out set values
#' @field setx.labels pretty-print qi
#' @field bsetx is x set?
#' @field bsetx1 is x1 set?
#' @field bsetrange is range set?
#' @field bsetrange1 is range1 set?
#' @field range range
#' @field range1 range1
#' @field test.statistics list of test statistics
#' @field sim.out simulated qi's
#' @field simparam simulated parameters
#' @field num  number of simulations
#' @field authors Zelig model authors
#' @field zeligauthors Zelig authors
#' @field modelauthors wrapped model authors
#' @field packageauthors wrapped package authors
#' @field refs citation information
#' @field year model is released
#' @field description model description
#' @field url model URL
#' @field url.docs model documentation URL
#' @field category model category
#' @field vignette.url vignette URL
#' @field json JSON export
#' @field ljson JSON export
#' @field outcome JSON export
#' @field wrapper JSON export
#' @field explanatory JSON export
#' @field mcunit.test unit testing
#' @field with.feedback Feedback

z <- setRefClass("Zelig", fields = list(fn = "ANY", # R function to call to wrap
                                        formula = "ANY", # Zelig formula
                                        weights = "numeric", 
                                        name = "character", # name of the Zelig model
                                        data = "ANY", # data frame or matrix,
                                        # ddata = "ANY",
                                        # data.by = "ANY", # data frame or matrix
                                        by = "ANY",
                                        mi = "logical",
                                        
                                        idx = "ANY", # model index
                                        
                                        zelig.call = "call", # Zelig function call
                                        model.call = "call", # wrapped function call
                                        zelig.out = "ANY", # estimated zelig model(s)
                                        
                                        setx.out = "ANY", # set values
                                        setx.labels = "list", # pretty-print qi,
                                        bsetx = "logical",
                                        bsetx1 = "logical",
                                        bsetrange = "logical",
                                        bsetrange1 = "logical",
                                        range = "ANY",
                                        range1 = "ANY",

                                        test.statistics = "ANY",
                                        
                                        sim.out = "list", # simulated qi's
                                        simparam = "ANY", # simulated parameters
                                        num = "numeric", # nb of simulations

                                        authors = "character", # Zelig model description
                                        zeligauthors = "character",
                                        modelauthors = "character",
                                        packageauthors = "character",
                                        refs = "ANY", # is there a way to recognize class "bibentry"?,

                                        year = "numeric",
                                        description = "character",
                                        url = "character",
                                        url.docs = "character",
                                        category = "character",
                                        
                                        vignette.url = "character",
                                        
                                        json = "ANY", # JSON export
                                        ljson = "ANY",
                                        outcome = "ANY",
                                        wrapper = "character",
                                        explanatory = "ANY",
                                        
                                        #Unit Testing
                                        mcunit.test = "ANY",
                                        
                                        # Feedback
                                        with.feedback = "logical"))

z$methods(
  initialize = function() {
    .self$authors <- "Kosuke Imai, Gary King, and Olivia Lau"
    .self$zeligauthors <- "Christine Choirat, James Honaker, Kosuke Imai, Gary King, and Olivia Lau"
    .self$refs <- bibentry()
    .self$year <- as.numeric(format(Sys.Date(), "%Y"))
    .self$url <- "http://zeligproject.org/"
    .self$url.docs <- "http://docs.zeligproject.org/en/latest/"
    .self$setx.out <- list()
    .self$setx.labels <- list(ev  = "Expected Values: E(Y|X)",
                              ev1 = "Expected Values: E(Y|X1)",
                              pv  = "Predicted Values: Y|X",
                              pv1 = "Predicted Values: Y|X1",
                              fd  = "First Differences: E(Y|X1) - E(Y|X)")
    .self$bsetx <- FALSE
    .self$bsetx1 <- FALSE
    .self$bsetrange <- FALSE
    .self$bsetrange1 <- FALSE
    # JSON
    .self$vignette.url <- paste(.self$url.docs, tolower(class(.self)[1]), ".html", sep = "")
    .self$vignette.url <- sub("-gee", "gee", .self$vignette.url)
    .self$vignette.url <- sub("-bayes", "bayes", .self$vignette.url)
    # .self$vignette.url <- paste(.self$url.docs, "zelig-", sub("-", "", .self$name), ".html", sep = "")
    .self$explanatory <- c("continuous",
                           "discrete",
                           "nominal",
                           "ordinal",
                           "binary")
    .self$outcome <- ""
    .self$wrapper <- "wrapper"
    # Is 'ZeligFeedback' package installed?
    .self$with.feedback <- "ZeligFeedback" %in% installed.packages()
  }
)

z$methods(
  packagename = function() {
    "Automatically retrieve wrapped package name"
    # If this becomes "quote(mypackage::myfunction) then
    # regmatches(.self$fn,regexpr("(?<=\\()(.*?)(?=\\::)",.self$fn, perl=TRUE))
    # would extract "mypackage"
    return(as.character(.self$fn)[2])
  }
)

z$methods(
  cite = function() {
    "Provide citation information about Zelig and Zelig model, and about wrapped package and wrapped model"
    title <- paste(.self$name, ": ", .self$description, sep="")
    localauthors <- ""
    if (length(.self$modelauthors) & (!identical(.self$modelauthors,""))){   # covers both empty styles: character(0) and "" --the latter being length 1.
        localauthors<-.self$modelauthors
    }else if (length(.self$packageauthors) & (!identical(.self$packageauthors,""))){
        localauthors<-.self$packageauthors
    }else{
        localauthors<-.self$zeligauthors
    }
    cat("How to cite this model in Zelig:\n  ",
    localauthors, ". ", .self$year, ".\n  ", title,
    "\n  in ", .self$zeligauthors,
    ",\n  \"Zelig: Everyone's Statistical Software,\" ",
    .self$url, "\n", sep = "")
  }
)

# Construct a reference list specific to a Zelig model
# Styles available from the bibentry print method: "text", "Bibtex", "citation", "html", "latex", "R", "textVersion"
# The "sphinx" style reformats "text" style with some markdown substitutions

z$methods(
  references = function(style="sphinx") {
    "Construct a reference list specific to a Zelig model."
    mystyle <- style
    if (mystyle=="sphinx"){
        mystyle <- "text"
    }
    mycites<-.self$refs
    if(!is.na(.self$packagename() )){
      mycites<-c(mycites, citation(.self$packagename()))  # Concatentate model specific Zelig references with package references
    }
    mycites<-mycites[!duplicated(mycites)]                            # Remove duplicates (many packages have duplicate references in their lists)
    s<-capture.output(print(mycites, style = mystyle))
    if(style == "sphinx"){                          # format the "text" style conventions for sphinx markdown for building docs for zeligproject.org
      s<-gsub("\\*","\\*\\*",s, perl=TRUE)
      s<-gsub("_","\\*",s, perl=TRUE)
      s<-gsub("\\*\\(","\\* \\(",s, perl=TRUE)
    }
    cat(s, sep="\n")
  }
)

z$methods(
  zelig = function(formula, data, ..., weights = NULL, by) {
    "The zelig command estimates a variety of statistical models."
    fn2 <- function(fc, data) {
      fc$data <- data
      return(fc)
    }
    .self$formula <- formula
    .self$by <- by
    # MI datasets from Amelia
    if (class(data) == "amelia"){
      idata <- data$imputations
      .self$data <- rbind_all(lapply(seq(length(idata)),
                                     function(imputationNumber)
                                       cbind(imputationNumber, idata[[imputationNumber]])))
      .self$by <- c("imputationNumber", by)
      .self$mi <- TRUE
      .self$refs<-c(.self$refs,citation("Amelia"))
    } else {
      .self$data <- data
      .self$mi <- FALSE
    }
    .self$model.call[[1]] <- .self$fn
    .self$model.call$by <- NULL
    if (is.null(.self$by)) {
      .self$data <- cbind(1, .self$data)
      names(.self$data)[1] <- "by"
      .self$by <- "by"
    }
    .self$data <- tbl_df(.self$data)
    .self$zelig.out <- .self$data %>% 
      group_by_(.self$by) %>% 
        do(z.out = eval(fn2(.self$model.call, quote(as.data.frame(.)))))
  }
)

z$methods(
  set = function(...) {
    "Setting Explanatory Variable Values"
    s <-list(...)
    f <- update(.self$formula, 1 ~ .)
    update <- na.omit(.self$data) %>% # remove missing values
      group_by_(.self$by) %>%
      do(mm = model.matrix(f, reduce(dataset = ., s, 
                                     formula = .self$formula,
                                     data = .self$data)))
    return(update)
  }
)

z$methods(
  setx = function(...) {
    .self$bsetx <- TRUE
    .self$setx.out$x  <- .self$set(...)
  }
)

z$methods(
  setx1 = function(...) {
    .self$bsetx1 <- TRUE
    .self$setx.out$x1 <- .self$set(...)
  }
)

z$methods(
  setrange = function(...) {
    .self$bsetrange <- TRUE
    rng <- list()
    s <- list(...)
    m <- expand.grid(s)
    .self$range <- m
    .self$setx.out$range <- list()
    for (i in 1:nrow(m)) {
      l <- as.list(as.list(m[i, ]))
      names(l) <- names(m)
      .self$setx.out$range[[i]] <- .self$set(l)
    }
  }
)

z$methods(
  setrange1 = function(...) {
    .self$bsetrange1 <- TRUE
    rng <- list()
    s <- list(...)
    m <- expand.grid(s)
    .self$range1 <- m
    .self$setx.out$range1 <- list()
    for (i in 1:nrow(m)) {
      l <- as.list(as.list(m[i, ]))
      names(l) <- names(m)
      .self$setx.out$range1[[i]] <- .self$set(l)
    }
  }
)

z$methods(
  sim = function(num = 1000) {
    "Generic Method for Computing and Organizing Simulated Quantities of Interest"
    if (length(.self$num) == 0) 
      .self$num <- num
    .self$simparam <- .self$zelig.out %>%
      do(simparam = .self$param(.$z.out))
    if (.self$bsetx)
      .self$simx()
    if (.self$bsetx1)
      .self$simx1()
    if (.self$bsetrange)
      .self$simrange()
    if (.self$bsetrange1)
      .self$simrange1()
  }
)

z$methods(
  simx = function() {
    d <- plyr::mutate(.self$zelig.out, simparam = .self$simparam$simparam)
    d <- plyr::mutate(d, mm = .self$setx.out$x$mm)
    .self$sim.out$x <-  d %>%
      do(qi = .self$qi(.$simparam, .$mm)) %>%
      do(ev = .$qi$ev, pv = .$qi$pv)
  }
)

z$methods(
  simx1 = function() {
    d <- plyr::mutate(.self$zelig.out, simparam = .self$simparam$simparam)
    d <- plyr::mutate(d, mm = .self$setx.out$x1$mm)
    .self$sim.out$x1 <-  d %>%
      do(qi = .self$qi(.$simparam, .$mm)) %>%
      do(ev = .$qi$ev, pv = .$qi$pv)
    d <- plyr::mutate(.self$sim.out$x1, ev0 = .self$sim.out$x$ev)
    d <- d %>%
      do(fd = .$ev - .$ev0)
    .self$sim.out$x1 <- plyr::mutate(.self$sim.out$x1, fd = d$fd) #JH
  }
)

z$methods(
  simrange = function() {
    .self$sim.out$range <- list()
    for (i in 1:nrow(.self$range)) {
      d <- plyr::mutate(.self$zelig.out, simparam = .self$simparam$simparam)
      d <- plyr::mutate(d, mm = .self$setx.out$range[[i]]$mm)
      .self$sim.out$range[[i]] <-  d %>%
        do(qi = .self$qi(.$simparam, .$mm)) %>%
        do(ev = .$qi$ev, pv = .$qi$pv)
    }
  }
)

z$methods(
  simrange1 = function() {
    .self$sim.out$range1 <- list()
    for (i in 1:nrow(.self$range1)) {
      d <- plyr::mutate(.self$zelig.out, simparam = .self$simparam$simparam)
      d <- plyr::mutate(d, mm = .self$setx.out$range1[[i]]$mm)
      .self$sim.out$range1[[i]] <-  d %>%
        do(qi = .self$qi(.$simparam, .$mm)) %>%
        do(ev = .$qi$ev, pv = .$qi$pv)
    }
  }
)

z$methods(
  show = function() {
    "Display a Zelig object"
    if ("uninitializedField" %in% class(.self$zelig.out))
      cat("Next step: Use 'zelig' method")
    else if (length(.self$setx.out) == 0) {
      summ <- .self$zelig.out %>%
        do(summ = {cat("Model: \n")
                   if (length(.self$by) == 1) {
                     if (.self$by == "by") {
                       cat()
                     }
                     else {
                       print(.[.self$by])
                     }
                   } else {
                     print(.[.self$by])
                   }
                   print(base::summary(.$z.out))
                   })
        
      if(.self$mi){
        cat("Model: Combined Imputations \n")
        vcovlist <-.self$getvcov()
        coeflist <-.self$getcoef()
        am.m<-length(coeflist)
        am.k<-length(coeflist[[1]])
        q <- matrix(unlist(coeflist), nrow=am.m, ncol=am.k, byrow=TRUE)
        se <- matrix(NA, nrow=am.m, ncol=am.k)
        for(i in 1:am.m){
          se[i,]<-sqrt(diag(vcovlist[[i]]))
        }
        ones <- matrix(1, nrow = 1, ncol = am.m)
        imp.q <- (ones %*% q)/am.m        # Slightly faster than "apply(b,2,mean)"
        ave.se2 <- (ones %*% (se^2))/am.m # Similarly, faster than "apply(se^2,2,mean)"
        diff <- q - matrix(1, nrow = am.m, ncol = 1) %*% imp.q
        sq2 <- (ones %*% (diff^2))/(am.m - 1)
        imp.se <- sqrt(ave.se2 + sq2 * (1 + 1/am.m))
            
        Estimate<-as.vector(imp.q)
        Std.Error<-as.vector(imp.se)
        zvalue<-Estimate/Std.Error
        Pr.z<-2*(1-pnorm(abs(zvalue)))
        stars<-rep("",am.k)
        stars[Pr.z<.05]<-"."
        stars[Pr.z<.01]<-"*"
        stars[Pr.z<.001]<-"**"
        stars[Pr.z<.0001]<-"***"

        results<-data.frame(Estimate,Std.Error,zvalue,Pr.z,stars,row.names=names(coeflist[[1]]))
        names(results)<-c("Estimate","Std.Error","z value","Pr(>|z|)","")
        print(results, digits=max(3, getOption("digits") - 3))
        cat("---\nSignif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n")
        cat("\n")
      }
      
      if(!is.null(.self$test.statistics$gim.criteria)){
          if(.self$test.statistics$gim.criteria){
              cat("According to the GIM-rule-of-thumb, your model probably has some type of specification error.\n",
              "We suggest you run model diagnostics and seek to fix the problem.\n",
              "You may also wish to run the full GIM test (which takes more time) to be sure.\n",
              "See http://.... for more information.\n \n")
          }
      }
      
      cat("Next step: Use 'setx' method\n")
    } else if (length(.self$setx.out) != 0 & length(.self$sim.out) == 0) {
      niceprint<-function(obj, name){
        if(!is.null(obj[[1]])){
          cat(name,":\n", sep="")
          screenoutput<-obj[[1]]
          attr(screenoutput,"assign")<-NULL
          print(screenoutput, digits=max(3, getOption("digits") - 3))
        }
      }
      niceprint(obj=.self$setx.out$x$mm, name="setx")
      niceprint(obj=.self$setx.out$x1$mm, name="setx1")
      niceprint(obj=.self$setx.out$range[[1]]$mm, name="range")
      niceprint(obj=.self$setx.out$range1[[1]]$mm, name="range1")
      cat("\nNext step: Use 'sim' method\n")
    } else { # sim.out
      pstat <- function(s.out, what = "sim x") {
        simu <- s.out %>%
          do(simu = {cat("\n", what, ":\n")
                     cat(" -----\n")
                     cat("ev\n")
                     print(stat(.$ev, .self$num))
                     cat("pv\n")
                     print(stat(.$pv, .self$num))
                     if (!is.null(.$fd)) {
                       cat("fd\n")
                       print(stat(.$fd, .self$num))}
          }
          )
      }
      pstat(.self$sim.out$x)
      pstat(.self$sim.out$x1, "sim x1")
      if (!is.null(.self$setx.out$range)) {
        for (i in seq(.self$sim.out$range)) {
          cat("\n")
          print(.self$range[i, ])
          cat("\n")
          pstat(.self$sim.out$range[[i]], "sim range")
          cat("\n")
        }
      }
      if (!is.null(.self$setx.out$range1)) {
        for (i in seq(.self$sim.out$range1)) {
          cat("\n")
          print(.self$range1[i, ])
          cat("\n")
          pstat(.self$sim.out$range1[[i]], "sim range")
          cat("\n")
        }
      }
    }
  }
)

z$methods(
  graph = function() {
    "Plot the quantities of interest"
    plot.qi(.self)
  }
)

z$methods(
  summarize = function(...) {
    "Display a Zelig object"
    show(...)
  }
)

z$methods(
  summarise = function(...) {
    "Display a Zelig object"
    summarize(...)
  }
)

z$methods(
  help = function() {
    "Open the model vignette from http://zeligproject.org/"
#     vignette(class(.self)[1])
    browseURL(.self$vignette.url)
  } 
)

z$methods(
  getcoef = function() {
    "Get estimated model coefficients"
    result <- try(lapply(.self$zelig.out$z.out, coef), silent = TRUE)
    if ("try-error" %in% class(result))
      stop("'coef' method' not implemented for model '", .self$name, "'")
    else
      return(result)
  } 
)

z$methods(
  getvcov = function() {
    "Get estimated model variance-covariance matrix"
    result <- lapply(.self$zelig.out$z.out, vcov)
    if ("try-error" %in% class(result))
      stop("'vcov' method' not implemented for model '", .self$name, "'")
    else
      return(result)
  }
)

z$methods(
  getpredict = function() {
    "Get predcted values"
    result <- lapply(.self$zelig.out$z.out, predict)
    if ("try-error" %in% class(result))
      stop("'predict' method' not implemented for model '", .self$name, "'")
    else
      return(result)
  }
)

z$methods(
  toJSON = function() {
    "Convert Zelig object to JSON format"
    if (!is.list(.self$json))
      .self$json <- list()
    .self$json$"name" <- .self$name
    .self$json$"description" <- .self$description
    .self$json$"outcome" <- list(modelingType = .self$outcome)
    .self$json$"explanatory" <- list(modelingType = .self$explanatory)
    .self$json$"vignette.url" <- .self$vignette.url
    .self$json$"wrapper" <- .self$wrapper
    tree <- c(class(.self)[1], .self$.refClassDef@refSuperClasses)
    .self$json$tree <- head(tree, match("Zelig", tree) - 1)
    .self$ljson <- .self$json
    .self$json <- jsonlite::toJSON(json, pretty = TRUE)
    return(.self$json)
  }
)

z$methods(
  mcunit.init = function(nsim, minx, maxx) {
    "Monte Carlo Unit Test"
    cat("\nRunning Monte Carlo Unit Test...", sep="")
    x.sim <- runif(nsim, minx, maxx)
    x.seq <- seq(from = minx, to = maxx, length = nsim)
    return(data.frame(cbind(x.sim, x.seq)))
  }
)

z$methods(
  test = function(z, data) {
    "Monte Carlo Unit Test"
    survival = c("Zelig-weibull", "Zelig-exp")
    if(class(z)[1] %in% survival){
      z$zelig(Surv(time.sim, event) ~ x.sim, data = data)
    }
    
    else{
      z$zelig(y.sim ~ x.sim, data = data)
    }
    #     z$setrange(x.sim = data$x.seq)    
    #     z$sim(num = nrow(data))
    return(z)
    
    #     ev <- c()
    # 
    #     for(i in 1:k){
    #       z$sim(num = nrow(data))
    #       ev <- append(ev, z$sim.out$x$ev)
    #     }
    #     
    #     return(ev)
    
    
    
    #     ev <- c()
    #     for(j in 1:nrow(data)){
    #       foo <- lapply(z$sim.out$range[j][[1]]$ev, mean)
    #       ev <- append(ev, foo[[1]])  
    #     }
    #     
    #     # Kolmogorov–Smirnov test
    #     kstest <- ks.test(ev, data$y.true)
    #     .self$mcunit.test <- if(kstest$p.value > .1) 'PASS' else 'FAIL'
    #     if(kstest$p.value < .05) {
    #       cat("\nFailed K-S Test.", sep = "")
    #     } else {
    #       cat("\nPassed K-S Test.")
    #     }
    #     
    #     output = list(
    #       kstest = kstest,
    #       data = data
    #     )
    #     
    #     return(output)
  }
)

z$methods(
  feedback = function() {
    "Send feedback to the Zelig team"
    if (!.self$with.feedback)
      return("ZeligFeedback package not installed")
    # If ZeligFeedback is installed
    print("ZeligFeedback package installed")
    print(ZeligFeedback::feedback(.self))
  }
)

z$methods(
  finalize = function() {
    if (!.self$with.feedback)
      return("ZeligFeedback package not installed")
    # If ZeligFeedback is installed
    print("Thanks for providing Zelig usage information")
    # print(ZeligFeedback::feedback(.self))
    write(paste("feedback", ZeligFeedback::feedback(.self)),
          file = paste0("test-zelig-finalize-", date(), ".txt"))
  }
)

setMethod("summary", "Zelig",
          function(object, ...) {
            object$summarize()
          }
)

setMethod("plot", "Zelig",
          function(x, ...) {
            x$graph()
          }
)

setMethod("vcov", "Zelig",
          function(object, ...) {
            object$getvcov()
          }
)

setMethod("coef", "Zelig",
          function(object, ...) {
            object$getcoef()
          }
)

setMethod("predict", "Zelig",
          function(object, ...) {
            object$getpredict()
          }
)

#       idx <- match(names(.self$setx.labels),
#                    names(.self$sim.out),
#                    nomatch = 0) 
#       names(.self$sim.out)[idx] <- .self$setx.labels[idx != 0]

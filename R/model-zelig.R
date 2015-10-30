#' Zelig reference class
#'
#' Zelig website: \url{http://zeligproject.org/}
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
                                        weights = "ANY", 
                                        acceptweights = "logical",
                                        name = "character", # name of the Zelig model
                                        data = "ANY", # data frame or matrix,
                                        originaldata = "ANY", # data frame or matrix,
                                        originalweights = "ANY",
                                        # ddata = "ANY",
                                        # data.by = "ANY", # data frame or matrix
                                        by = "ANY",
                                        mi = "logical",
                                        matched = "logical",
                                        
                                        idx = "ANY", # model index
                                        
                                        zelig.call = "call", # Zelig function call
                                        model.call = "call", # wrapped function call
                                        zelig.out = "ANY", # estimated zelig model(s)
                                        signif.stars = "logical",
                                        signif.stars.default = "logical", # significance stars default
                                        
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
                                        mcformula = "ANY",
                                        
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
    .self$acceptweights <- FALSE
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
  zelig = function(formula, data, ..., weights=NULL, by) {
    "The zelig command estimates a variety of statistical models"
    fn2 <- function(fc, data) {
      fc$data <- data
      return(fc)
    }
    .self$formula <- formula
    # Overwrite formula with mc unit test formula into correct environment, if it exists
    # Requires fixing R scoping issue
    if("formula" %in% class(.self$mcformula)){
      .self$formula <- as.formula( deparse(.self$mcformula), env=environment(.self$formula) )
      .self$model.call$formula <- as.formula( deparse(.self$mcformula), env=globalenv() )
    }else if(is.character(.self$mcformula)) {
      .self$formula <- as.formula( .self$mcformula, env=environment(.self$formula) )
      .self$model.call$formula <- as.formula( .self$mcformula, env=globalenv() )
    }
    
    .self$by <- by
    .self$originaldata <- data
    .self$originalweights <- weights
    datareformed <- FALSE

    # Matched datasets from MatchIt
    if ("matchit" %in% class(data)){
      idata <- MatchIt::match.data(data) 
      iweights <- idata$weights

      .self$matched <- TRUE
      .self$data <- idata
      datareformed <- TRUE

      # Check if noninteger valued weights exist and are incompatible with zelig model
      validweights <- TRUE
      if(!.self$acceptweights){           # This is a convoluted way to do this, but avoids the costly "any()" calculation if not necessary
      	if(any(iweights != ceiling(iweights))){  # any(y != ceiling(y)) tests slightly faster than all(y == ceiling(y))
      		validweights <- FALSE
      	}
      }
      if(!validweights){   # could also be  if((!acceptweights) & (any(iweights != ceiling(iweights))  but avoid the long any for big datasets
      	cat("The weights created by matching for this dataset have noninteger values,\n",
             "however, the statistical model you have chosen is only compatible with integer weights.\n",
             "Either change the matching method (such as to `optimal' matching with a 1:1 ratio)\n",
             "or change the statistical model in Zelig.\n",
             "We will round matching weights up to integers to proceed.\n\n")
      	.self$weights <- ceiling(iweights)
      }else{
        .self$weights <- iweights
      }

      # Set references appropriate to matching methods used 
      .self$refs <- c(.self$refs, citation("MatchIt"))
      if(m.out$call$method=="cem" & ("cem" %in% installed.packages())) .self$refs <- c(.self$refs, citation("cem"))
      #if(m.out$call$method=="exact") .self$refs <- c(.self$refs, citation(""))
      if((m.out$call$method=="full") & ("optmatch" %in% installed.packages())) .self$refs <- c(.self$refs, citation("optmatch"))
      if(m.out$call$method=="genetic" & ("Matching" %in% installed.packages())) .self$refs <- c(.self$refs, citation("Matching"))
      #if(m.out$call$method=="nearest") .self$refs <- c(.self$refs, citation(""))
      if(m.out$call$method=="optimal" & ("optmatch" %in% installed.packages())) .self$refs <- c(.self$refs, citation("optmatch"))
      #if(m.out$call$method=="subclass") .self$refs <- c(.self$refs, citation(""))
    } else {
      .self$matched  <- FALSE
    }
    # Multiply Imputed datasets from Amelia  
    # Notice Amelia objects ignore weights currently, which is reasonable as the Amelia package ignores weights
    if ("amelia" %in% class(data)){
      idata <- data$imputations
      .self$data <- rbind_all(lapply(seq(length(idata)),
                                     function(imputationNumber)
                                       cbind(imputationNumber, idata[[imputationNumber]])))
      .self$weights <- NULL  # This should be considered or addressed
      datareformed <- TRUE
      .self$by <- c("imputationNumber", by)
      .self$mi <- TRUE
      .self$refs<-c(.self$refs, citation("Amelia"))
    } else {
      .self$mi <- FALSE
    }

    if (!datareformed){
      .self$data <- data  # If none of the above package integrations have already reformed the data from another object, use the supplied data

      # Run some checking on weights argument, and see if is valid string or vector
      if(!is.null(weights)){
      	if(is.character(weights)){
      		if(weights %in% names(.self$data)){
      			.self$weights <- .self$data[[weights]]  # This is a way to convert data.frame portion to type numeric (as data.frames are lists)
      		}else{
      			cat("Variable name given for weights not found in dataset, so will be ignored.\n\n")
      			.self$weights <- NULL  # No valid weights
                .self$model.call$weights <- NULL
      		}
      	}else if(is.vector(weights)){
      		if(length(weights)==nrow(.self$data) & is.vector(weights)){
      			if(min(weights)<0){
      				weights[weights < 0] <- 0
      				cat("Negative valued weights were supplied and will be replaced with zeros.")
      			}
      			.self$weights <- weights # Weights 
      		}else{
      			cat("Length of vector given for weights is not equal to number of observations in dataset, and will be ignored.\n\n")
      			.self$weights <- NULL # No valid weights
                .self$model.call$weights <- NULL
      		}
      	}else{
      		cat("Supplied weights argument is not a vector or a variable name in the dataset, and will be ignored.\n\n")
      		.self$weights <- NULL # No valid weights
            .self$model.call$weights <- NULL
      	}
      }else{
        .self$weights <- NULL  # No weights set, so weights are NULL
      }
    } 

    # If the Zelig model does not not accept weights, but weights are provided, we rebuild the data 
    #   by bootstrapping using the weights as probabilities
    #   or by duplicating rows proportional to the ceiling of their weight
    # Otherwise we pass the weights to the model call  
    if(!is.null(.self$weights)){
      if ((!.self$acceptweights)){
        .self$buildDataByWeights2()  # Could use alternative method $buildDataByWeights() for duplication approach.  Maybe set as argument?\
        .self$model.call$weights <- NULL
	  } else {
		.self$model.call$weights <- .self$weights   # NEED TO CHECK THIS IS THE NAME FOR ALL MODELS, or add more generic field containing the name for the weights argument
	  }
	}

    .self$model.call[[1]] <- .self$fn
    .self$model.call$by <- NULL
    if (is.null(.self$by)) {
      .self$data <- cbind(1, .self$data)
      names(.self$data)[1] <- "by"
      .self$by <- "by"
    }

    #cat("zelig.call:\n")
    #print(.self$zelig.call)
    #cat("model.call:\n")
    #print(.self$model.call)
    .self$data <- tbl_df(.self$data)
    #.self$zelig.out <- eval(fn2(.self$model.call, quote(as.data.frame(.)))) # shortened test version that bypasses "by"
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
    # update <- na.omit(.self$data) %>% # remove missing values
    update <- .self$data %>%
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
  show = function(signif.stars = FALSE) {
    "Display a Zelig object"
    .self$signif.stars <- signif.stars
    .self$signif.stars.default <- getOption("show.signif.stars")
    options(show.signif.stars = .self$signif.stars)
    if ("uninitializedField" %in% class(.self$zelig.out))
      cat("Next step: Use 'zelig' method")
    else if (length(.self$setx.out) == 0) {
      #############################################################################	
      # Current workaround to display call as $zelig.call rather than $model.call
      # should improve this approach in future:
      for(jj in 1:length(.self$zelig.out$z.out)){
      	.self$zelig.out$z.out[[jj]]$call <- .self$zelig.call
      }	
      #############################################################################
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
        cat("---\nSignif. codes:  '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
        cat("\n")
      }
      
      if(!is.null(.self$test.statistics$gim.criteria)){
          if(.self$test.statistics$gim.criteria){
#               cat("According to the GIM-rule-of-thumb, your model probably has some type of specification error.\n",
#               "We suggest you run model diagnostics and seek to fix the problem.\n",
#               "You may also wish to run the full GIM test (which takes more time) to be sure.\n",
#               "See http://.... for more information.\n \n")
            cat("Statistical Warning: The GIM test suggests this model is misspecified\n",
                "(based on comparisons between classical and robust SE's; see http://j.mp/GIMtest).\n",
                "We suggest you run diagnostics to ascertain the cause, respecify the model\n",
                "and run it again.\n\n")
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
    options(show.signif.stars = .self$signif.stars.default)
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
  getfitted = function() {
    result <- lapply(.self$zelig.out$z.out, fitted)
    if ("try-error" %in% class(result))
      stop("'predict' method' not implemented for model '", .self$name, "'")
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

# empty default data generating process to avoid error if not created as model specific method
z$methods(
  mcfun = function(x, ...){
    return( rep(1,length(x)) )
  }
)

# Monte Carlo unit test
z$methods(
  mcunit = function(nsim=500, minx=-2, maxx=2, b0=0, b1=1, alpha=1, ci=0.95, plot = TRUE, ...){
    
    passes <- TRUE
    n.short <- 10      # number of p
    alpha.ci <- 1-ci   # alpha values for ci bounds, not speed parameter
    x.sim <- runif(n=nsim, min=minx, max=maxx)
    x.seq <- seq(from=minx, to=maxx, length = nsim)
    
    data.hat <- .self$mcfun(x=x.seq, b0=b0, b1=b1, alpha=alpha, ..., sim=FALSE)
    if(!is.data.frame(data.hat)){
        data.hat<-data.frame(x.seq=x.seq, y.hat=data.hat)
    }
    data.sim <- .self$mcfun(x=x.sim, b0=b0, b1=b1, alpha=alpha, ..., sim=TRUE)
    if(!is.data.frame(data.sim)){
        data.sim<-data.frame(x.sim=x.sim, y.sim=data.sim)
    }

    ## Estimate Zelig model and create numerical bounds on expected values
    # This should be the solution, but requires fixing R scoping issue:
    #.self$zelig(y.sim~x.sim, data=data.sim)      # formula will be overwritten in zelig() if .self$mcformula has been set
    
    ## Instead, remove formula field and set by hard code
    .self$mcformula <- NULL
    if(.self$name %in% c("exp","weibull","lognorm")){
      .self$zelig(Surv(y.sim,event)~x.sim, data=data.sim)
    }else{
      .self$zelig(y.sim~x.sim, data=data.sim)
    }
    
    x.short.seq<-seq(from=minx, to=maxx, length=n.short)
    .self$setrange(x.sim=x.short.seq)
    .self$sim()
    
    data.short.hat <- .self$mcfun(x=x.short.seq, b0=b0, b1=b1, alpha=alpha, ..., sim=FALSE)
    if(!is.data.frame(data.short.hat)){
        data.short.hat<-data.frame(x.seq=x.short.seq, y.hat=data.short.hat)
    }
    
    history.ev <- history.pv <- matrix(NA, nrow=n.short, ncol=2)
    for(i in 1:n.short){
        xtemp<-x.short.seq[i]
        .self$setx(x.sim=xtemp)
        .self$sim()
        #temp<-sort( .self$sim.out$x$ev[[1]] )
        temp<-sort( .self$sim.out$range[[i]]$ev[[1]] )
        
        #calculate bounds of expected values
        history.ev[i,1]<-temp[max(round(length(temp)*(alpha.ci/2)),1) ]     # Lower ci bound
        history.ev[i,2]<-temp[round(length(temp)*(1 - (alpha.ci/2)))]       # Upper ci bound
        #temp<-sort( .self$sim.out$x$pv[[1]] )
        temp<-sort( .self$sim.out$range[[i]]$pv[[1]] )
        
        #check that ci contains true value
        passes <- passes & (min(history.ev[i,]) <= data.short.hat$y.hat[i] ) & (max(history.ev[i,]) >= data.short.hat$y.hat[i] )
        
        #calculate bounds of predicted values
        history.pv[i,1]<-temp[max(round(length(temp)*(alpha.ci/2)),1) ]     # Lower ci bound
        history.pv[i,2]<-temp[round(length(temp)*(1 - (alpha.ci/2)))]       # Upper ci bound
    }
    
    ## Plot Monte Carlo Data
    if(plot){
      all.main = substitute(
        paste(modelname, "(", beta[0], "=", b0, ", ", beta[1], "=", b1,",", alpha, "=", a0, ")"),
        list(modelname = .self$name, b0 = b0, b1=b1, a0 = alpha)
      )
    
      all.ylim<-c( min(c(data.sim$y.sim, data.hat$y.hat)) , max(c(data.sim$y.sim, data.hat$y.hat)) )
    
      plot(data.sim$x.sim, data.sim$y.sim, main=all.main, ylim=all.ylim, xlab="x", ylab="y", col="steelblue")
      par(new=TRUE)
      plot(data.hat$x.seq, data.hat$y.hat, main="", ylim=all.ylim, xlab="", ylab="", xaxt="n", yaxt="n", type="l", col="green", lwd=2)
  
      for(i in 1:n.short){
        lines(x=rep(x.short.seq[i],2), y=c(history.pv[i,1],history.pv[i,2]), col="lightpink", lwd=1.6)
        lines(x=rep(x.short.seq[i],2), y=c(history.ev[i,1],history.ev[i,2]), col="firebrick", lwd=1.6)
      }
    }
    return(passes)
    
  }
)

# rebuild dataset by duplicating observations by (rounded) weights
z$methods(
  buildDataByWeights = function() {
    if(!.self$acceptweights){
      idata <- .self$data
      iweights <- .self$weights
      ceilweights <- ceiling(iweights)
      n.obs <- nrow(idata)
      windex <- rep(1:n.obs, ceilweights)
      idata <- idata[windex,]
      .self$data <- idata
      if(any(iweights != ceiling(iweights))){
        cat("Noninteger weights were set, but the model in Zelig is only able to use integer valued weights.\n",
      	   "Each weight has been rounded up to the nearest integer.\n\n")
  	  }
    }
  }
)

# rebuild dataset by bootstrapping using weights as probabilities
z$methods(
  buildDataByWeights2 = function() {
    if(!.self$acceptweights){
      iweights <- .self$weights
      if(any(iweights != ceiling(iweights))){
        cat("Noninteger weights were set, but the model in Zelig is only able to use integer valued weights.\n",
      	   "A bootstrapped version of the dataset was constructed using the weights as sample probabilities.\n\n")
        idata <- .self$data	
        n.obs <- nrow(idata)
        n.w   <- sum(iweights)
        iweights <- iweights/n.w
        windex <- sample(x=1:n.obs, size=n.w, replace=TRUE, prob=iweights)
        idata <- idata[windex,]
        .self$data <- idata
      }else{
         .self$buildDataByWeights()  # If all weights are integers, just use duplication to rebuild dataset.
  	  }
    }
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

# z$methods(
#   finalize = function() {
#     if (!.self$with.feedback)
#       return("ZeligFeedback package not installed")
#     # If ZeligFeedback is installed
#     print("Thanks for providing Zelig usage information")
#     # print(ZeligFeedback::feedback(.self))
#     write(paste("feedback", ZeligFeedback::feedback(.self)),
#           file = paste0("test-zelig-finalize-", date(), ".txt"))
#   }
# )

setMethod("summary", "Zelig",
          function(object, ...) {
            object$summarize(...)
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

setMethod("fitted", "Zelig",
          function(object, ...) {
            object$getfitted()
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

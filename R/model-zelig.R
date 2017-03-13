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
#' @field weights [forthcoming]
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

                                        avg = "ANY",

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
                                        setforeveryby = "logical",

                                        test.statistics = "ANY",

                                        sim.out = "list", # simulated qi's
                                        simparam = "ANY", # simulated parameters
                                        num = "numeric", # nb of simulations
                                        bootstrap = "logical", # use bootstrap
                                        bootstrap.num = "numeric", # number of bootstraps to use

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
    .self$zeligauthors <- "Christine Choirat, Christopher Gandrud, James Honaker, Kosuke Imai, Gary King, and Olivia Lau"
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

    .self$bootstrap <- FALSE
    .self$bootstrap.num <- 100
    # JSON
    .self$vignette.url <- paste(.self$url.docs, tolower(class(.self)[1]), ".html", sep = "")
    .self$vignette.url <- sub("-gee", "gee", .self$vignette.url)
    .self$vignette.url <- sub("-bayes", "bayes", .self$vignette.url)
    # .self$vignette.url <- paste(.self$url.docs, "zelig-", sub("-", "", .self$name), ".html", sep = "")
    .self$category <- "undefined"
    .self$explanatory <- c("continuous",
                           "discrete",
                           "nominal",
                           "ordinal",
                           "binary")
    .self$outcome <- ""
    .self$wrapper <- "wrapper"
    # Is 'ZeligFeedback' package installed?
    .self$with.feedback <- "ZeligFeedback" %in% installed.packages()
    .self$setforeveryby <- TRUE

    .self$avg <- function(val) {
      if (is.numeric(val))
        mean(val)
      else if (is.ordered(val))
        Median(val)
      else
        Mode(val)
    }
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
  zelig = function(formula, data, model = NULL, ..., weights = NULL, by,
                   bootstrap = FALSE) {
    "The zelig function estimates a variety of statistical models"

    fn2 <- function(fc, data) {
      fc$data <- data
      return(fc)
    }

    .self$formula <- formula

    # Convert factors converted internally to the zelig call
    if (transformer(formula, FUN = 'as.factor', check = TRUE)) {
      localformula <- transformer(formula, data, FUN = 'as.factor',
                                  f_out = TRUE)
      localdata <- transformer(formula, data, FUN = 'as.factor', d_out = TRUE)
      .self$formula <- localformula
      .self$data <- localdata
    }

    # Convert natural logs converted internally to the zelig call
    if (transformer(formula, FUN = 'log', check = TRUE)) {
      localformula <- transformer(formula, data, FUN = 'log',
                                  f_out = TRUE)
      localdata <- transformer(formula, data, FUN = 'log', d_out = TRUE)
      .self$formula <- localformula
      .self$data <- localdata
    }

    # Overwrite formula with mc unit test formula into correct environment, if it exists
    # Requires fixing R scoping issue
    if("formula" %in% class(.self$mcformula)){
      .self$formula <- as.formula( deparse(.self$mcformula),
                                   env = environment(.self$formula) )
      .self$model.call$formula <- as.formula( deparse(.self$mcformula),
                                              env = globalenv() )
    } else if(is.character(.self$mcformula)) {
      .self$formula <- as.formula( .self$mcformula,
                                   env = environment(.self$formula) )
      .self$model.call$formula <- as.formula( .self$mcformula, env = globalenv() )
    }
    if(!is.null(model)){
      cat("Argument model is only valid for the Zelig wrapper, but not the Zelig method, and will be ignored.\n")
      flag <- !(names(.self$model.call) == "model")
      .self$model.call <- .self$model.call[flag]
      flag <- !(names(.self$zelig.call) == "model")
      .self$zelig.call <- .self$zelig.call[flag]
    }

    .self$by <- by
    .self$originaldata <- data
    .self$originalweights <- weights
    datareformed <- FALSE

    if(is.numeric(bootstrap)){
      .self$bootstrap <- TRUE
      .self$bootstrap.num <- bootstrap
    } else if(is.logical(bootstrap)){
      .self$bootstrap <- bootstrap
    }
    # Remove bootstrap argument from model call
    .self$model.call$bootstrap <- NULL
    # Check if bootstrap possible by checking whether param method has method argument available
    if(.self$bootstrap){
      if(!("method" %in% names(formals(.self$param)))){
        stop("The bootstrap does not appear to be implemented for this Zelig model. Check that the param() method allows point predictions.")
      }
      .self$setforeveryby <- FALSE  # compute covariates in set() at the dataset-level
    }


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
      } else {
        .self$weights <- iweights
      }

      # Set references appropriate to matching methods used
      .self$refs <- c(.self$refs, citation("MatchIt"))
      if(m.out$call$method=="cem" & ("cem" %in% installed.packages()))
        .self$refs <- c(.self$refs, citation("cem"))
      #if(m.out$call$method=="exact") .self$refs <- c(.self$refs, citation(""))
      if((m.out$call$method=="full") & ("optmatch" %in% installed.packages()))
        .self$refs <- c(.self$refs, citation("optmatch"))
      if(m.out$call$method=="genetic" & ("Matching" %in% installed.packages()))
        .self$refs <- c(.self$refs, citation("Matching"))
      #if(m.out$call$method=="nearest") .self$refs <- c(.self$refs, citation(""))
      if(m.out$call$method=="optimal" & ("optmatch" %in% installed.packages()))
        .self$refs <- c(.self$refs, citation("optmatch"))
      #if(m.out$call$method=="subclass") .self$refs <- c(.self$refs, citation(""))
    } else {
      .self$matched  <- FALSE
    }
    # Multiply Imputed datasets from Amelia or mi utility
    # Notice imputed objects ignore weights currently, which is reasonable as the Amelia package ignores weights
    if (("amelia" %in% class(data)) | ("mi" %in% class(data))){
      if ("amelia" %in% class(data)){
        idata <- data$imputations
      }else{
        idata <- data
      }

      .self$data <- bind_rows(lapply(seq(length(idata)),
                                     function(imputationNumber)
                                       cbind(imputationNumber, idata[[imputationNumber]])))
      .self$weights <- NULL  # This should be considered or addressed
      datareformed <- TRUE
      .self$by <- c("imputationNumber", by)
      .self$mi <- TRUE
      .self$setforeveryby <- FALSE  # compute covariates in set() at on the entire stacked dataset
      .self$refs <- c(.self$refs, citation("Amelia"))
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
          } else {
            warning("Variable name given for weights not found in dataset, so will be ignored.\n\n", .call=FALSE)
            .self$weights <- NULL  # No valid weights
            .self$model.call$weights <- NULL
          }
        } else if(is.vector(weights)){
          if(length(weights)==nrow(.self$data) & is.vector(weights)){
            localWeights <- weights # avoids CRAN warning about deep assignment from weights existing separately as argument and field
            if(min(localWeights)<0){
              localWeights[localWeights < 0] <- 0
              warning("Negative valued weights were supplied and will be replaced with zeros.", .call=FALSE)
            }
            .self$weights <- localWeights # Weights
          } else{
            warning("Length of vector given for weights is not equal to number of observations in dataset, and will be ignored.\n\n", .call=FALSE)
            .self$weights <- NULL # No valid weights
            .self$model.call$weights <- NULL
          }
        } else {
          warning("Supplied weights argument is not a vector or a variable name in the dataset, and will be ignored.\n\n", .call=FALSE)
          .self$weights <- NULL # No valid weights
          .self$model.call$weights <- NULL
        }
      } else {
        .self$weights <- NULL  # No weights set, so weights are NULL
        .self$model.call$weights <- NULL
      }
    }

    # If the Zelig model does not not accept weights, but weights are provided, we rebuild the data
    #   by bootstrapping using the weights as probabilities
    #   or by duplicating rows proportional to the ceiling of their weight
    # Otherwise we pass the weights to the model call
    if(!is.null(.self$weights)){
      if ((!.self$acceptweights)){
        .self$buildDataByWeights2()   # Could use alternative method $buildDataByWeights() for duplication approach.  Maybe set as argument?\
        .self$model.call$weights <- NULL
      } else {
        .self$model.call$weights <- .self$weights   # NEED TO CHECK THIS IS THE NAME FOR ALL MODELS, or add more generic field containing the name for the weights argument
      }
    }

    if (.self$bootstrap){
      .self$buildDataByBootstrap()
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
  set = function(..., fn = list(numeric = mean, ordered = Median)) {
    "Setting Explanatory Variable Values"
    is_uninitializedField(.self$zelig.out)

    .self$avg <- function(val) {
      if (is.numeric(val))
        ifelse(is.null(fn$numeric), mean(val), fn$numeric(val))
      else if (is.ordered(val))
        ifelse(is.null(fn$ordered), Median(val), fn$ordered(val))
      else
        Mode(val)
    }
    s <- list(...)

    # This eliminates warning messages when factor rhs passed to lm() model in reduce() utility function
    if(.self$category=="multinomial"){  # Perhaps find more robust way to test if dep.var. is factor
      f2 <- update(.self$formula, as.numeric(.) ~ .)
    } else {
      f2 <- .self$formula
    }
    f <- update(.self$formula, 1 ~ .)
    # update <- na.omit(.self$data) %>% # remove missing values

    # compute on each slice of the dataset defined by "by"
    if(.self$setforeveryby){
      update <- .self$data %>%
        group_by_(.self$by) %>%
        do(mm = model.matrix(f, reduce(dataset = "MEANINGLESS ARGUMENT", s,
                                       formula = f2,
                                       data = ., avg = .self$avg))) # fix in last argument from data=.self$data to data=.  (JH)

      # compute over the entire dataset  - currently used for mi and bootstrap.  Should be opened up to user.
    } else {
      if(.self$bootstrap){
        flag <- .self$data$bootstrapIndex == (.self$bootstrap.num + 1) # These are the original observations
        tempdata <- .self$data[flag,]
      } else {
        tempdata <- .self$data # presently this is for mi.  And this is then the entire stacked dataset.
      }

      allreduce <- reduce(dataset = "MEANINGLESS ARGUMENT", s,
                          formula = f2,
                          data = tempdata,
                          avg = .self$avg)
      allmm <- model.matrix(f, allreduce)
      update <- .self$data %>%
        group_by_(.self$by) %>%
        do(mm = allmm)
    }
    return(update)
  }
)

z$methods(
  setx = function(..., fn = list(numeric = mean, ordered = Median,
                                 other = Mode)) {
    is_uninitializedField(.self$zelig.out)

    .self$bsetx <- TRUE
    .self$setx.out$x  <- .self$set(..., fn = fn)
  }
)

z$methods(
  setx1 = function(..., fn = list(numeric = mean, ordered = Median, other = Mode)) {
    .self$bsetx1 <- TRUE
    .self$setx.out$x1 <- .self$set(...)
  }
)

z$methods(
  setrange = function(..., fn = list(numeric = mean, ordered = Median, other = Mode)) {
    is_uninitializedField(.self$zelig.out)

    .self$bsetrange <- TRUE
    rng <- list()
    s <- list(...)
    m <- expand_grid_setrange(s)
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
  setrange1 = function(..., fn = list(numeric = mean, ordered = Median, other = Mode)) {
    .self$bsetrange1 <- TRUE
    rng <- list()
    s <- list(...)
    m <- expand_grid_setrange(s)
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
  param = function(z.out, method="mvn") {
    if(identical(method,"mvn")){
      return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
    } else if(identical(method,"point")){
      return(t(as.matrix(coef(z.out))))
    } else {
      stop("param called with method argument of undefined type.")
    }
  }
)

z$methods(
  sim = function(num = NULL) {
    "Generic Method for Computing and Organizing Simulated Quantities of Interest"
    is_zelig(.self)
    is_uninitializedField(.self$zelig.out)

    ## If num is defined by user, it overrides the value stored in the .self$num field.
    ## If num is not defined by user, but is also not yet defined in .self$num, then it defaults to 1000.

    localNum <- num # avoids CRAN warning about deep assignment from num existing separately as argument and field
    if (length(.self$num) == 0){
      if(is.null(localNum)){
        localNum <- 1000
      }
    }
    if(!is.null(localNum)){
      .self$num <- localNum
    }

    # This was previous version, that assumed sim only called once, or only method to access/write .self$num field:
    #if (length(.self$num) == 0)
    #  .self$num <- num

    # Divide simulations among imputed datasets
    if(.self$mi){
      am.m <- length(.self$get_coef())
      .self$num <- ceiling(.self$num/am.m)
    }
    # If bootstrapped, use distribution of estimated parameters,
    #  otherwise use $param() method for parametric bootstrap.
    if (.self$bootstrap & ! .self$mi){
      .self$num <- 1
      .self$simparam <- .self$zelig.out %>%
        do(simparam = .self$param(.$z.out, method = "point"))
    } else {
      .self$simparam <- .self$zelig.out %>%
        do(simparam = .self$param(.$z.out))
    }

    if (.self$bsetx)
      .self$simx()
    if (.self$bsetx1)
      .self$simx1()
    if (.self$bsetrange)
      .self$simrange()
    if (.self$bsetrange1)
      .self$simrange1()

    #if (is.null(.self$sim.out$x) & is.null(.self$sim.out$range))
    if (!isTRUE(is_sims_present(.self$sim.out, fail = FALSE)))
      warning('No simulations drawn, likely due to insufficient inputs.',
              call. = FALSE)
  }
)

z$methods(
  simx = function() {
    d <- zelig_mutate(.self$zelig.out, simparam = .self$simparam$simparam)
    d <- zelig_mutate(d, mm = .self$setx.out$x$mm)
    .self$sim.out$x <-  d %>%
      do(qi = .self$qi(.$simparam, .$mm)) %>%
      do(ev = .$qi$ev, pv = .$qi$pv)
  }
)

z$methods(
  simx1 = function() {
    d <- zelig_mutate(.self$zelig.out, simparam = .self$simparam$simparam)
    d <- zelig_mutate(d, mm = .self$setx.out$x1$mm)
    .self$sim.out$x1 <-  d %>%
      do(qi = .self$qi(.$simparam, .$mm)) %>%
      do(ev = .$qi$ev, pv = .$qi$pv)
    d <- zelig_mutate(.self$sim.out$x1, ev0 = .self$sim.out$x$ev)
    d <- d %>%
      do(fd = .$ev - .$ev0)
    .self$sim.out$x1 <- zelig_mutate(.self$sim.out$x1, fd = d$fd) #JH
  }
)

z$methods(
  simrange = function() {
    .self$sim.out$range <- list()
    for (i in 1:nrow(.self$range)) {
      d <- zelig_mutate(.self$zelig.out, simparam = .self$simparam$simparam)
      d <- zelig_mutate(d, mm = .self$setx.out$range[[i]]$mm)
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
      d <- zelig_mutate(.self$zelig.out, simparam = .self$simparam$simparam)
      d <- zelig_mutate(d, mm = .self$setx.out$range1[[i]]$mm)
      .self$sim.out$range1[[i]] <-  d %>%
        do(qi = .self$qi(.$simparam, .$mm)) %>%
        do(ev = .$qi$ev, pv = .$qi$pv)
    }
  }
)



z$methods(
  simx = function() {
    d <- zelig_mutate(.self$zelig.out, simparam = .self$simparam$simparam)
    d <- zelig_mutate(d, mm = .self$setx.out$x$mm)
    .self$sim.out$x <-  d %>%
      do(qi = .self$qi(.$simparam, .$mm)) %>%
      do(ev = .$qi$ev, pv = .$qi$pv)
  }
)


z$methods(
  ATT = function(treatment, treated=1, quietly=TRUE, num=NULL) {
    "Generic Method for Computing Simulated (Sample) Average Treatment Effects on the Treated"

    ## Checks on user provided arguments
    if(!is.character(treatment)){
      stop("Argument treatment should be the name of the treatment variable in the dataset.")
    }
    if(!(treatment %in% names(.self$data))){
      stop(cat("Specified treatment variable", treatment, "is not in the dataset."))
    }
    # Check treatment variable included in model.
    # Check treatment variable is 0 or 1 (or generalize to dichotomous).
    # Check argument "treated" is 0 or 1 (or generalize to values of "treatment").
    # Check "ev" is available QI.
    # Check if multiple equation model (which will need method overwrite).


    ## If num is defined by user, it overrides the value stored in the .self$num field.
    ## If num is not defined by user, but is also not yet defined in .self$num, then it defaults to 1000.
    localNum <- num
    if (length(.self$num) == 0){
      if(is.null(localNum)){
        localNum <- 1000
      }
    }
    if(!is.null(localNum)){
      if(!identical(localNum,.self$num)){   # .self$num changed, so regenerate simparam
        .self$num <- localNum
        .self$simparam <- .self$zelig.out %>%
          do(simparam = .self$param(.$z.out))
      }
    }

    ## Extract name of dependent variable, treated units
    depvar <- as.character(.self$zelig.call[[2]][2])

    ## Use dplyr to cycle over all splits of dataset
    ## NOTE: THIS IS GOING TO USE THE SAME simparam SET FOR EVERY SPLIT
    .self$sim.out$TE <- .self$data %>%
      group_by_(.self$by) %>%
      do(ATT = .self$simATT(simparam=.self$simparam$simparam[[1]], data=. , depvar=depvar, treatment=treatment, treated=treated) )   # z.out = eval(fn2(.self$model.call, quote(as.data.frame(.)))))

    if(!quietly){
      return(.self$sim.out$TE)  # The $get_qi() method may generalize, otherwise, write a $getter.
    }
  }
)

# Has calls to .self, so constructed as method rather than function internal to $ATT()
# Function to simulate ATT

z$methods(
  simATT = function(simparam, data, depvar, treatment, treated) {
    "Simulate an Average Treatment on the Treated"

    localData <- data # avoids CRAN warning about deep assignment from data existing separately as argument and field
    flag <- localData[[treatment]]==treated
    localData[[treatment]] <- 1-treated

    cf.mm <- model.matrix(.self$formula, localData) # Counterfactual model matrix
    cf.mm <- cf.mm[flag,]

    y1 <- localData[flag, depvar]
    y1.n <- sum(flag)

    ATT <- matrix(NA, nrow=y1.n, ncol= .self$num)
    for(i in 1:y1.n){                   # Maybe $qi() generally works for all mm? Of all dimensions? If so, loop not needed.
      ATT[i,] <- as.numeric(y1[i,1]) - .self$qi(simparam=simparam, mm=cf.mm[i, , drop=FALSE])$ev
    }
    ATT <- apply(ATT, 2, mean)
    return(ATT)
  }
)

z$methods(
  get_names = function() {
    "Return Zelig object field names"
    z_names <- names(as.list(.self))
    return(z_names)
  }
)


z$methods(
  show = function(signif.stars = FALSE, subset = NULL, bagging = FALSE) {
    "Display a Zelig object"

    is_uninitializedField(.self$zelig.out)
    .self$signif.stars <- signif.stars
    .self$signif.stars.default <- getOption("show.signif.stars")
    options(show.signif.stars = .self$signif.stars)
    if ("uninitializedField" %in% class(.self$zelig.out))
      cat("Next step: Use 'zelig' method")
    else if (length(.self$setx.out) == 0) {

      #############################################################################
      # Current workaround to display call as $zelig.call rather than $model.call
      # This is becoming a more complex workaround than revising the summary method
      # should improve this approach in future:
      for(jj in 1:length(.self$zelig.out$z.out)){
        if("S4" %in% typeof(.self$zelig.out$z.out[[jj]]) ){
          slot(.self$zelig.out$z.out[[jj]],"call") <- .self$zelig.call
        } else {
          if("call" %in% names(.self$zelig.out$z.out[[jj]])){
            .self$zelig.out$z.out[[jj]]$call <- .self$zelig.call
          } else if ("call" %in% names(attributes(.self$zelig.out$z.out[[1]])) ){
            attr(.self$zelig.out$z.out[[1]],"call")<- .self$zelig.call
          }
        }
      }
      #############################################################################

    if((.self$mi || .self$bootstrap)  & is.null(subset)){
        if (.self$mi)
            cat("Model: Combined Imputations \n\n")
        else
            cat("Model: Combined Bootstraps \n\n")

        mi_combined <- combine_coef_se(.self, messages = FALSE)
        printCoefmat(mi_combined, P.values = TRUE, has.Pvalue = TRUE,
                     digits = max(2, getOption("digits") - 4))
        cat("\n")

        if (.self$mi)
            cat("For results from individual imputed datasets, use summary(x, subset = i:j)\n")
        else
            cat("For results from individual bootstrapped datasets, use summary(x, subset = i:j)\n")
    } else if ((.self$mi) & !is.null(subset)) {
            for(i in subset){
                cat("Imputed Dataset ", i, sep = "")
                print(base::summary(.self$zelig.out$z.out[[i]]))
            }
    } else if ((.self$bootstrap) & !is.null(subset)) {
        for(i in subset){
            cat("Bootstrapped Dataset ", i, sep = "")
            print(base::summary(.self$zelig.out$z.out[[i]]))
        }
    } else {
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
                if("S4" %in% typeof(.$z.out)){  # Need to change summary method here for some classes
                    print(summary(.$z.out))
                } else {
                    print(base::summary(.$z.out))
                }
            })
    }


      if("gim.criteria" %in% names(.self$test.statistics)){
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
      niceprint <- function(obj, name){
        if(!is.null(obj[[1]])){
          cat(name, ":\n", sep = "")
          if (is.data.frame(obj))
              screenoutput <- obj
          else
              screenoutput <- obj[[1]]
          attr(screenoutput,"assign") <- NULL
          print(screenoutput, digits = max(2, getOption("digits") - 4))
        }
      }
      range_out <- function(x, which_range = 'range') {
        if (!is.null(x$setx.out[[which_range]])) {
            xvarnames <- names(as.data.frame(x$setx.out[[which_range]][[1]]$mm[[1]]))
            d <- length(x$setx.out[[which_range]])
            num_cols <- length(x$setx.out[[which_range]][[1]]$mm[[1]] )
            xmatrix <- matrix(NA, nrow = d, ncol = num_cols)
            for (i in 1:d){
                xmatrix[i,] <- matrix(x$setx.out[[which_range]][[i]]$mm[[1]],
                                      ncol = num_cols)
            }
            xdf <- data.frame(xmatrix)
            names(xdf) <- xvarnames
            return(xdf)
          }
      }

      niceprint(obj=.self$setx.out$x$mm, name="setx")
      niceprint(obj=.self$setx.out$x1$mm, name="setx1")
      niceprint(obj = range_out(.self), name = "range")
      niceprint(obj = range_out(.self, 'range1'), name = "range1")
     # niceprint(obj=.self$setx.out$range[[1]]$mm, name="range")
     #  niceprint(obj=.self$setx.out$range1[[1]]$mm, name="range1")
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
  graph = function(...) {
    "Plot the quantities of interest"

    is_uninitializedField(.self$zelig.out)
    is_sims_present(.self$sim.out)

    if (is_simsx(.self$sim.out, fail = FALSE)) qi.plot(.self, ...)
    if (is_simsrange(.self$sim.out, fail = FALSE)) ci.plot(.self, ...)
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
    show(...)
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
  from_zelig_model = function() {
    "Extract the original fitted model object from a zelig call. Note only works for models using directly wrapped functions."
    is_uninitializedField(.self$zelig.out)
    result <- try(.self$zelig.out$z.out, silent = TRUE)

    if ("try-error" %in% class(result)) {
      stop("from_zelig_model not available for this fitted model.")
    } else {
      if (length(result) == 1) {
        result <- result[[1]]
        result <- strip_package_name(result)
      } else if (length(result) > 1) {
        if (.self$mi) {
          message("Returning fitted model objects for each imputed data set in a list.")
        } else if (.self$bootstrap) {
          message("Returning fitted model objects for each bootstrapped data set in a list.")
        } else {
          message("Returning fitted model objects for each subset of the data created from the 'by' argument, in a list.")
        }
        result <- lapply(result, strip_package_name)
      }
      return(result)
    }
  })

#' Method for extracting estimated coefficients from Zelig objects
#' @param nonlist logical whethe to \code{unlist} the result if there are only
#'   one set of coefficients. Enables backwards compatibility.

z$methods(
  get_coef = function(nonlist = FALSE) {
    "Get estimated model coefficients"

    is_uninitializedField(.self$zelig.out)
    result <- try(lapply(.self$zelig.out$z.out, coef), silent = TRUE)
    if ("try-error" %in% class(result))
      stop("'coef' method' not implemented for model '", .self$name, "'")
    else {
        if (nonlist & length(result) == 1) result <- unlist(result)
        return(result)
    }
  }
)

#' Method for extracting estimated variance covariance matrix from Zelig objects
#' @param nonlist logical whethe to \code{unlist} the result if there are only
#'   one set of coefficients. Enables backwards compatibility.

z$methods(
    get_vcov = function() {
        "Get estimated model variance-covariance matrix"
        is_uninitializedField(.self$zelig.out)
        result <- lapply(.self$zelig.out$z.out, vcov)
        if ("try-error" %in% class(result))
            stop("'vcov' method' not implemented for model '", .self$name, "'")
        else
            return(result)
    }
)

#' Method for extracting p-values from Zelig objects
#' @param object an object of class Zelig

z$methods(
  get_pvalue = function() {
    "Get estimated model p-values"

    is_uninitializedField(.self$zelig.out)
    result <- try(lapply(.self$zelig.out$z.out, p_pull), silent = TRUE)
    if ("try-error" %in% class(result))
      stop("'get_pvalue' method' not implemented for model '", .self$name, "'")
    else
      return(result)
  }
)

#' Method for extracting standard errors from Zelig objects
#' @param object an object of class Zelig

z$methods(
  get_se = function() {
    "Get estimated model standard errors"

    is_uninitializedField(.self$zelig.out)
    result <- try(lapply(.self$zelig.out$z.out, se_pull), silent = TRUE)
    if ("try-error" %in% class(result))
      stop("'get_se' method' not implemented for model '", .self$name, "'")
    else
      return(result)
  }
)

z$methods(
  get_residuals = function() {
    "Get estimated model residuals"

    is_uninitializedField(.self$zelig.out)
    result <- try(lapply(.self$zelig.out$z.out, residuals), silent = TRUE)
    if ("try-error" %in% class(result))
      stop("'residuals' method' not implemented for model '", .self$name, "'")
    else
      return(result)
  }
)

z$methods(
  get_df_residual = function() {
    "Get residual degrees-of-freedom"

    is_uninitializedField(.self$zelig.out)
    result <- try(lapply(.self$zelig.out$z.out, df.residual), silent = TRUE)
    if ("try-error" %in% class(result))
      stop("'df.residual' method' not implemented for model '", .self$name, "'")
    else
      return(result)
  }
)

z$methods(
  get_fitted = function() {
    "Get estimated fitted values"

    is_uninitializedField(.self$zelig.out)
    result <- lapply(.self$zelig.out$z.out, fitted)
    if ("try-error" %in% class(result))
      stop("'predict' method' not implemented for model '", .self$name, "'")
    else
      return(result)
  }
)

z$methods(
  get_predict = function() {
    "Get predicted values"

    is_uninitializedField(.self$zelig.out)
    result <- lapply(.self$zelig.out$z.out, predict)
    if ("try-error" %in% class(result))
      stop("'predict' method' not implemented for model '", .self$name, "'")
    else
      return(result)
  }
)

z$methods(
  get_qi = function(qi = "ev", xvalue = "x", subset = NULL) {
    "Get quantities of interest"

    is_sims_present(.self$sim.out)

    possiblexvalues <- names(.self$sim.out)
    if(!(xvalue %in% possiblexvalues)){
      stop(paste("xvalue must be ", paste(possiblexvalues, collapse = " or ") ,
                 ".", sep = ""))
    }
    possibleqivalues <- c(names(.self$sim.out[[xvalue]]),
                          names(.self$sim.out[[xvalue]][[1]]))
    if(!(qi %in% possibleqivalues)){
      stop(paste("qi must be ", paste(possibleqivalues, collapse=" or ") , ".",
                                      sep = ""))
    }
    if(.self$mi){
      if(is.null(subset)){
        am.m<-length(.self$get_coef())
        subset <- 1:am.m
      }
      tempqi <- do.call(rbind, .self$sim.out[[xvalue]][[qi]][subset])
    } else if(.self$bootstrap){
      if(is.null(subset)){
        subset <- 1:.self$bootstrap.num
      }
      tempqi <- do.call(rbind, .self$sim.out[[xvalue]][[qi]][subset])
    } else if(xvalue %in% c("range", "range1")) {
      tempqi <- do.call(rbind, .self$sim.out[[xvalue]])[[qi]]
    } else {
      tempqi<- .self$sim.out[[xvalue]][[qi]][[1]]   # also works:   tempqi <- do.call(rbind, .self$sim.out[[xvalue]][[qi]])
    }
    return(tempqi)
  }
)

z$methods(
    get_model_data = function() {
        "Get data used to estimate the model"

        is_uninitializedField(.self$zelig.out)
        model_data <- .self$originaldata
        return(model_data)
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
  mcunit = function(nsim = 500, minx = -2, maxx = 2, b0 = 0, b1 = 1, alpha = 1,
                    ci = 0.95, plot = TRUE, ...){

    passes <- TRUE
    n.short <- 10      # number of p
    alpha.ci <- 1 - ci   # alpha values for ci bounds, not speed parameter
    x.sim <- runif(n=nsim, min=minx, max=maxx)
    x.seq <- seq(from=minx, to=maxx, length = nsim)

    data.hat <- .self$mcfun(x=x.seq, b0=b0, b1=b1, alpha=alpha, ..., sim=FALSE)
    if(!is.data.frame(data.hat)){
      data.hat <- data.frame(x.seq=x.seq, y.hat=data.hat)
    }
    data.sim <- .self$mcfun(x=x.sim, b0=b0, b1=b1, alpha=alpha, ..., sim=TRUE)
    if(!is.data.frame(data.sim)){
      data.sim <- data.frame(x.sim=x.sim, y.sim=data.sim)
    }

    ## Estimate Zelig model and create numerical bounds on expected values
    # This should be the solution, but requires fixing R scoping issue:
    #.self$zelig(y.sim~x.sim, data=data.sim)
    # formula will be overwritten in zelig() if .self$mcformula has been set

    ## Instead, remove formula field and set by hard code
    .self$mcformula <- NULL
    if(.self$name %in% c("exp", "weibull", "lognorm")){
      .self$zelig(Surv(y.sim,event) ~ x.sim, data = data.sim)
    } else if (.self$name %in% c("relogit")) {
      tau <- sum(data.sim$y.sim)/nsim
      .self$zelig(y.sim ~ x.sim, tau = tau, data = data.sim)
    } else {
      .self$zelig(y.sim ~ x.sim, data = data.sim)
    }

    x.short.seq<-seq(from = minx, to = maxx, length = n.short)
    .self$setrange(x.sim = x.short.seq)
    .self$sim()

    if (.self$name %in% c("relogit")) {
      data.short.hat <- .self$mcfun(x=x.short.seq, b0=b0, b1=b1, alpha=alpha, keepall=TRUE, ..., sim=FALSE)
    } else {
      data.short.hat <- .self$mcfun(x=x.short.seq, b0=b0, b1=b1, alpha=alpha, ..., sim=FALSE)
    }

    if(!is.data.frame(data.short.hat)){
      data.short.hat<-data.frame(x.seq=x.short.seq, y.hat=data.short.hat)
    }

    history.ev <- history.pv <- matrix(NA, nrow=n.short, ncol=2)
    for(i in 1:n.short){
      xtemp <- x.short.seq[i]
      .self$setx(x.sim = xtemp)
      .self$sim()
      #temp<-sort( .self$sim.out$x$ev[[1]] )
      temp <- .self$sim.out$range[[i]]$ev[[1]]
      # This is for ev's that are a probability distribution across outcomes, like ordered logit/probit
      if(ncol(temp) > 1){
        temp <- temp %*% as.numeric(sort(unique(data.sim$y.sim)))  #as.numeric(colnames(temp))
      }
      temp <- sort(temp)

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
        windex <- sample(x=1:n.obs, size=n.w, replace=TRUE, prob=iweights)  # Should size be n.w or n.obs?  Relatedly, n.w might not be integer.
        idata <- idata[windex,]
        .self$data <- idata
      }else{
        .self$buildDataByWeights()  # If all weights are integers, just use duplication to rebuild dataset.
      }
    }
  }
)


# rebuild dataset by bootstrapping using weights as probabilities
#   might possibly combine this method with $buildDataByWeights2()
z$methods(
  buildDataByBootstrap = function() {
    idata <- .self$data
    n.boot <- .self$bootstrap.num
    n.obs <- nrow(idata)

    if(!is.null(.self$weights)){
      iweights <- .self$weights
      n.w   <- sum(iweights)
      iweights <- iweights/n.w
    }else{
      iweights <- NULL
    }

    windex <- bootstrapIndex <- NULL
    for(i in 1:n.boot){
      windex <- c(windex, sample(x=1:n.obs, size=n.obs, replace=TRUE, prob=iweights))
      bootstrapIndex <- c(bootstrapIndex, rep(i,n.obs))
    }
    # Last dataset is original data
    idata <- rbind(idata[windex,], idata)
    bootstrapIndex <- c(bootstrapIndex, rep(n.boot+1,n.obs))

    idata$bootstrapIndex <- bootstrapIndex
    .self$data <- idata
    .self$by <- c("bootstrapIndex", .self$by)
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


#' Summary method for Zelig objects
#' @param object An Object of Class Zelig
#' @param ... Additional parameters to be passed to summary
setMethod("summary", "Zelig",
          function(object, ...) {
            object$summarize(...)
          }
)

#' Plot method for Zelig objects
#' @param x An Object of Class Zelig
#' @param y unused
#' @param ... Additional parameters to be passed to plot
setMethod("plot", "Zelig",
          function(x, ...) {
            x$graph(...)
          }
)

#' Names method for Zelig objects
#' @param x An Object of Class Zelig
setMethod("names", "Zelig",
          function(x) {
            x$get_names()
          }
)

setGeneric("vcov")
#' Variance-covariance method for Zelig objects
#' @param object An Object of Class Zelig

setMethod("vcov", "Zelig",
          function(object) {
            object$get_vcov()
          }
)

#' Method for extracting estimated coefficients from Zelig objects
#' @param object An Object of Class Zelig

setMethod("coefficients", "Zelig",
          function(object) {
              object$get_coef(nonlist = TRUE)
          }
)

setGeneric("coef")
#' Method for extracting estimated coefficients from Zelig objects
#' @param object An Object of Class Zelig

setMethod("coef", "Zelig",
          function(object) {
            object$get_coef(nonlist = TRUE)
          }
)

#' Method for extracting residuals from Zelig objects
#' @param object An Object of Class Zelig
setMethod("residuals", "Zelig",
          function(object) {
            object$get_residuals()
          }
)

#' Method for extracting residual degrees-of-freedom from Zelig objects
#' @param object An Object of Class Zelig
setMethod("df.residual", "Zelig",
          function(object) {
            object$get_df_residual()
          }
)

setGeneric("fitted")
#' Method for extracting estimated fitted values from Zelig objects
#' @param object An Object of Class Zelig
#' @param ... Additional parameters to be passed to fitted
setMethod("fitted", "Zelig",
          function(object, ...) {
            object$get_fitted(...)
          }
)

setGeneric("predict")
#' Method for getting predicted values from Zelig objects
#' @param object An Object of Class Zelig
#' @param ... Additional parameters to be passed to predict
setMethod("predict", "Zelig",
          function(object, ...) {
            object$get_predict(...)
          }
)

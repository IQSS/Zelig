Zelig-ls
~~~~~~~

.. sourcecode:: r
    

    # Run least squares regression and save the output in working memory:
    z.out <- zelig(y ~ x1 + x2, model = "ls", data = mydata)
    # See what's in the R object:
    names(z.out)
    ## [1] 'coefficients' 'residuals' 'effects' 'rank' Extract and display the
    ## coefficients in z.out:
    z.out$coefficients




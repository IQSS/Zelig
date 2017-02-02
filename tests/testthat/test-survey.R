# REQUIRE TEST svydesign user specified cluster ids ----------------------------

test_that('REQUIRE TEST svydesign user specified cluster ids', {
    # Based on an example provided by Michael Evangelist
    # https://groups.google.com/d/msg/zelig-statistical-software/dAX0lhEnptY/sZ2YvM4nCAAJ
    province <- read.table(text =
            "id str clu wt ue91 race hou85 binary
            1 1 1 0.431149 5 white 6393 1
            2 1 1 0.2105972 10 hispanic 2895 0
            3 1 1 0.7161301 10 hispanic 4067 1
            4 1 2 0.0634466 9 white 6885 0
            5 1 2 0.2340882 9 hispanic 1627 1
            6 1 3 0.1120269 5 hispanic 2638 0
            7 1 3 0.3034396 7 black 2275 1
            8 1 3 0.6921046 6 black 6510 0
            9 1 3 0.7676221 8 white 4458 0
            10 1 4 0.3942932 8 black 1142 0
            11 1 4 0.7244728 6 white 6847 1
            12 1 5 0.6714353 8 white 2463 1
            13 1 5 0.1773429 7 white 4128 0
            14 1 5 0.0404869 10 hispanic 6546 0
            15 1 5 0.6101976 6 black 2927 1
            16 2 6 0.3316885 9 hispanic 3836 0
            17 2 6 0.3663854 8 white 2091 0
            18 2 6 0.4135158 5 hispanic 4871 0
            19 2 7 0.7784832 9 hispanic 1260 1
            20 2 7 0.2358319 7 white 5548 0
            21 2 7 0.2866012 5 white 5550 1
            22 2 7 0.3902279 6 white 2606 1
            23 2 8 0.4849472 6 hispanic 1466 1
            24 2 8 0.0357191 7 hispanic 6483 1
            25 2 9 0.1038419 8 hispanic 1001 1
            ",
            header = TRUE
    )
    province_design <- svydesign(id = ~clu, strata = ~str, data = province, 
                                 weights = ~wt)
    model_glm <- svyglm(binary ~ hou85 + ue91 + race, province_design, 
                        family=binomial(link="logit"))
    z <- zlogitsurvey$new()
    z$zelig(binary ~ hou85 + ue91 + race,  data = province, id = ~clu,
            strata = ~str, weights = ~wt)
    intercept_se <- coef(summary(z$zelig.out$z.out[[1]]))[, "Std. Error"][1]
    expect_equivalent(round(as.numeric(intercept_se), 4), 
                      round(as.numeric(coef(summary(model_glm))[, "Std. Error"][1]), 
                            4))
})

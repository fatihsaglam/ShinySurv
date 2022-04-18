color_names <- c("d3",
                 "aaas",
                 "brewer",
                 "futurama",
                 "grey",
                 "igv",
                 "jama",
                 "jco",
                 "lancet",
                 "locuszoom",
                 "nejm",
                 "npg",
                 "rickandmorty",
                 "simpsons",
                 "startrek",
                 "tron",
                 "uchicago")

theme_names <- c("bw",
                 "classic",
                 "dark",
                 "gray",
                 "light",
                 "minimal",
                 "void")

conf_methods <- c("peto",
                  "linear",
                  "log",
                  "loglog",
                  "rothman",
                  "grunkemeier",
                  "hall-wellner",
                  "loghall",
                  "epband",
                  "logep")

conf_methods_names <- c("Peto",
                        "linear",
                        "log",
                        "loglog",
                        "Rothman",
                        "Grunkemeier",
                        "Hall-Wellner",
                        "log Hall-Wellner",
                        "Equal Precision",
                        "log Equal Precision")

image_file_types <- c("eps",
                      "ps",
                      "tex",
                      "pdf",
                      "jpeg",
                      "tiff",
                      "png",
                      "bmp",
                      "svg",
                      "wmf")

curve_type_names <- c("Survival",
                     "Failure",
                     "Cumulative hazard",
                     "Hazard",
                     "Density")

curve_types <- c("S",
                "F",
                "H",
                "h",
                "f")

dist_names <- c("Exponential",
                "Weibull",
                "Gamma",
                "Generalized Gamma",
                "log Normal",
                "log Logistic",
                "Gompertz",
                "Rayleigh"
                )

dists <- c("exp", #1 rate
           "weibull", #2 shape, scale
           "gamma", #2 shape, rate
           "gengamma", #3 location, scale, shape
           "lnorm", #2 meanlog, sdlog
           "llogis", #2 shape, scale
           "gompertz", #2 shape, rate
           "rayleigh" #1 sigma
           )

dists_par_names <- list(c("rate"),
                        c("shape", "scale"),
                        c("shape", "rate"),
                        c("location", "scale", "shape"),
                        c("meanlog", "sdlog"),
                        c("shape", "scale"),
                        c("shape", "rate"),
                        c("sigma")
                        )

dists_n_par <- c(1,
                 2,
                 2,
                 3,
                 2,
                 2,
                 2,
                 1)

bootstraps <- c("JKBoot", "NPBoot")
bootstrap_names <- c("Jackknife", "Non-parametric bootstrap")

legend_positions <- c("none",
                      "right",
                      "left",
                      "top",
                      "bottom")

face_types <- c("plain", "italic", "bold", "bold.italic")

font_names <- c()

surv_var_ids <- c("time_1", "time_2", "group", "c_l", "c_r")
surv_var_labels <- c("Time 1", "Time 2", "Group", "Left censor", "Right Censor")

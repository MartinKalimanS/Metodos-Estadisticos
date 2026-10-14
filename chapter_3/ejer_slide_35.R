source("chapter_3/formulas_chapter3.R")

seasonal_indices("- - 1.0778 0.9337
                  1.1079 0.8764 1.1025 0.9246
                  1.0906 0.8743 1.0932 0.9337
                  1.1069 0.8932 1.0703 0.9573
                  1.0559 0.8829 - -",
                 season_names = c("Spring","Summer","Autumn","Winter"))

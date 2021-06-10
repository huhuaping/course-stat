
#=======load data======
df <- openxlsx::read.xlsx(here("data","textbook","example",
                                    "case-13-4-stock.xlsx"),
                               startRow = 2)
n <- nrow(df)

# mod for trend
mod_line <- formula(price ~ week)
mod_power <- formula(price ~ week +I(week^2))

#==========function for eq in ggplot===================
# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA
# SOURCE: https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
lm_eqn <- function(mod, dt, cat="line"){
  m <- lm(mod, dt)
  eq <- if (cat =="line"){
    substitute(italic(Y) == a + b* italic(X)*","~~italic(r)^2~"="~r2,
        list(a = formatC(unname(m$coeffi[1]), digits = 2, format = "f"),
             b = formatC(unname(coef(m)[2]), digits = 2, format = "f"),
             r2 = formatC(summary(m)$r.squared, digits = 4, format = "f")))
    }
    else if (cat =="power"){
      substitute(italic(Y) == a + b* italic(X)+ c* italic(X)^2*","~~italic(r)^2~"="~r2,
                 list(a = formatC(unname(coef(m)[1]), digits = 2, format = "f"),
                      b = formatC(unname(coef(m)[2]), digits = 2, format = "f"),
                      c = formatC(unname(coef(m)[3]), digits = 2, format = "f"),
                      r2 = formatC(summary(m)$r.squared, digits = 4, format = "f")))
    }
  as.character(as.expression(eq))
}



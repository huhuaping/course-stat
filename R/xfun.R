
#====fun str_compose=====

str_compose <- function(vec1, vec2,
                        decorate = c("（", "）"),
                        clps = "、"){
  if (length(vec1) != length(vec2)) {
    "length of the two vectors not equal"
  }
  out <- str_c(str_c(vec1,decorate[1]),
               str_c(vec2,decorate[2]), 
               collapse = clps )
  out
}

#=====format number=====
num_round <- function(num, digits = 4) {
  formatC(num, format = "f", digits = digits)
}

#=====partial correlation coefficients====

par_corl <- function(v1, v2, v3){
  r12 <- cor(v1, v2)
  r13 <- cor(v1, v3)
  r23 <- cor(v2, v3)
  out <- (r12 - r13*r23)/sqrt((1-r13^2) * (1-r23^2))
  return(out)
}

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

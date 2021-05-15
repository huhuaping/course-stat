
# fun str_compose ----


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

# format number
num_round <- function(num, digits = 4) {
  formatC(num, format = "f", digits = digits)
}

# partial correlation coefficent

par_corl <- function(v1, v2, v3){
  r12 <- cor(v1, v2)
  r13 <- cor(v1, v3)
  r23 <- cor(v2, v3)
  out <- (r12 - r13*r23)/sqrt((1-r13^2) * (1-r23^2))
  return(out)
}

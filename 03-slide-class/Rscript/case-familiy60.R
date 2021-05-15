
# 05-02 families 60---------------

require("here")
url_fams60 <- here("data","extra","Table-2-1-60families-new.xlsx")
fams60 <- as_tibble(read.xlsx(url_fams60,shee = 1))
fams60_long <- as_tibble(read.xlsx(url_fams60,  sheet =2))

## expectation value-----
exp_cond <- fams60 %>%
  filter(Mark!="X") %>%
  select(-Mark) %>%
  colMeans(.,na.rm = T) %>%
  rbind(fams60[1,-1], .) %>%
  add_column(var = c("X","E(Y|X)"), .before = "G1")

pivot_exp <- as_tibble(t(exp_cond))[-1,] %>%
  dplyr::rename( X=V1,exp.Y="V2") %>%
  type_convert(cols(X=col_double(),
                    exp.Y= col_double()))


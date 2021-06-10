
#=======load data======
df <- openxlsx::read.xlsx(here("data","textbook","example",
                                    "case-13-5-beer-sale.xlsx"),
                               startRow = 2)

n <- nrow(df)

df_wide <- df %>%
  pivot_wider(names_from = year, values_from = sale)



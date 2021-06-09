
# load data ----
df <- openxlsx::read.xlsx(here("data","textbook","example",
                                    "case-13-4-stock.xlsx"),
                               startRow = 2)
n <- nrow(df)


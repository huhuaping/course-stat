
theme_basic <- function(){
  theme(axis.text = element_text(size = 14),
        axis.text.x = element_text(size = 16,
                                   margin = margin(t = 10, r = 0, 
                                                   b = 0, l = 0)),
        axis.text.y = element_text(size = 16,
                                   margin = margin(t = 0, r = 10, 
                                                   b = 0, l = 0)),
        axis.title.x = element_text(size = 16,
                                    margin = margin(t = 15, r = 0, 
                                                    b = 0, l = 0)),
        axis.title.y = element_text(size = 16,
                                    margin = margin(t = 0, r = 15, 
                                                    b = 0, l = 0))
  )
}

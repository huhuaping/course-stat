#' Create Button for DT::table Funciton
#'
#' @param dt 
#' @param caption 
#' @param dom 
#' @param pg_len 
#'
#' @return
#' @export
#'
#' @examples

# see [url](https://martinctc.github.io/blog/vignette-downloadable-tables-in-rmarkdown-with-the-dt-package/)

create_DTbutton <- function(dt, caption='', dom='Bt', pg_len=10){
  DT::datatable(dt,
                caption = caption,
                #see [url](https://stackoverflow.com/questions/37546035/change-color-of-tabletools-buttons-in-dt-library-in-r)
                callback=JS(
                '$("button.buttons-copy").css("background","yellow");
                $("button.buttons-csv").css("background","orange");
                $("button.buttons-excel").css("background","#a88d32");
                $("button.dt-button").css("padding","0.2em 0.2em");
                $("button.dt-button").css("font-size","0.6em");
                $("button.dt-button").css("margin-right","0.2em");
                $("button.dt-button").css("margin-bottom","0.1em");
                $("button.dt-button").css("line-height","1em");
                return table;'),
                extensions = 'Buttons',
                options = list(
                  dom = dom,
                  pageLength = pg_len,
                  buttons = c('copy', 'csv', 'excel'),
                  initComplete = JS( # header background color
                    # see [url](https://www.r-bloggers.com/2019/04/styling-datatables/)
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#517fb9', 'color': '#fff'});",
                    "}"
                    )
                  )
                )
}
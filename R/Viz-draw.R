# require needed pkgs
library(DiagrammeR)
library(DiagrammeRsvg)
library(magrittr)
library(svglite)
library(rsvg)
library(png)


# population and sample ---------------------------------------------------


g_ps <- grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']

      # edge definitions with the node IDs
      tab1 -> tab2 ;
      tab1 -> tab3;
      }

      [1]: '总体\\n(Population)'
      [2]: 'Y_i'
      [3]: 'X_i'
      ")
g_ps %>% export_svg %>% charToRaw %>% rsvg %>% png::writePNG("pic/flowchart-pop-smpl.pdf")

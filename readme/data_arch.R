library(DiagrammeR)
library(DiagrammeRsvg)

gv <- grViz(diagram = "readme/data_arch.gv")
gv_svg <- export_svg(gv = gv)
write(x = gv_svg, file = "readme/data_arch.svg")

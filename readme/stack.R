setwd('readme')

library(DiagrammeR)
library(DiagrammeRsvg)
library(dplyr)
library(glue)
library(htmltools)

g <- create_graph(graph_name = "stack") %>%
  add_node(label = "container", node_aes = node_aes(image = "img/container.png")) %>%
  add_node(label = "docker", node_aes = node_aes(image = "img/docker.png")) %>%
  add_node(label = "docker-hub", node_aes = node_aes(image = "img/docker-hub.png")) %>%
  add_node(label = "hdf5", node_aes = node_aes(image = "img/hdf5.png")) %>%
  add_node(label = "npm", node_aes = node_aes(image = "img/npm.png")) %>%
  add_node(label = "renv", node_aes = node_aes(image = "img/renv.png")) %>%
  add_node(label = "pcf", node_aes = node_aes(image = "img/pcf.png")) %>%
  add_node(label = "r", node_aes = node_aes(image = "img/r.svg")) %>%
  add_node(label = "shiny", node_aes = node_aes(image = "img/shiny.png")) %>%
  add_edge(from = "container", to = "docker")


dot_code <- generate_dot(g)
svg_vec <- strsplit(export_svg(grViz(diagram = dot_code)), "\n") %>% unlist()
svg_tbl <- DiagrammeR:::get_svg_tbl(svg_vec)
svg_lines <- "<svg display=\"block\" margin=\"0 auto\" position=\"absolute\" width=\"100%\" height=\"100%\""
svg_line_no <- svg_tbl %>% filter(type == "svg") %>% pull(index)
svg_vec[svg_line_no] <- svg_lines

node_id_images <- g %>% 
  get_node_df() %>% 
  select(id, image) %>% 
  filter(image != "") %>% 
  pull(id)
filter_lines <- g %>% 
  get_node_df() %>% 
  select(id, image) %>% 
  filter(image != "") %>% 
  mutate(
    filter_lines = as.character(
      glue("<filter id=\"{id}\" x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\"><feImage xlink:href=\"{image}\"/></filter>")
    )
  ) %>% 
  pull(filter_lines) %>% 
  paste(collapse = "\n")
filter_shape_refs <- as.character(glue(" filter=\"url(#{node_id_images})\" "))
svg_shape_nos <- svg_tbl %>% 
  filter(node_id %in% node_id_images) %>% 
  filter(type == "node_block") %>% 
  pull(index)
svg_shape_nos <- svg_shape_nos + 3
svg_text_nos <- svg_shape_nos + 1
for (i in seq(node_id_images)) {
  svg_vec[svg_shape_nos[i]] <- sub(" ", paste0(filter_shape_refs[i]), svg_vec[svg_shape_nos[i]])
  svg_vec[svg_text_nos[i]] <- ""
}
svg_vec[svg_line_no + 1] <- paste0(svg_vec[svg_line_no + 1], "\n\n", filter_lines, "\n")
svg <- paste(svg_vec, collapse = "\n") 
display <- browsable(HTML(svg)) 
save_html(html = display, file = 'stack.html')

# writeLines(paste(dot_code, collapse = "\n"), "stack.gv")
# 
# pg <- tags$div(gv)
# print(pg, browse = T)
# save_html(html = pg, file = 'stack.html')
# 
# gv_svg <- export_svg(gv = gv)
# write(x = gv_svg, file = "stack.svg")

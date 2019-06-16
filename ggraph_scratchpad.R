library(tidyverse)
library(ggraph)
library(igraph)
library(viridis)

# https://www.data-imaginist.com/2017/ggraph-introduction-layouts/
# https://www.data-imaginist.com/2017/ggraph-introduction-edges/
# https://www.data-imaginist.com/2017/ggraph-introduction-nodes/


# setwd
setwd("C:/Users/Stephen/Desktop/R/ggraph")

graph <- graph_from_data_frame(highschool)
glimpse(graph)
graph
attributes(graph)
graph[1]
graph[2]
graph[3]
length(graph)
head(highschool)


highschool2 <- highschool[1:10, ]
highschool2
graph2 <- graph_from_data_frame(highschool2)
graph2
graph2[1]
graph2[2]
graph2[3]


#############################################################


# create icicle plot from flare hierarchy data
glimpse(flare)
str(flare)

# inspect edges
str(flare$edges)
glimpse(flare$edges)
flare_edges <- flare$edges %>% as_tibble()
flare_edges

# add depth to edges just for inspection purposes
flare_edges <- flare_edges %>% mutate(from_depth = str_count(string = from, pattern = regex("\\.")),
                       to_depth = str_count(string = to, pattern = regex("\\."))) %>% arrange(from_depth)
flare_edges
flare_edges %>% arrange(desc(to_depth))

# inspect vertices
str(flare$vertices)
glimpse(flare$vertices)
flare_vertices <- flare$vertices %>% as_tibble()
flare_vertices
flare_vertices %>% distinct(shortName)

# add depth to vertices just for inspection purposes
flare_vertices <- flare_vertices %>% mutate(depth = str_count(string = name, pattern = regex("\\."))) %>% 
        arrange(depth)
flare_vertices
flare_vertices %>% arrange(desc(depth))
flare_vertices %>% count(shortName) %>% arrange(desc(n))


##############################################


# combine edges and vertices 
graph <- graph_from_data_frame(flare_edges, vertices = flare_vertices)
graph
graph %>% glimpse()
attributes(graph)


#################################################


# icicle plot
ggraph(graph, layout = "partition") + 
        geom_node_tile(aes(fill = depth), size = 0.25) + scale_fill_gradientn(colours = viridis(200, alpha = 1))

ggraph(graph, layout = "partition") + 
        geom_node_tile(aes(fill = shortName), show.legend = FALSE, size = 0.25) + 
        scale_fill_manual(values = viridis(251, alpha = 1))
 
ggraph(graph, 'partition') + 
        geom_node_tile(aes(fill = shortName), show.legend = FALSE, size = 0.25) + 
        scale_fill_manual(values = viridis(251, alpha = 1)) +
        theme(
                # panel.grid.major = element_line(color = "transparent"),
              # plot.background = element_blank(), 
              # panel.grid.minor = element_blank(), panel.border = element_blank(),
              # axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              # axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              # plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
              # legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
              # legend.text = element_text(size = 7),
              # panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank())


#############################################################################


# sunburst
# ggraph(graph, 'partition', circular = TRUE) +
#         geom_node_arc_bar(aes(fill = depth), size = 0.25) + geom_node_text(aes(label = shortName), repel = TRUE)

ggraph(graph, layout = "partition", circular = TRUE) +
        geom_node_arc_bar(aes(fill = depth), size = 0.25) + geom_node_text(aes(label = shortName)) + 
        scale_fill_gradientn(colours = viridis(200, alpha = 1))

ggraph(graph, 'partition', circular = TRUE) +
        geom_node_arc_bar(aes(fill = shortName), size = 0.25, show.legend = FALSE) + 
        scale_fill_manual(values = viridis(251, alpha = 1))

plot <- ggraph(graph, 'partition', circular = TRUE) +
        geom_node_arc_bar(aes(fill = shortName), size = 0.25, show.legend = FALSE) + 
        scale_fill_manual(values = viridis(251, alpha = 1)) + 
        theme(
                # panel.grid.major = element_line(color = "transparent"),
                # plot.background = element_blank(), 
                # panel.grid.minor = element_blank(), panel.border = element_blank(),
                # axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
                # axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
                # plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
                # legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
                # legend.text = element_text(size = 7),
                # panel.grid = element_blank(),
                line = element_blank(),
                rect = element_blank(),
                text = element_blank())
plot

# save


#################################################################


simple <- make_graph('bull')
simple
simple$name
simple$edges

# Random names - I swear
V(simple)$name <- c('Thomas', 'Bob', 'Hadley', 'Winston', 'Baptiste')
E(simple)$type <- sample(c('friend', 'foe'), 5, TRUE)

simple

ggraph(simple, layout = 'graphopt') + 
        geom_edge_link(aes(label = type), 
                       angle_calc = 'along',
                       label_dodge = unit(2.5, 'mm'),
                       arrow = arrow(length = unit(4, 'mm')), 
                       end_cap = circle(3, 'mm')) + 
        geom_node_point(size = 5)




##############################################


flaregraph <- graph_from_data_frame(flare$edges, vertices = flare$vertices)
glimpse(flaregraph)

from <- match(flare$imports$from, flare$vertices$name)
to <- match(flare$imports$to, flare$vertices$name)

head(flare$imports)
head(flare$imports$from)
head(flare$imports$to)
head(flare$vertices$name)
head(flare$edges)

ggraph(flaregraph, layout = 'dendrogram', circular = TRUE) + 
        geom_conn_bundle(data = get_con(from = from, to = to), alpha = 0.1) + 
        coord_fixed()




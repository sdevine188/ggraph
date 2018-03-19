library(ggraph)
library(igraph)

# https://www.data-imaginist.com/2017/ggraph-introduction-layouts/
# https://www.data-imaginist.com/2017/ggraph-introduction-edges/
# https://www.data-imaginist.com/2017/ggraph-introduction-nodes/


graph <- graph_from_data_frame(highschool)
glimpse(graph)
graph
attributes(graph)
graph[1]
graph[2]
graph[3]

head(highschool)


highschool2 <- highschool[1:10, ]
highschool2
graph2 <- graph_from_data_frame(highschool2)
graph2
graph2[1]
graph2[2]
graph2[3]


#############################################################

glimpse(flare)
str(flare)

glimpse(flare$edges)
flare_edges <- flare$edges
head(flare_edges)

glimpse(flare$vertices)
flare_vertices <- flare$vertices
head(flare_vertices)
flare_vertices %>% distinct(shortName)

head(flare)


graph <- graph_from_data_frame(flare_edges, vertices = flare_vertices)
graph
attributes(graph)

# icicle plot
ggraph(graph, 'partition') + 
        geom_node_tile(aes(fill = depth), size = 0.25) + scale_fill_gradientn(colours = viridis(200, alpha = 1))

ggraph(graph, 'partition') + 
        geom_node_tile(aes(fill = shortName), show.legend = FALSE, size = 0.25) + scale_fill_manual(values = viridis(251, alpha = 1))


# sunburst
# ggraph(graph, 'partition', circular = TRUE) +
#         geom_node_arc_bar(aes(fill = depth), size = 0.25) + geom_node_text(aes(label = shortName), repel = TRUE)

ggraph(graph, 'partition', circular = TRUE) +
        geom_node_arc_bar(aes(fill = depth), size = 0.25) + geom_node_text(aes(label = shortName)) + scale_fill_gradientn(colours = viridis(200, alpha = 1))

ggraph(graph, 'partition', circular = TRUE) +
        geom_node_arc_bar(aes(fill = shortName), size = 0.25, show.legend = FALSE) + scale_fill_manual(values = viridis(251, alpha = 1))


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




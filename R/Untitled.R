# # Libraries
# library(tidyverse)
# library(viridis)
# #install.packages('patchwork')
# library(patchwork)
# #install.packages('hrbrthemes')
# library(hrbrthemes)
# #install.packages('circlize')
# library(circlize)

# # Load dataset from github
# data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)
# # Package
# #install.packages('networkD3')
# library(networkD3)

# # I need a long format
# colnames(data) <- str_replace_all(colnames(data), "\\.", " ")
# data_long <- data %>%
#     rownames_to_column %>%
#     gather(key = 'key', value = 'value', -rowname) %>%
#     filter(value > 0)
# colnames(data_long) <- c("source", "target", "value")

# nodes <- nodes %>% mutate(name = str_replace_all(name, "\\.", " "))

# data_long$target <- paste(data_long$target, " ", sep="")

# # From these flows we need to create a node data frame: it lists every entities involved in the flow
# nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())

# # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
# data_long$IDsource=match(data_long$source, nodes$name)-1
# data_long$IDtarget=match(data_long$target, nodes$name)-1

# color_string <- rt_str_collapse(rt_colors(),.surround = '"', .separate = ", ")
# ColourScal <- paste0('d3.scaleOrdinal().range([', color_string,'])')
# # prepare colour scale
# #ColourScal ='d3.scaleOrdinal().range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# # Make the Network
# ?sankeyNetwork
# sankeyNetwork(Links = data_long, Nodes = nodes,
#               Source = "IDsource", Target = "IDtarget",
#               Value = "value", NodeID = "name",
#               sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)

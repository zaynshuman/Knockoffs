library("treemapify")

rank = c(1:100)
large_eq <- data.frame(large_eq, rank)
ggplot(large_eq, aes(area = mkt_cap, fill = rank, label = tic)) +
  geom_treemap() +
  geom_treemap_text(, colour = "white", place = "centre") + 
  theme(
        legend.key.height = unit(3, 'cm'), #change legend key height
        legend.title = element_text(size=18), #change legend title font size
        legend.text = element_text(size=14)) #change legend text font size


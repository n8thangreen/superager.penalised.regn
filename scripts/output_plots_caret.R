
# plots using caret output
#
# plots and tables
# post processing


TENSOR <- "7T"
save_plots <- FALSE
res <-
  readRDS(glue::glue("data/final/elasticnet_res_{TENSOR}.RDS"))


## L1 plots

if (save_plots)
  png(glue::glue("output/L1_plots_{TENSOR}.png"))

par(mfrow = c(2,3))
purrr::map(res, ~plot(.x$finalModel))

if (save_plots) dev.off()


## bar plots by network

# gg <-
#   ggplot(out, aes(x = network, y = or)) +
#   geom_bar(aes(fill = Label), position = "dodge", stat = "identity") +
#   coord_flip() +
#   theme_bw() +
#   ylab("Count") +
#   scale_x_discrete("Network name",
#                    breaks = factor(NETWORK_NAMES),
#                    drop = FALSE) #+
#   # scale_fill_brewer(palette = "Set3")
#
# gg
#
# if (save_plots)
#   ggsave(plot = gg, glue("output/barplots_network_{TENSOR}.png"), width = 5)
#
#
# #######################
# # tile plot
# ##TODO:
# ggplot(or_long, aes(network_name, variable)) +
#   geom_tile(aes(fill = value), colour = "white") +
#   scale_fill_gradient(low = "white", high = "steelblue") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#   scale_x_discrete("Network name",
#                    breaks = factor(NETWORK_NAMES),
#                    drop = FALSE) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())
#
# if (save_plots)
#   ggsave(glue("output/tile_plot_{TENSOR}.png"), width = 5)
#
# # grid.arrange(p1, p2, nrow = 1)


#######################
# boxplot

library(ggplot2)
library(viridis)
# https://www.r-graph-gallery.com/89-box-and-scatter-plot-with-ggplot2.html

out <- read.csv(file = glue::glue("data/elastic_net_{TENSOR}_table.csv"))

# x11()
ggplot(out, aes(x = network, y = or)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  # scale_x_discrete("Network name",
  #                  breaks = factor(NETWORK_NAMES$network_name),
  #                  drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) +
  ylab("OR") +
  coord_flip() +
  ylim(0.95, 1.05) +
  theme_bw()

if (save_plots)
  ggsave(glue("output/boxplot_{TENSOR}.png"), width = 5)


##########
# tables #
##########

summary_tab <-
  or_long %>%
  mutate(value = ifelse(value == "", NA, value),
         value = as.numeric(value)) %>%
  group_by(network) %>%
  summarise(OR = mean(value, na.rm = TRUE),
            L95 = quantile(value, probs = 0.025, na.rm = TRUE),
            U95 = quantile(value, probs = 0.975, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            count = sum(!is.na(value)),
            prop = count/n())
summary_tab

if (save_plots)
  write.csv(summary_tab, file = glue("data/summary_table_{TENSOR}.csv"))


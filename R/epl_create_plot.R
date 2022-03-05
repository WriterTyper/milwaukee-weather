library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(forcats)

current_table <- read_csv("data/current_table.csv")

current_table2 <- current_table %>% 
  pivot_longer(!team, names_to = "match", values_to = "count") %>% 
  mutate(match = as.numeric(match)) %>% 
  mutate(team3 = fct_inorder(team, ordered = TRUE))

epl_plot <- ggplot(current_table2, aes(factor(match), team3, fill = count)) +
  geom_tile(color = "white",
            lwd = .5,
            linetype = 1,
             height = 0.75) +
  coord_fixed() +
  theme_minimal() +
  scale_fill_gradient2(low = "#fa7645",
                       mid = "#F5F5F5",
                       high = "#a0bcd6", 
                       na.value = "white") +
  scale_y_discrete(limits = rev(levels(current_table2$team3))) +
  scale_x_discrete(position = "top") +
  xlab("Match") +
  ylab("") +
  theme(legend.position="none") +
  theme(text=element_text(family="mono")) +
  labs(caption = paste("Updated:", Sys.time(), Sys.timezone()))


ggsave("graphs/heatmap.png", plot = epl_plot,
       width = 8, height = 6, units = "in")


tidy_fun <- function(.data) {
  data <- .data %>% 
  pivot_longer(!team, names_to = "match", values_to = "count") %>% 
  mutate(match = as.numeric(match)) %>% 
  mutate(team3 = fct_inorder(team, ordered = TRUE))

  data
}

plot_fun <- function(.data) {
  
epl_plot <- ggplot(.data, aes(factor(match), team3, fill = count)) +
  geom_tile(color = "white",
            lwd = .5,
            linetype = 1,
             height = 0.4) +
  coord_fixed() +
  theme_minimal() +
  scale_y_discrete(limits = rev(levels(current_table2$team3))) +
  scale_x_discrete(position = "top") +
  xlab("Match") +
  ylab("") +
  theme(legend.position="none") +
  theme(text=element_text(family="mono")) +
  labs(caption = paste("Updated:", Sys.time(), Sys.timezone())) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) 


epl_plot
}


current_table_simple <- current_table %>% 
  mutate_if(is.numeric, funs(case_when(
      . < 0 ~ -1,
      . == 0 ~ 0,
      . > 0 ~ 1
    )))



epl_plot2 <- tidy_fun(current_table_simple) %>% 
  plot_fun()  + 
  scale_fill_gradient2(low = "#FFD0C0",
                       mid = "#F5F5F5",
                       high = "#A0BCD6", 
                       na.value = "white"
)



ggsave("graphs/heatmap2.png", plot = epl_plot2,
       width = 8, height = 6, units = "in")


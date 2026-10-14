setwd("~/be623_assignment5")
# Answer 1
circ_df <- data.frame(
  Sample = paste0("S", 1:12),
  Gene = c(
    rep("PER2", 6),
    rep("BMAL1", 6)
  ),
  ZT = rep(c(0, 4, 8, 12, 16, 20), 2),
  Expression = c(
    1.8, 3.2, 6.5, 9.4, 7.1, 3.6,
    8.6, 5.9, 2.7, 1.5, 3.4, 6.8
  )
)
str(circ_df)

# Answer 2
per2_rows <- circ_df[circ_df$Gene == "PER2", ]
mean(per2_rows$Expression)
median(per2_rows$Expression)
sd(per2_rows$Expression)

#Answer 3
write.csv(circ_df, "circadian_expression.csv",
          row.names = FALSE)
circ_reloaded <- read.csv("circadian_expression.csv")
identical(circ_df$Expression, circ_reloaded$Expression)

#Answer 4
per2_sorted <- circ_df %>%
  filter(Gene == "PER2") %>%
  arrange(desc(Expression))
bmal1_sorted <- circ_df %>%
  filter(Gene == "BMAL1") %>%
  arrange(desc(Expression))
per2_sorted
bmal1_sorted

#Answer 5
circ_df2 <- circ_df %>%
  mutate(dark = ZT >= 12)
dark_summary <- circ_df2 %>%
  filter(dark == TRUE) %>%
  group_by(Gene) %>%
  summarise(
    mean_expression = mean(Expression),
    n = n()
  )
dark_summary

light_summary <- circ_df2 %>%
  filter(dark == FALSE) %>%
  group_by(Gene) %>%
  summarise(
    mean_expression = mean(Expression),
    n = n()
  )
light_summary

#Answer 6
rhythm_plot <- ggplot(
  circ_df,
  aes(x = ZT, y = Expression, color = Gene)
) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("PER2" = "steelblue",
               "BMAL1" = "firebrick")
  ) +
  labs(
    title = "Circadian Gene Expression",
    x = "Zeitgeber time (h)",
    y = "Expression"
  ) +
  theme_minimal()
rhythm_plot
ggsave("rhythm_plot.png",
       plot = rhythm_plot,
       width = 2, height = 9, dpi = 300)

#Answer 7
box_plot <- ggplot(
  circ_df,
  aes(x = Gene, y = Expression, fill = Gene)
) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("PER2" = "steelblue",
               "BMAL1" = "firebrick")
  ) +
  labs(
    title = "Expression Distribution by Gene",
    x = "Gene",
    y = "Expression"
  ) +
  theme_minimal()
box_plot
ggsave("expression_boxplot.png",
       plot = box_plot,
       width = 1, height =7 , dpi = 300)


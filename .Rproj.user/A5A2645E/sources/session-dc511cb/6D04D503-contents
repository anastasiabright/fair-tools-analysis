library(dplyr)
library(reshape2)
library(ggplot2)


#load("output/Rdata/comparison_data.Rdata")
load("output/Rdata/selected_articles_overlapping_indicators.Rdata")
#load("output/Rdata/comparisson_overlapping_indicators.Rdata")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ F1
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
selected_articles_overlap_f1 <- selected_articles_overlap |>
  select(best_identifier, fuji_percent_f1, fair_enough_f1,
         fair_checker_f1) |>
  mutate(fuji_percent_f1 = fuji_percent_f1/100,
         fair_enough_f1 = fair_enough_f1/100,
         fair_checker_f1 = fair_checker_f1/100)

intersect_f1 <- selected_articles_overlap_f1 |>
  mutate(all_equal = (fuji_percent_f1 == fair_checker_f1) & (fair_checker_f1 == fair_enough_f1),
         fuji_checker_egual =  (fuji_percent_f1 == fair_checker_f1),
         enough_checker_equal = (fair_checker_f1 == fair_enough_f1),
         fuji_enough_equal = (fuji_percent_f1 == fair_enough_f1))

equality_summary_f1 <- intersect_f1 |>
  summarise(
    all_equal = sum(all_equal),
    fuji_checker_equal = sum(fuji_checker_egual),
    enough_checker_equal = sum(enough_checker_equal),
    fuji_enough_equal = sum(fuji_enough_equal)
  )

fuji_enough_long_f1 <- melt(as.matrix(table(intersect_f1$fuji_percent_f1, 
                                            intersect_f1$fair_checker_f1)))#, varnames =  c("FujiF1", "EnoughF1"))
enough_checker_long_f1 <- melt(as.matrix(table(intersect_f1$fair_checker_f1, 
                                                intersect_f1$fair_enough_f1)))#, varnames = c("EnoughF1", "CheckerF1"))
fuji_checker_long_f1 <- melt(as.matrix(table(intersect_f1$fuji_percent_f1, 
                                             intersect_f1$fair_enough_f1)))#, varnames = c("FujiF1", "CheckerF1"))

fuji_enough_long_f1$Comparison <- "F-UJI vs FAIR Enough"
enough_checker_long_f1$Comparison <- "FAIR Enough vs FAIR-Checker"
fuji_checker_long_f1$Comparison <- "F-UJI vs FAIR-Checker"

combined_long_f1 <- rbind(fuji_enough_long_f1, enough_checker_long_f1, fuji_checker_long_f1)
#equality_summary_long <- gather(equality_summary, key = "condition", value = "count")

ggplot(combined_long_f1, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue")+
  facet_wrap(~ Comparison, scales = "free") +
  theme_minimal() +
  labs(#title = "Heatmaps of Pairwise Comparisons for principle F1",
       x = "Tool 1",
       y = "Tool 2")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ F2
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
selected_articles_overlap_f2 <- selected_articles_overlap |>
  select(best_identifier, fuji_percent_f2, fair_enough_f2, fair_checker_f2) |>
  mutate(fuji_percent_f2 = fuji_percent_f2/100,
         fair_enough_f2 = fair_enough_f2/100,
         fair_checker_f2 = fair_checker_f2/100)

intersect_f2 <- selected_articles_overlap_f2 |>
  mutate(all_equal = (fuji_percent_f2 == fair_checker_f2) & (fair_checker_f2 == fair_enough_f2),
         fuji_checker_egual =  (fuji_percent_f2 == fair_checker_f2),
         enough_checker_equal = (fair_checker_f2 == fair_enough_f2),
         fuji_enough_equal = (fuji_percent_f2 == fair_enough_f2))

equality_summary_f2 <- intersect_f2 |>
  summarise(
    all_equal = sum(all_equal),
    fuji_checker_equal = sum(fuji_checker_egual),
    enough_checker_equal = sum(enough_checker_equal),
    fuji_enough_equal = sum(fuji_enough_equal)
  )

fuji_enough_long_f2 <- melt(as.matrix(table(intersect_f2$fuji_percent_f2,
                                            intersect_f2$fair_enough_f2)))#, varnames =  c("FujiF1", "EnoughF1"))
enough_checker_long_f2 <- melt(as.matrix( table(intersect_f2$fair_checker_f2,
                                                intersect_f2$fair_enough_f2)))#, varnames = c("EnoughF1", "CheckerF1"))
fuji_checker_long_f2 <- melt(as.matrix(table(intersect_f2$fuji_percent_f2,
                                             intersect_f2$fair_checker_f2)))#, varnames = c("FujiF1", "CheckerF1"))

fuji_enough_long_f2$Comparison <- "F-UJI vs FAIR Enough"
enough_checker_long_f2$Comparison <- "FAIR Enough vs FAIR-Checker"
fuji_checker_long_f2$Comparison <- "F-UJI vs FAIR-Checker"

combined_long_f2 <- rbind(fuji_enough_long_f2, enough_checker_long_f2, fuji_checker_long_f2)
#equality_summary_long <- gather(equality_summary, key = "condition", value = "count")

ggplot(combined_long_f2, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue")+
  facet_wrap(~ Comparison, scales = "free") +
  theme_minimal() +
  labs(#title = "Heatmaps of Pairwise Comparisons for principle F2",
       x = "Tool 1",
       y = "Tool 2")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ I1
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
selected_articles_overlap_i1 <- selected_articles_overlap |>
  select(best_identifier, fuji_percent_i1, fair_enough_i1, fair_checker_i1) |>
  mutate(fuji_percent_i1 = fuji_percent_i1/100,
         fair_enough_i1 = fair_enough_i1/100,
         fair_checker_i1 = fair_checker_i1/100)

intersect_i1 <- selected_articles_overlap_i1 |>
  mutate(all_equal = (fuji_percent_i1 == fair_checker_i1) & (fair_checker_i1 == fair_enough_i1),
         fuji_checker_egual =  (fuji_percent_i1== fair_checker_i1),
         enough_checker_equal = (fair_checker_i1 == fair_enough_i1),
         fuji_enough_equal = (fuji_percent_i1 == fair_enough_i1))

equality_summary_i1 <- intersect_i1 |>
  summarise(
    all_equal = sum(all_equal),
    fuji_checker_equal = sum(fuji_checker_egual),
    enough_checker_equal = sum(enough_checker_equal),
    fuji_enough_equal = sum(fuji_enough_equal)
  )

fuji_enough_long_i1 <- melt(as.matrix(table(intersect_i1$fuji_percent_i1, 
                                            intersect_i1$fair_enough_i1)))#, varnames =  c("FujiF1", "EnoughF1"))
enough_checker_long_i1 <- melt(as.matrix(table(intersect_i1$fair_checker_i1, 
                                               intersect_i1$fair_enough_i1)))#, varnames = c("EnoughF1", "CheckerF1"))
fuji_checker_long_i1 <- melt(as.matrix(table(intersect_i1$fuji_percent_i1, 
                                             intersect_i1$fair_checker_i1)))#, varnames = c("FujiF1", "CheckerF1"))

fuji_enough_long_i1$Comparison <- "F-UJI vs FAIR Enough"
enough_checker_long_i1$Comparison <- "FAIR Enough vs FAIR-Checker"
fuji_checker_long_i1$Comparison <- "F-UJI vs FAIR-Checker"

combined_long_i1 <- rbind(fuji_enough_long_i1, enough_checker_long_i1, fuji_checker_long_i1)
#equality_summary_long <- gather(equality_summary, key = "condition", value = "count")

ggplot(combined_long_i1, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue")+
  facet_wrap(~ Comparison, scales = "free") +
  theme_minimal() +
  labs(#title = "Heatmaps of Pairwise Comparisons for principle I2",
       x = "Tool 1",
       y = "Tool 2")
cor(intersect_i1[,2:4])
#                 fuji_percent_i1 fair_checker_i1 enough_i1
# fuji_percent_i1       1.0000000       0.5999881 0.5236363
# fair_checker_i1       0.5999881       1.0000000 0.5301828
# enough_i1             0.5236363       0.5301828 1.0000000


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ I2
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
selected_articles_overlap_i2 <- selected_articles_overlap |>
  select(best_identifier, fuji_percent_i2,fair_enough_i2, fair_checker_i2) |>
  mutate(fuji_percent_i2 = fuji_percent_i2/100,
         fair_enough_i2 = fair_enough_i2/100,
         fair_checker_i2 = fair_checker_i2/100)

intersect_i2 <- selected_articles_overlap_i2 |>
  mutate(all_equal = (fuji_percent_i2 == fair_checker_i2) & 
           (fair_checker_i2 == fair_enough_i2),
         fuji_checker_egual =  (fuji_percent_i2== fair_checker_i2),
         enough_checker_equal = (fair_checker_i2 == fair_enough_i2),
         fuji_enough_equal = (fuji_percent_i2 == fair_enough_i2))

equality_summary_i2 <- intersect_i2 |>
  summarise(
    all_equal = sum(all_equal),
    fuji_checker_equal = sum(fuji_checker_egual),
    enough_checker_equal = sum(enough_checker_equal),
    fuji_enough_equal = sum(fuji_enough_equal)
  )
cor(intersect_i2[,2:4])
#                 fuji_percent_i2 fair_checker_i2 enough_i2
# fuji_percent_i2      1.00000000     -0.05141682 0.4712789
# fair_checker_i2     -0.05141682      1.00000000 0.3347200
# enough_i2            0.47127886      0.33472001 1.0000000
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ I3
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
selected_articles_overlap_i3 <- selected_articles_overlap |>
  select(best_identifier, fuji_percent_i3,fair_enough_i3, fair_checker_i3) |>
  mutate(fuji_percent_i3 = fuji_percent_i3/100,
         fair_enough_i3 = fair_enough_i3/100,
         fair_checker_i3 = fair_checker_i3/100)

intersect_i3 <- selected_articles_overlap_i3 |>
  mutate(all_equal = (fuji_percent_i3 == fair_checker_i3) & 
           (fair_checker_i3 == fair_enough_i3),
         fuji_checker_egual =  (fuji_percent_i3== fair_checker_i3),
         enough_checker_equal = (fair_checker_i3 == fair_enough_i3),
         fuji_enough_equal = (fuji_percent_i3 == fair_enough_i3))

equality_summary_i3 <- intersect_i3 |>
  summarise(
    all_equal = sum(all_equal),
    fuji_checker_equal = sum(fuji_checker_egual),
    enough_checker_equal = sum(enough_checker_equal),
    fuji_enough_equal = sum(fuji_enough_equal)
  )
cor(intersect_i3[,2:4])
#                 fuji_percent_i3 fair_enough_i3 fair_checker_i3
# fuji_percent_i3       1.0000000      0.5940885       0.7615829
# fair_enough_i3        0.5940885      1.0000000       0.5481578
# fair_checker_i3       0.7615829      0.5481578       1.0000000

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ R1.1
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
selected_articles_overlap_r1.1 <- selected_articles_overlap |>
  select(best_identifier, `FsF-R1.1-01M`,fair_enough_r1.1,fair_checker_r1.1) |>
  mutate(fuji_percent_r1.1 = `FsF-R1.1-01M`/100,
         fair_enough_r1.1 = fair_enough_r1.1/100,
         fair_checker_r1.1 = fair_checker_r1.1/100) |>
  select(-c(`FsF-R1.1-01M`))

intersect_r1.1 <- selected_articles_overlap_r1.1 |>
  mutate(all_equal = (fuji_percent_r1.1 == fair_checker_r1.1) & 
           (fair_checker_r1.1 == fair_enough_r1.1),
         fuji_checker_egual =  (fuji_percent_r1.1== fair_checker_r1.1),
         enough_checker_equal = (fair_checker_r1.1 == fair_enough_r1.1),
         fuji_enough_equal = (fuji_percent_r1.1 == fair_enough_r1.1))

equality_summary_r1.1 <- intersect_r1.1 |>
  summarise(
    all_equal = sum(all_equal),
    fuji_checker_equal = sum(fuji_checker_egual),
    enough_checker_equal = sum(enough_checker_equal),
    fuji_enough_equal = sum(fuji_enough_equal)
  )
cor(intersect_r1.1[,2:4])
#                   fair_enough_r1.1 fair_checker_r1.1 fuji_percent_r1.1
# fair_enough_r1.1         1.0000000         0.4473962         0.7597361
# fair_checker_r1.1        0.4473962         1.0000000         0.6316566
# fuji_percent_r1.1        0.7597361         0.6316566         1.0000000

save.image("output/Rdata/comparisson_overlapping_indicators.Rdata")
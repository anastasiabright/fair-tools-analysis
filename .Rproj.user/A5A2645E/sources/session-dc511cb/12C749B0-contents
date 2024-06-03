load("output/Rdata/selected_articles_overlapping_indicators_long.Rdata")

shapiro.test(overlapping_selected_long$score[overlapping_selected_long$assessment_tool == "fuji_score_overlap"])
shapiro.test(overlapping_selected_long$score[overlapping_selected_long$assessment_tool == "enough_score"])
shapiro.test(overlapping_selected_long$score[overlapping_selected_long$assessment_tool == "checker_score"])

kruskal_result <- kruskal.test(score ~ assessment_tool, data = overlapping_selected_long)
print(kruskal_result)

pairwise.wilcox.test(overlapping_selected_long$score, 
                     overlapping_selected_long$assessment_tool, 
                     p.adjust.method = "bonferroni")

load("output/Rdata/all_overlapping_indicators_long.Rdata")
shapiro.test(overlapping_long$score[overlapping_long$assessment_tool == "fuji_score_overlap"])
shapiro.test(overlapping_long$score[overlapping_long$assessment_tool == "enough_score"])
shapiro.test(overlapping_long$score[overlapping_long$assessment_tool == "checker_score"])

kruskal_result <- kruskal.test(score ~ assessment_tool, data = overlapping_long)
print(kruskal_result)

pairwise.wilcox.test(overlapping_long$score, 
                     overlapping_long$assessment_tool, 
                     p.adjust.method = "bonferroni")
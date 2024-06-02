load("output/Rdata/selected_articles_overlapping_indicators_long.Rdata")


shapiro.test(overlapping_selected_long$score[overlapping_selected_long$assessment_tool == "fuji_score_overlap"])
shapiro.test(overlapping_selected_long$score[overlapping_selected_long$assessment_tool == "enough_score"])
shapiro.test(overlapping_selected_long$score[overlapping_selected_long$assessment_tool == "checker_score"])
#In all three cases (FUJI, FAIR Enough, FAIR-Checker), the p-values are significantly less than 0.05, 
# indicating that the scores for each assessment tool are not normally distributed.

kruskal_result <- kruskal.test(score ~ assessment_tool, data = overlapping_selected_long)
print(kruskal_result)
#	Kruskal-Wallis rank sum test
# data:  score by assessment_tool
# Kruskal-Wallis chi-squared = 3.2203, df = 2, p-value = 0.1999

# The Kruskal-Wallis test compares the distributions of scores across the three 
# different assessment tools. Here are the interpretations:
  
#   Null Hypothesis (H0): The distributions of scores are the same across the different assessment tools.
# Alternative Hypothesis (H1): At least one assessment tool has a different distribution of scores.

# p-value = 0.1999: This is greater than the typical significance level of 0.05. 
# Therefore, we fail to reject the null hypothesis. This suggests that there is 
# no statistically significant difference in the distributions of scores among the three assessment tools.

pairwise.wilcox.test(overlapping_selected_long$score, 
                     overlapping_selected_long$assessment_tool, 
                     p.adjust.method = "bonferroni")

# Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
# 
# data:  overlapping_selected_long$score and overlapping_selected_long$assessment_tool 
# 
#             checker_score enough_score
# enough_score       1.000         -           
# fuji_score_overlap 0.027         1.000       
# 
# Checker vs. FAIR Enough: p-value = 1.000
# No significant difference in scores between the "Checker" and "FAIR Enough" assessment tools.

# Checker vs. FUJI: p-value = 0.027
# This p-value is less than 0.05, suggesting a significant difference in scores
# between the "Checker" and "FUJI" assessment tools. 
# However, note that the Bonferroni adjustment is very conservative, 
# and this significance may warrant further exploration given the context of your analysis.

# FAIR Enough vs. FUJI: p-value = 1.000
# No significant difference in scores between the "FAIR Enough" and "FUJI" assessment tools.

# The Kruskal-Wallis test indicates no overall significant difference in score 
# distributions across the three assessment tools.
# Pairwise comparisons reveal a significant difference between "Checker" and "FUJI" 
# but no significant differences between the other pairs.
# Given the mixed results, further investigation or additional data might be necessary 
# to draw more robust conclusions.


load("output/Rdata/all_overlapping_indicators_long.Rdata")
shapiro.test(overlapping_long$score[overlapping_long$assessment_tool == "fuji_score_overlap"])
shapiro.test(overlapping_long$score[overlapping_long$assessment_tool == "enough_score"])
shapiro.test(overlapping_long$score[overlapping_long$assessment_tool == "checker_score"])
#In all three cases (FUJI, FAIR Enough, FAIR-Checker), the p-values are significantly less than 0.05, 
# indicating that the scores for each assessment tool are not normally distributed.

kruskal_result <- kruskal.test(score ~ assessment_tool, data = overlapping_long)
print(kruskal_result)
# Kruskal-Wallis chi-squared = 75.446, df = 2, p-value < 2.2e-16
# p-value < 2.2e-16: This extremely small p-value indicates a highly significant result. 
# Therefore, we reject the null hypothesis, suggesting that there are significant 
# differences in the distributions of scores among the three assessment tools.

pairwise.wilcox.test(overlapping_long$score, 
                     overlapping_long$assessment_tool, 
                     p.adjust.method = "bonferroni")

# Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
# 
# data:  overlapping_long$score and overlapping_long$assessment_tool 
# 
#               checker_score enough_score
# enough_score       1.0e-15       -           
# fuji_score_overlap 4.0e-06       7.6e-10     
# 
# P value adjustment method: bonferroni 

# All pairwise comparisons show extremely small p-values (after Bonferroni adjustment), 
# indicating strong evidence of significant differences in the distributions of scores 
# between each pair of assessment tools. This suggests that each tool assesses 
# the scores quite differently from each other.
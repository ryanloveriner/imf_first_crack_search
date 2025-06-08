library(tidyverse)
library(readxl)
library(ggplot2)
library(car)
library(nortest)



# FIRST DOWNLOAD THE ASSOCIATED DATA FROM THE REPO, THEN COPY AND PASTE THE FILE AS PATH INTO THE READ FUNCTION BELOW
inlet_data=read.csv()



##### PEAK TEMP TIME #####

# Plot a pairwise comparison of peak inlet temperature time across green beans
inlet_peak_time_box_plot <- ggplot(inlet_data, aes(x = `Green Id`, y= `Peak Inlet Temp Time`))
# Plot a reference line for the global mean
inlet_peak_time_box_plot <- inlet_peak_time_box_plot + geom_hline(yintercept = mean(inlet_data$`Peak Inlet Temp Time`),
                                                                  colour = "black", linetype = "dashed", size = 0.3, alpha = 0.5)
# Box plot
inlet_peak_time_box_plot <- inlet_peak_time_box_plot + geom_boxplot(width = 0.25, alpha = 0.25)
# Points for observed data
inlet_peak_time_box_plot <- inlet_peak_time_box_plot + geom_point()
# Diamond at mean for each group
inlet_peak_time_box_plot <- inlet_peak_time_box_plot + stat_summary(fun = mean, geom = "point", shape = 18, size = 4,
                                                                          colour = "red", alpha = 0.8)
# Confidence limits based on normal distribution
inlet_peak_time_box_plot <- inlet_peak_time_box_plot + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
                                         width = .2, colour = "red", alpha = 0.8)
# Plot labs
inlet_peak_time_box_plot <- inlet_peak_time_box_plot + labs(x =     "Green Coffee Lot"
                                                          , y =     "Time of Peak Inlet Temperature (sec)"
                                                          , title = "Peak Inlet Temperature Time by Green Bean")
# Print box plot
print(inlet_peak_time_box_plot)



# Hypothesis test: The population mean peak inlet time is different across bean groups.
inlet_peak_aov <- aov(`Peak Inlet Temp Time` ~ `Green Id`, data = inlet_data)
summary(inlet_peak_aov)
# P-value 3.71e-16 < 0.05, reject null and conclude SIGNIFICANT DIFFERENCE in group means.

# Histogram plot of residuals to analyze for normality
inlet_peak_time_residuals <- data.frame(res = inlet_peak_aov$residuals)
residuals_plot <- ggplot(inlet_peak_time_residuals, aes(x = res))
residuals_plot <- residuals_plot + geom_histogram(aes(y = ..density..), binwidth = 0.5)
residuals_plot <- residuals_plot + geom_density(colour = "blue")
residuals_plot <- residuals_plot + geom_rug()
residuals_plot <- residuals_plot + stat_function(fun = dnorm, colour = "red", args = list(mean = mean(inlet_peak_time_residuals$res), sd = sd(inlet_peak_time_residuals$res)))
residuals_plot <- residuals_plot + labs(x = "Residuals"
                                      , y = "Density"
                                      , title = "Kernal Density vs Datapoint Residuals"
                                      , caption = "Blue = Kernal density curve\nRed = Normal distribution")
print(residuals_plot)

# QQ Plot of data to analyze for normality
peakTimeqq <- car::qqPlot(inlet_data$`Peak Inlet Temp Time`,
                          main = "QQ plot of peak temperature time",
                          xlab = "Theoretical Quantiles",
                          ylab = "Peak Inlet Temp Time (s)")
print(peakTimeqq)
## S-shape distribution indicates excessive central density


# Anderson-Darling test for normality
nortest::ad.test(inlet_peak_aov$residuals)
# P-value 0.02 < 0.05, reject null and conclude LACK OF NORMALITY

# Lacking normality, use Levene's Test for Homogeneity of Variance
car::leveneTest(`Peak Inlet Temp Time` ~ `Green Id`, data = inlet_data)
#P-value 0.14 > 0.05, reject null and conclude HOMOGENEITY OF VARIANCE

# Although we have shown homogeneity of variance, the lack of normality in residuals makes ANOVA inappropriate. We will use the Kruskal-Wallis test instead.
kruskal.test(`Peak Inlet Temp Time` ~ `Green Id`, data = inlet_data)
# P-value 4.57e-06 < 0.05, reject null and conclude SIGNIFICANT DIFFERENCE IN MEDIANS



##### PEAK TEMP DIFFERENCE #####

# Plot a pairwise comparison of peak inlet temperature difference from recipe across green beans
inlet_peak_temp_box_plot <- ggplot(inlet_data, aes(x = `Green Id`, y= `Roast Recipe Inlet Difference at Peak`))
# Plot a reference line for the global mean
inlet_peak_temp_box_plot <- inlet_peak_temp_box_plot + geom_hline(yintercept = mean(inlet_data$`Roast Recipe Inlet Difference at Peak`),                                                                        colour = "black", linetype = "dashed", size = 0.3, alpha = 0.5)
# Box plot
inlet_peak_temp_box_plot <- inlet_peak_temp_box_plot + geom_boxplot(width = 0.25, alpha = 0.25)
# Points for observed data
inlet_peak_temp_box_plot <- inlet_peak_temp_box_plot + geom_point()
# Diamond at mean for each group
inlet_peak_temp_box_plot <- inlet_peak_temp_box_plot + stat_summary(fun = mean, geom = "point", shape = 18, size = 4,
                                                                    colour = "red", alpha = 0.8)
# Confidence limits based on normal distribution
inlet_peak_temp_box_plot <- inlet_peak_temp_box_plot + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
                                                                    width = .2, colour = "red", alpha = 0.8)
# Plot labs
inlet_peak_temp_box_plot <- inlet_peak_temp_box_plot + labs(x =     "Green Coffee Lot"
                                                          , y =     "Peak Inlet Temp Diff (F)"
                                                          , title = "Peak Inlet Temperature Difference From Recipe by Green Bean")
# Print box plot
print(inlet_peak_temp_box_plot)

# Hypothesis test: The population mean peak inlet temperature difference is different across bean groups
inlet_peak_aov2 <- aov(`Roast Recipe Inlet Difference at Peak` ~ `Green Id`, data = inlet_data)
summary(inlet_peak_aov2)
# P-value 2.66e-10 < 0.05, reject null and conclude SIGNIFICANT DIFFERENCE IN MEANS

# Plot residuals to analyze for normality
inlet_peak_temp_residuals2 <- data.frame(res = inlet_peak_aov2$residuals)
residuals_plot2 <- ggplot(inlet_peak_temp_residuals2, aes(x = res))
residuals_plot2 <- residuals_plot2 + geom_histogram(aes(y = ..density..), binwidth = 0.5)
residuals_plot2 <- residuals_plot2 + geom_density(colour = "blue")
residuals_plot2 <- residuals_plot2 + geom_rug()
residuals_plot2 <- residuals_plot2 + stat_function(fun = dnorm, colour = "red", args = list(mean = mean(inlet_peak_temp_residuals2$res), sd = sd(inlet_peak_temp_residuals2$res)))
residuals_plot2 <- residuals_plot2 + labs(x = "Residuals"
                                        , y = "Density",
                                        , title = "Kernal Density vs Datapoint Residuals"
                                        , caption = "Blue = Kernal density curve\nRed = Normal distribution")
print(residuals_plot2)

# Anderson-Darling test for normality
nortest::ad.test(inlet_peak_aov$residuals)
# P-value 0.05 = 0.05, conclude THRESHOLD OF NORMALITY

# Bartlett's Test for Homogeneity of Variance (assumes normality)
bartlett.test(`Roast Recipe Inlet Difference at Peak` ~ `Green Id`, data = inlet_data)
# P-value 0.24 < 0.05, fail to reject null and conclude HOMOGENEITY OF VARIANCE

# Levene's Test for Homogeneity of Variance (doesn't assume normality)
car::leveneTest(`Roast Recipe Inlet Difference at Peak` ~ `Green Id`, data = inlet_data)
# p-value 0.67 > 0.05, reject null and conclude HOMOGENEITY OF VARIANCE

# As a safeguard, corroborate ANOVA results with the Kruskal-Wallis test
kruskal.test(`Roast Recipe Inlet Difference at Peak` ~ `Green Id`, data = inlet_data)
# P-value 1.2e-05 < 0.05, reject null and conclude DIFFERENCE IN MEDIANS



##### PEAK ROR TIME #####

#Plot a pairwise comparison of peak inlet RoR time across green beans
inlet_peak_ror_time_box_plot <- ggplot(inlet_data, aes(x = `Green Id`, y= `Peak Inlet RoR Time`))
# Plot a reference line for the global mean
inlet_peak_ror_time_box_plot <- inlet_peak_ror_time_box_plot + geom_hline(yintercept = mean(inlet_data$`Peak Inlet RoR Time`),
                                                                     colour = "black", linetype = "dashed", size = 0.3, alpha = 0.5)
# Box plot
inlet_peak_ror_time_box_plot <- inlet_peak_ror_time_box_plot + geom_boxplot(width = 0.25, alpha = 0.25)
# Points for observed data
inlet_peak_ror_time_box_plot <- inlet_peak_ror_time_box_plot + geom_point()
# Diamond at mean for each group
inlet_peak_ror_time_box_plot <- inlet_peak_ror_time_box_plot + stat_summary(fun = mean, geom = "point", shape = 18, size = 4,
                                                                    colour = "red", alpha = 0.8)
# Confidence limits based on normal distribution
inlet_peak_ror_time_box_plot <- inlet_peak_ror_time_box_plot + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
                                                                    width = .2, colour = "red", alpha = 0.8)
# Plot labs
inlet_peak_ror_time_box_plot <- inlet_peak_ror_time_box_plot + labs(x =     "Green Coffee Lot"
                                                            , y =     "Peak Inlet RoR Time (sec)"
                                                            , title = "Peak Inlet RoR Time by Green Bean")
# Print box plot
print(inlet_peak_ror_time_box_plot)



# Hypothesis test: The population mean peak inlet temperature difference is different across bean groups
inlet_peak_aov3 <- aov(`Peak Inlet RoR Time` ~ `Green Id`, data = inlet_data)
summary(inlet_peak_aov3)
# P-value 2.88e-13 < 0.05, reject null and conclude SIGNIFICANT DIFFERENCE IN MEANS

# Plot residuals to analyze for normality
inlet_peak_temp_residuals3 <- data.frame(res = inlet_peak_aov3$residuals)
residuals_plot3 <- ggplot(inlet_peak_temp_residuals3, aes(x = res))
residuals_plot3 <- residuals_plot3 + geom_histogram(aes(y = ..density..), binwidth = 0.5)
residuals_plot3 <- residuals_plot3 + geom_density(colour = "blue")
residuals_plot3 <- residuals_plot3 + geom_rug()
residuals_plot3 <- residuals_plot3 + stat_function(fun = dnorm, colour = "red", args = list(mean = mean(inlet_peak_temp_residuals3$res), sd = sd(inlet_peak_temp_residuals3$res)))
residuals_plot3 <- residuals_plot3 + labs(x = "Residuals"
                                          , y = "Density",
                                          , title = "Kernal Density vs Datapoint Residuals"
                                          , caption = "Blue = Kernal density curve\nRed = Normal distribution")
print(residuals_plot3)

# Anderson-Darling test for normality
nortest::ad.test(inlet_peak_aov3$residuals)
# P-value 0.17, fail to reject null and conclude NORMALITY

# Given normality, use Bartlett's Test for Homogeneity of Variance 
bartlett.test(`Peak Inlet RoR Time` ~ `Green Id`, data = inlet_data)
# P-value 0.05 = 0.05, conclude THRESHOLD OF HOMOGENEITY

# Although we have shown normality, the distribution's nonhomogeneous variance makes us reject the metric as statistically unreliable



##### PEAK ROR TEMP #####

# Plot a pairwise comparison of peak inlet RoR across green beans
inlet_peak_ror_box_plot <- ggplot(inlet_data, aes(x = `Green Id`, y= `Peak Inlet RoR`))
# Plot a reference line for the global mean
inlet_peak_ror_box_plot <- inlet_peak_ror_box_plot + geom_hline(yintercept = mean(inlet_data$`Peak Inlet RoR`),
                                                                colour = "black", linetype = "dashed", size = 0.3, alpha = 0.5)
# Box plot
inlet_peak_ror_box_plot <- inlet_peak_ror_box_plot + geom_boxplot(width = 0.25, alpha = 0.25)
# Points for observed data
inlet_peak_ror_box_plot <- inlet_peak_ror_box_plot + geom_point()
# Diamond at mean for each group
inlet_peak_ror_box_plot <- inlet_peak_ror_box_plot + stat_summary(fun = mean, geom = "point", shape = 18, size = 4,
                                                                    colour = "red", alpha = 0.8)
# Confidence limits based on normal distribution
inlet_peak_ror_box_plot <- inlet_peak_ror_box_plot + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
                                                                    width = .2, colour = "red", alpha = 0.8)
# Plot labs
inlet_peak_ror_box_plot <- inlet_peak_ror_box_plot + labs(x =     "Green Coffee Lot"
                                                            , y =     "Peak Inlet RoR (F/15sec)"
                                                            , title = "Peak Inlet RoR by Green Bean")
# Print box plot
print(inlet_peak_ror_box_plot)

# Hypothesis test: The population mean peak inlet temperature difference is different between bean groups
inlet_peak_aov4 <- aov(`Peak Inlet RoR` ~ `Green Id`, data = inlet_data)
summary(inlet_peak_aov4)
# P-value 0.0003 < 0.05, reject null and conclude SIGNIFICANT DIFFERENCE IN MEANS

# Plot residuals to analyze for normality
inlet_peak_temp_residuals4 <- data.frame(res = inlet_peak_aov4$residuals)
residuals_plot4 <- ggplot(inlet_peak_temp_residuals4, aes(x = res))
residuals_plot4 <- residuals_plot4 + geom_histogram(aes(y = ..density..), binwidth = 0.5)
residuals_plot4 <- residuals_plot4 + geom_density(colour = "blue")
residuals_plot4 <- residuals_plot4 + geom_rug()
residuals_plot4 <- residuals_plot4 + stat_function(fun = dnorm, colour = "red", args = list(mean = mean(inlet_peak_temp_residuals4$res), sd = sd(inlet_peak_temp_residuals4$res)))
residuals_plot4 <- residuals_plot4 + labs(x = "Residuals"
                                          , y = "Density",
                                          , title = "Kernal Density vs Datapoint Residuals"
                                          , caption = "Blue = Kernal density curve\nRed = Normal distribution")
print(residuals_plot4)

# Anderson-Darling test for normality 
nortest::ad.test(inlet_peak_aov4$residuals)
# P-value 0.17 > 0.05, fail to reject null and conclude NORMALITY

# Given normality, use Bartlett's Test for Homogeneity of Variance 
bartlett.test(`Peak Inlet RoR` ~ `Green Id`, data = inlet_data)
# P-value 0.38 < 0.05, fail to reject null and conclude HOMOGENEITY OF VARIANCE
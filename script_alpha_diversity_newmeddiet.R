library(tidyverse)
library(qiime2R)
library(ggpubr)

##Load Data
# In the code, the text before = is what the file will be called in R. 
# Make this short but unique as this is how you will tell R to use this 
# file in later commands.

# header: tells R that the first row is column names, not data
# row.names: tells R that the first column is row names, not data
# sep: tells R that the data are tab-delimited. 
# If you had a comma-delimited file, you would us sep=","

# Load data
getwd()

metaproj<-read_q2metadata("metadata_project_updated.txt")
str(metaproj)
colnames(metaproj)[2] <- "time"
colnames(metaproj)[3] <- "gender"
colnames(metaproj)[4] <- "treatment"
colnames(metaproj)[5] <- "percent.weightchange"
str(metaproj)

evenness = read_qza("evenness_vector.qza")
evenness<-evenness$data %>% rownames_to_column("SampleID") # this moves the sample names to a new column that matches the metadata and allows them to be merged

observed_features = read_qza("observed_features_vector.qza")
observed_features<-observed_features$data %>% rownames_to_column("SampleID") # this moves the sample names to a new column that matches the metadata and allows them to be merged

shannon = read_qza("shannon_vector.qza")
shannon<-shannon$data %>% rownames_to_column("SampleID") # this moves the sample names to a new column that matches the metadata and allows them to be merged

faith_pd = read_qza("faith_pd_vector.qza")
faith_pd<-faith_pd$data %>% rownames_to_column("SampleID") # this moves the sample names to a new column that matches the metadata and allows them to be merged\
faith_pd$SampleID <- NULL
colnames(faith_pd)[1] <- "SampleID"
colnames(faith_pd)[2] <- "faith_pd"
str(metaproj)

#observed_features$observed_features_num <- lapply(observed_features$observed_features, as.numeric)
#observed_features$observed_features <- as.numeric(observed_features$observed_features)

###Alpha Diversity tables
# These tables will be merged for convenience and added to the 
# metadata table as the original tutorial was organized.

alpha_diversity = merge(x=faith_pd, y=evenness, by.x = "SampleID", by.y = "SampleID", all=TRUE)
alpha_diversity = merge(alpha_diversity, observed_features, by.x = "SampleID", by.y = "SampleID", all=TRUE)
alpha_diversity = merge(alpha_diversity, shannon, by.x = "SampleID", by.y = "SampleID", all=TRUE)
metaproj = merge(metaproj, alpha_diversity, by.x = "SampleID", by.y = "SampleID", all=TRUE)
row.names(metaproj) <- metaproj$SampleID
#meta = meta[,-1]
str(metaproj)
hist(metaproj$shannon_entropy, main="Shannon diversity", xlab="", breaks=10)
str(metaproj)

hist(metaproj$faith_pd, main="Faith phylogenetic diversity", xlab="", breaks=10)



hist(metaproj$pielou_evenness, main="Evenness", xlab="", breaks=10)


hist(metaproj$observed_features, main="Observed Features", xlab="", breaks=10)


#Plots the qq-plot for residuals
ggqqplot(metaproj$shannon_entropy, title = "Diversity Index_Shannon")

ggqqplot(metaproj$pielou_evenness, title = "Diversity Index_Evenness")

ggqqplot(metaproj$faith_pd, title = "Faith phylogenetic diversity ")

ggqqplot(metaproj$observed_features, title = "Observed Features")



library(tidyr)
library(ggplot2)
library("ggpubr")

#Filter metaproj 
meddiet <- metaproj[metaproj$treatment == "meddiet", ]
meddiet$time <- factor(meddiet$time, levels = c("T1", "T2"))
# Changing 'time' column with the timepoint information
meddiet$time <- factor(meddiet$time, levels = c("T1", "T2"), labels = c("Baseline", "Post-Intervention"))
colnames(meddiet)[2]<-"Timepoint"


boxplot(observed_features ~ `Timepoint`, data=meddiet, ylab="Observed Features")

boxplot(pielou_evenness ~ `Timepoint`, data=meddiet, ylab="Evenness")

boxplot(faith_pd ~ `Timepoint`, data=meddiet, ylab="Faith_PD")

boxplot(shannon_entropy ~ `Timepoint`, data=meddiet, ylab="Shannon")   

# To test for normalcy statistically, we can run the Shapiro-Wilk 
# test of normality.

shapiro.test(meddiet$shannon)
shapiro.test(meddiet$faith_pd)
shapiro.test(meddiet$pielou_evenness)
shapiro.test(meddiet$observed_features)

ggqqplot(meddiet$shannon_entropy, title = "Diversity Index_Shannon")

ggqqplot(meddiet$pielou_evenness, title = "Diversity Index_Evenness")

ggqqplot(meddiet$faith_pd, title = "Faith phylogenetic diversity ")

ggqqplot(meddiet$observed_features, title = "Observed Features")


hist(meddiet$faith_pd, main="Faith phylogenetic diversity", xlab="", breaks=10)

hist(meddiet$pielou_evenness, main="Evenness", xlab="", breaks=10)


hist(meddiet$observed_features, main="Observed Features", xlab="", breaks=10)

hist(meddiet$shannon_entropy, main="Shannon Diversity", xlab="", breaks=10)


# Now, the above graph is kind of not correct. Our test and our graphic do not exactly match. ANOVA and Tukey are tests based on the mean, but the boxplot plots the median. Its not wrong, its just not the best method. Unfortunately plotting the average and standard deviation is a little complicated.

observed_summary <- medddiet %>%
  group_by(Timepoint) %>%
  summarise(
    mean_observed = mean(observed_features, na.rm = TRUE),
    sd_observed = sd(observed_features, na.rm = TRUE),
    n_observed = n(),
    se_observed = sd(observed_features, na.rm = TRUE)/sqrt(n())
  )
print(observed_summary)

evenness_summary <- meddiet %>%
  group_by(Timepoint) %>%
  summarise(
    mean_evenness = mean(pielou_evenness, na.rm = TRUE),
    sd_evenness = sd(pielou_evenness, na.rm = TRUE),
    n_evenness = n(),
    se_evenness = sd(pielou_evenness, na.rm = TRUE)/sqrt(n())
  )
print(evenness_summary)

faith_summary <- meddiet %>%
  group_by(Timepoint) %>%
  summarise(
    mean_faith = mean(faith_pd, na.rm = TRUE),
    sd_faith = sd(faith_pd, na.rm = TRUE),
    n_faith = n(),
    se_faith = sd(faith_pd, na.rm = TRUE)/sqrt(n())
  )
print(faith_summary)


shannon_summary <- meddiet %>%
  group_by(Timepoint) %>%
  summarise(
    mean_shannon = mean(shannon_entropy, na.rm = TRUE),
    sd_shannon = sd(shannon_entropy, na.rm = TRUE),
    n_shannon = n(),
    se_shannon = sd(shannon_entropy, na.rm = TRUE)/sqrt(n())
  )
print(shannon_summary)


observed_se <- ggplot(observed_summary, aes(x = `Timepoint`, y = mean_observed, fill = `Timepoint`)) +
  geom_col(width=0.7) + 
  geom_errorbar(aes(ymin = mean_observed - se_observed, ymax = mean_observed + se_observed), width = 0.1) +
  scale_fill_manual(values = c("Baseline" = "#56B4E9", "Post-Intervention" = "#E69F00")) +
  theme_q2r() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.1, size = 10), # Increase size for x axis labels
    axis.text.y = element_text(size = 10), # Increase size for y axis labels
    axis.title.x = element_text(size = 12), # Increase size for x axis title
    axis.title.y = element_text(size = 12), # Increase size for y axis title
    legend.title = element_blank(),
    legend.text = element_text(size = 12) # Increase size for legend text
  ) +
  labs(y = "Observed Features", x = "Timepoint") +
  guides(fill = guide_legend(title = "Timepoints", title.position = "top", title.theme = element_text(size = 14))) # Increase legend title size

print(observed_se)


shannon_se <- ggplot(shannon_summary, aes(x = `Timepoint`, y = mean_shannon, fill = `Timepoint`)) +
  geom_col(width=0.7) + 
  geom_errorbar(aes(ymin = mean_shannon - se_shannon, ymax = mean_shannon + se_shannon), width = 0.1) +
  scale_fill_manual(values = c("Baseline" = "#56B4E9", "Post-Intervention" = "#E69F00")) +
  theme_q2r() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.1, size = 10), # Increase size for x axis labels
    axis.text.y = element_text(size = 10), # Increase size for y axis labels
    axis.title.x = element_text(size = 12), # Increase size for x axis title
    axis.title.y = element_text(size = 12), # Increase size for y axis title
    legend.title = element_blank(),
    legend.text = element_text(size = 12) # Increase size for legend text
  ) +
  labs(y = "Shannon Diversity", x = "Timepoint") +
  guides(fill = guide_legend(title = "Timepoints", title.position = "top", title.theme = element_text(size = 14))) # Increase legend title size

print(shannon_se)




evenness_se <- ggplot(evenness_summary, aes(x = `Timepoint`, y = mean_evenness, fill = `Timepoint`)) +
  geom_col(width=0.7) + 
  geom_errorbar(aes(ymin = mean_evenness - se_evenness, ymax = mean_evenness + se_evenness), width = 0.1) +
  scale_fill_manual(values = c("Baseline" = "#56B4E9", "Post-Intervention" = "#E69F00")) +
  theme_q2r() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.1, size = 10), # Increase size for x axis labels
    axis.text.y = element_text(size = 10), # Increase size for y axis labels
    axis.title.x = element_text(size = 12), # Increase size for x axis title
    axis.title.y = element_text(size = 12), # Increase size for y axis title
    legend.title = element_blank(),
    legend.text = element_text(size = 12) # Increase size for legend text
  ) +
  labs(y = "Pielou Evenness", x = "Timepoint") +
  guides(fill = guide_legend(title = "Timepoints", title.position = "top", title.theme = element_text(size = 14))) # Increase legend title size

print(evenness_se)


faith_se <- ggplot(faith_summary, aes(x = `Timepoint`, y = mean_faith, fill = `Timepoint`)) +
  geom_col(width=0.7) + 
  geom_errorbar(aes(ymin = mean_faith - se_faith, ymax = mean_faith + se_faith), width = 0.1) +
  scale_fill_manual(values = c("Baseline" = "#56B4E9", "Post-Intervention" = "#E69F00")) +
  theme_q2r() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.1, size = 10), # Increase size for x axis labels
    axis.text.y = element_text(size = 10), # Increase size for y axis labels
    axis.title.x = element_text(size = 12), # Increase size for x axis title
    axis.title.y = element_text(size = 12), # Increase size for y axis title
    legend.title = element_blank(),
    legend.text = element_text(size = 12) # Increase size for legend text
  ) +
  labs(y = "Faith PD", x = "Timepoint") +
  guides(fill = guide_legend(title = "Timepoints", title.position = "top", title.theme = element_text(size = 14))) # Increase legend title size

print(faith_se)


#####Performing two sample t-test on various alpha diversity metrics
library(tidyverse)
library(ggplot2)

######Normality check
Baseline<- meddiet$shannon_entropy[meddiet$Timepoint == "Baseline"]
'Post-Intervention'<- meddiet$shannon_entropy[meddiet$Timepoint == "Post-Intervention"]

shapiro.test(Baseline)
##Given that the data is not normally distributed, i will use wilcox.test
# Calculate the differences
meddiet$differences <- meddiet$'Post-Intervention' - meddiet$Baseline


#Given that the data is not normally distributed, Wilcoxon test would be the appropriate test to use 
#Pielou Evenness
baseline <- meddiet$pielou_evenness[meddiet$Timepoint == 'Baseline']
post_intervention <- meddiet$pielou_evenness[meddiet$Timepoint == 'Post-Intervention']
evenness_differences <- post_intervention - baseline
print(evenness_differences)
mean_differences <- mean(evenness_differences, na.rm = TRUE)
evenness_differences[is.na(evenness_differences)] <- mean_differences
print(evenness_differences)
mean(evenness_differences)
sd(evenness_differences)
wilcox_evenness <- wilcox.test(evenness_differences, conf.int = TRUE, exact = FALSE, correct = FALSE)
print(wilcox_evenness)


#Shannon Diversity
baseline <- meddiet$shannon_entropy[meddiet$Timepoint == 'Baseline']
post_intervention <- meddiet$shannon_entropy[meddiet$Timepoint == 'Post-Intervention']
shannon_differences <- post_intervention - baseline
print(shannon_differences)
mean_differences <- mean(shannon_differences, na.rm = TRUE)
shannon_differences[is.na(shannon_differences)] <- mean_differences
print(shannon_differences)
mean(shannon_differences)
sd(shannon_differences)
wilcox_shannon <- wilcox.test(shannon_differences, conf.int = TRUE, exact = FALSE, correct = FALSE)
print(wilcox_shannon)


#Observed Features
baseline <- meddiet$observed_features[meddiet$Timepoint == 'Baseline']
post_intervention <- meddiet$observed_features[meddiet$Timepoint == 'Post-Intervention']
observed_differences <- post_intervention - baseline
print(observed_differences)
mean_differences <- mean(observed_differences, na.rm = TRUE)
observed_differences[is.na(observed_differences)] <- mean_differences
print(observed_differences)
mean(evenness_differences)
sd(evenness_differences)
mean(observed_differences)
sd(observed_differences)
wilcox_observed_result <- wilcox.test(observed_differences, conf.int = TRUE, exact = FALSE, correct = FALSE)
print(wilcox_observed_result)


#Fath PD
baseline <- meddiet$faith_pd[meddiet$Timepoint == 'Baseline']
post_intervention <- meddiet$faith_pd[meddiet$Timepoint == 'Post-Intervention']
faith_differences <- post_intervention - baseline
print(faith_differences)
faith_differences[is.na(faith_differences)] <- 0
print(faith_differences)
mean(faith_differences)
sd(faith_differences)

wilcox_result <- wilcox.test(faith_differences, conf.int = TRUE, exact = FALSE, correct = FALSE)
print(wilcox_result)


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
     
     boxplot(observed_features ~ `treatment`, data=metaproj, ylab="Observed Features")
     
     boxplot(faith_pd ~ `treatment`, data=metaproj, ylab="Faith phylogenetic diversity")
     
     boxplot(shannon_entropy ~ `treatment`, data=metaproj, ylab="shannon diversity")
     
     boxplot(pielou_evenness ~ `treatment`, data=metaproj, ylab="Evenness")
     
     
     # To test for normalcy statistically, we can run the Shapiro-Wilk 
     # test of normality.
     
     shapiro.test(metaproj$shannon)
     shapiro.test(metaproj$faith_pd)
     shapiro.test(metaproj$pielou_evenness)
     shapiro.test(metaproj$observed_features)
     
     # Now, the above graph is kind of not correct. Our test and our graphic do not exactly match. ANOVA and Tukey are tests based on the mean, but the boxplot plots the median. Its not wrong, its just not the best method. Unfortunately plotting the average and standard deviation is a little complicated.
     
     observed_summary <- meddiet %>%
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
       labs(y = "Shannon Entropy", x = "Timepoint") +
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
     
     
     
     
     
     ggsave("output/shannon_se1.png", observed_se, height = 2.5, width = 3)
     
     
     
     ggsave("output/observed_se.png", observed_se, height = 2.5, width = 3)
     
     
     
     ggsave("output/observed_se.png", observed_se, height = 2.5, width = 3)
     
     
     
     
     metaproj$treatment_ord <- factor(rep(c("meddiet", "weightloss"), length.out = nrow(metaproj)))
     
     levels(metaproj$`treatment`)
     levels(metaproj$treatment_ord)
    
     
     boxplot(observed_features ~ `treatment`, data=metaproj, ylab="Observed Features")
     
     boxplot(faith_pd ~ `treatment`, data=metaproj, ylab="Faith phylogenetic diversity")
     
     boxplot(shannon_entropy ~ `treatment`, data=metaproj, ylab="shannon diversity")
     
     boxplot(pielou_evenness ~ `treatment`, data=metaproj, ylab="Evenness")
     
     evenness_boxplot <- ggplot(metaproj, aes(x = treatment_ord, y = pielou_evenness)) +
       geom_boxplot() +
       geom_jitter(aes(color = treatment_ord), width = 0.2, alpha = 0.5) +  # Add color to the jittered points
       theme_q2r() +
       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
       scale_color_brewer(palette = "Set1")  # Apply a color palette for the points
     
     ggsave("output/evenness_boxplot.png", plot = evenness_boxplot, height = 3, width = 3)
     
     
     faith_boxplot <- ggplot(metaproj, aes(x = treatment_ord, y = faith_pd)) +
       geom_boxplot() +
       geom_jitter(aes(color = treatment_ord), width = 0.2, alpha = 0.5) +  # Add color to the jittered points
       theme_q2r() +
       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
       scale_color_brewer(palette = "Set1")  # Apply a color palette for the points
     
     ggsave("output/faith_boxplot.png", plot = faith_boxplot, height = 3, width = 3)
     
     observed_boxplot <- ggplot(metaproj, aes(x = treatment_ord, y = observed_features)) +
       geom_boxplot() +
       geom_jitter(aes(color = treatment_ord), width = 0.2, alpha = 0.5) +  # Add color to the jittered points
       theme_q2r() +
       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
       scale_color_brewer(palette = "Set1")  # Apply a color palette for the points
     
     ggsave("output/observed_boxplot.png", plot = observed_boxplot, height = 3, width = 3)
     
     
     shannon_boxplot <- ggplot(metaproj, aes(x = treatment_ord, y = shannon_entropy)) +
       geom_boxplot() +
       geom_jitter(aes(color = treatment_ord), width = 0.2, alpha = 0.5) +  # Add color to the jittered points
       theme_q2r() +
       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
       scale_color_brewer(palette = "Set1")  # Apply a color palette for the points
     
     ggsave("output/shannon_boxplot.png", plot = shannon_boxplot, height = 3, width = 3)
     
      
     # Now, the above graph is kind of not correct. Our test and our graphic do not exactly match. ANOVA and Tukey are tests based on the mean, but the boxplot plots the median. Its not wrong, its just not the best method. Unfortunately plotting the average and standard deviation is a little complicated.
     
     observed_summary <- metaproj %>%
       group_by(treatment_ord) %>%
       summarise(
         mean_observed = mean(observed_features, na.rm = TRUE),
         sd_observed = sd(observed_features, na.rm = TRUE),
         n_observed = n(),
         se_observed = sd(observed_features, na.rm = TRUE)/sqrt(n())
       )
     
     
     observed_se <- ggplot(observed_summary, aes(`treatment`, mean_observed, fill = `treatment_ord`)) + 
       geom_col() + 
       geom_errorbar(aes(ymin = mean_observed - se_observed, ymax = mean_observed + se_observed), width=0.2) + 
       theme_q2r() +
       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
       theme(legend.title = element_blank()) +
       labs(y="observed features  ± s.e.", x = "") 
     ggsave("output/observed_se.png", observed_se, height = 2.5, width = 3)
     
     # Since days.since.experiment.start is a continuous variable, we run a 
     # general linear model. We will again use evenness as our roughly normal 
     # metric. The default of `glm` and `lm` is the normal distribution so we 
     # don't have to specify anything.
     
     # Does days.since.experiment.start impact evenness of the microbiota?
     
     glm.evenness.weightloss <- glm(pielou_evenness ~ `percent.Weightchange`, data=metaproj)
     glm.Treatment.weightloss <- glm( ~ `Percent Weight Change` , data=metaproj)
     summary(glm.evenness.weightloss)
     
     plot(glm.evenness.weightloss)
     
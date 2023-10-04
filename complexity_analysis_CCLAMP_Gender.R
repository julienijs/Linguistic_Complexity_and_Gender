setwd("./Datasets")

library(readxl)
library(ggplot2)
library(matrixStats)
library(effects)

#### Morphology ####

# read data
morph_data <- read_xlsx("zipped_morphology_CCLAMP.xlsx", col_names = TRUE)
zipped <- read_xlsx("CCLAMP_Zipped_Sizes.xlsx", col_names = TRUE)

# merge data
morph_total <- merge(zipped, morph_data, by="...1")

# make all numbers negative
morph_total[,7:106] <- morph_total[,7:106]*(-1)
# divide by full zipped size to get complexity ratio
morph_total[,7:106] <- morph_total[,7:106]/morph_total[,5]
# get means for each row = get mean complexity ratio
morph_means <- rowMeans(morph_total[,7:106])
# add mean complexity ratios to data frame
morph_total$morph_means <- morph_means
# get standard deviations
morph_std = rowSds(as.matrix(morph_total[,7:106]))


#### Syntax ####

# read data
synt_data <- read_xlsx("zipped_syntax_CCLAMP.xlsx", col_names = TRUE)
# merge data
synt_total <- merge(zipped, synt_data, by="...1")

# divide by full zipped size to get complexity ratio
synt_total[,7:106] <- synt_total[,7:106]/synt_total[,5]
# get means for each row = get mean complexity ratio
synt_means <- rowMeans(synt_total[,7:106])
# add mean complexity ratios to data frame
synt_total$synt_means <- synt_means
# get standard deviations
synt_std = rowSds(as.matrix(synt_total[,7:106]))


#### Morphology vs syntax ####

morph_and_synt <- data.frame("Morphology" = morph_total$morph_means,
                             "filename" = synt_total$filename,
                             "Syntax" = synt_total$synt_means,
                             "Year" = synt_total$year)
#### Gender ####

# read metadata file
metadata <- read.delim("C-CLAMP_metadata_gender.txt", header = FALSE, sep = "\t", fill = FALSE)

names(metadata)[names(metadata) == 'V1'] <- 'filename'
names(metadata)[names(metadata) == 'V4'] <- 'Author'
names(metadata)[names(metadata) == 'V10'] <- 'Gender'
names(morph_and_synt)[names(morph_and_synt) == 'synt_total.filename'] <- 'filename'

# merge data

meta_morph_and_synt <- merge(metadata, morph_and_synt, by="filename")

meta_morph_and_synt<-subset(meta_morph_and_synt, Gender!="mixed" & Gender!="NA")
meta_morph_and_synt$Gender <- as.factor(meta_morph_and_synt$Gender)

meta_morph_and_synt$Decade <- meta_morph_and_synt$Year - meta_morph_and_synt$Year %% 10 # calculate decades

# distribution of gender in dataset

gender_table <- table(meta_morph_and_synt$Gender)
print(gender_table)

# distribution of authors in dataset

author_table <- table(meta_morph_and_synt$Author)
print(author_table)

# model: morphology

morph_gender_model <- lm(Morphology ~ Gender*Decade, data=meta_morph_and_synt)
morph_gender_model <- lm(Morphology ~ Gender, data=meta_morph_and_synt)
summary(morph_gender_model)
plot(allEffects(morph_gender_model))

# model: syntax

synt_gender_model <- lm(Syntax ~ Gender*Decade, data=meta_morph_and_synt)
synt_gender_model <- lm(Syntax ~ Gender, data=meta_morph_and_synt)
summary(synt_gender_model)
plot(allEffects(synt_gender_model))

# model: manova
# check whether there is an association of the aggregate of the linguistic measures with gender

summary(manova.model.overall <- manova(cbind(Morphology, Syntax) ~ Gender, data=meta_morph_and_synt))


## Script to extract OMSS data from https://github.com/cstevenson-uva/omss-sciencelive/tree/main 


library(data.table)
library(dplyr)
library(ggplot2)

## Load in ## includes info about which subjects should be removed
dataLLM <- data.table::fread("/Users/maaikeschumer/Downloads/omss_response_vlm_dec2025.csv")


dataHumans <- data.table::fread("/Users/maaikeschumer/Downloads/omss_response.csv")

#Description of variables in dataset:
# omss_response_id = row id in omss_response table
# participant_fk = user_id = participant id in database
# itemid = item id from the 130 items from CODEC pilot dataset sent by Rogier on 25 july 2026
# response = option that participant choose
# rt = reaction time of participant for this item
# correct = score of whether item response is correct or not
# saveitem_timestamp = timestamp of when item response was saved
# created_timestamp = timestamp of when user id was created (you can drop this)
# consent = participant('s guardian) gave consent
# research_study = which event the data was collected
# age = age of participant
# language = language of instruction for participant (but this is not failsafe, so only for short descriptive purposes)



# How many subjects are in dataHumans?
str(dataHumans)
length(unique(dataHumans$participant_fk))

length(unique(dataHumans$user_id))

# Age distribution
hist(dataHumans$age)


# Create Nemo only subset
unique(dataHumans$research_study) [1] "NEMO2025"               "ABCday2025"             "UvARECcampusNovDec2025"

dataHumans_NEMOlive <- dataHumans %>%
  dplyr::filter(research_study == "NEMO2025")

length(unique(dataHumans_NEMOlive$participant_fk)) # 329

length(unique(dataHumans_NEMOlive$user_id)) # 329

# Age distribution
hist(dataHumans_NEMOlive$age)


# How many kids only? < 12
dataHumans_NEMOlive_sub13 <- dataHumans_NEMOlive %>%
  dplyr::filter(age <= 12)

length(unique(dataHumans_NEMOlive_sub13$participant_fk)) # 121

# How many unique item ids?
unique(dataHumans_NEMOlive_sub13$itemid)

unique(dataHumans$correct)

# How many correct?
pcor_sub13 <- mean(dataHumans_NEMOlive_sub13$correct) * 100

## dataLLM
str(dataLLM)

# create column accuracy in LLM data
dataLLM <- dataLLM %>%
  mutate(correct = as.integer(Output == CorrectAnswer))

str(dataLLM)

unique(dataLLM$`Model Name`)

sum(dataLLM$correct)        # total correct
pcor_LLM <- mean(dataLLM$correct) * 100      # proportion correct
mean(df$correct) * 100 # percentage correct

# Plot two scores

data.frame(Group = c("Children (age <= 12)", "LLM"),
           Accuracy = c(pcor_sub13, pcor_LLM)) %>%
  ggplot(aes(x = Group, y = Accuracy, fill = Group)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = c("#b9d4ff", "#80dbd0")) +
  ylim(0, 100) +
  labs(title = "121 children vs. 30 LLM's", y = "Percentage correct") +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank())



## adding max score across 
dataHumans_NEMOlive_sub13 %>%
  group_by(participant_) %>%

individual_acc <- aggregate(correct ~ participant_fk, data = dataHumans_NEMOlive_sub13, FUN = mean)

individual_acc[which.max(individual_acc$correct), ]


individual_acc_LLM <- aggregate(correct ~ `Model Name`, data = dataLLM, FUN = mean)

individual_acc[which.max(individual_acc$correct), ]
  
# Add group label to each dataframe first
individual_acc$Group <- "Children"
individual_acc_LLM$Group <- "LLM"

names(individual_acc_LLM) <- names(individual_acc)

individual_acc$pcor <- individual_acc$correct*100
individual_acc_LLM$pcor <- individual_acc_LLM$correct*100

# Combine
df_combined <- rbind(individual_acc, individual_acc_LLM)

# Plot
df_combined %>%
  ggplot(aes(x = Group, y = pcor, fill = Group)) +
  geom_bar(stat = "summary", fun = "mean", width = 0.5) +
  geom_dots(position = position_jitter(size = 1, width = 0.1, seed = 42)) +
  scale_fill_manual(values = c("#b9d4ff", "#80dbd0")) +
  ylim(0, 100) +
  labs(y = "Percentage correct") +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank())


ggplot(df_combined, aes(x = Group, y = pcor, fill = Group)) +
  geom_rain(rain.side = 'r') +
  scale_fill_manual(values = c("#b9d4ff", "#80dbd0")) +
  ylim(0, 100) +
  labs(title = "121 children vs. 30 LLM's", y = "Percentage correct") +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank())

#save plot
ggsave(
  filename = file.path("/Users/maaikeschumer/Desktop", paste0("omss_nemo", ".png")),
  width = 6,
  height = 6,
  dpi = 300
)


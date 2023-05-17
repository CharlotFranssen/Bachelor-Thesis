# figures
library(skimr)
library(dplyr)
library(readr)
library(ggplot2)

setwd("C:/tilburg university/2022-2023/Thesis/")

dataset_rcode <- read_csv("C:/tilburg university/2022-2023/Thesis/df_survival_RCode.csv")


#############

percentage_breakup <- nrow(dataset_rcode[dataset_rcode$Status == 1,])/nrow(dataset_rcode)
percentage_nobreakup <- nrow(dataset_rcode[dataset_rcode$Status == 0,])/nrow(dataset_rcode)

dataset_piechart = data.frame("perc" = c(percentage_breakup, percentage_nobreakup))
dataset_piechart$labels <- c(paste0(round(percentage_breakup*100), "%"),
                             paste0(round(percentage_nobreakup*100), "%"))
dataset_piechart$breakup = c("Yes", "No")


#View(dataset_piechart)
# Create pie chart with percentages inside bars
piechart <- ggplot(dataset_piechart, aes(x = "", y = perc, fill = breakup)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = c("grey", "black"),
                    name = "Relationship Breakup Status",
                    labels = c("No Breakup", "Breakup")) +
  coord_polar(theta = "y") +
  ggtitle("Relationship Breakup %") +
  theme_void() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5),
            size = 4,
            color = "white")

# Display plot
piechart


# _____________________________

df_survival <- dataset_rcode

# Change grouping variable to be a factor
df_survival$StatusCat = as.factor(df_survival$Status)

# Rename the values in Status to be more informative
levels(df_survival$StatusCat) <- c("No Breakup", "Breakup")

df_survival$YearCat = factor(df_survival$Year, levels = as.character(2010:2017))

# Stacked bar plot
combined_figure <- ggplot(df_survival, aes(x = YearCat, fill = StatusCat)) +
  geom_bar() +
  labs(title = "Frequency distribution of relationship breakups over time",
       x = "Year",
       y = "Frequency") +
  scale_fill_grey(name = "Status", start = 0.8, end = 0.2) +
  scale_x_discrete(expand = c(0, 0), limits = as.character(2010:2017))

combined_figure


ggsave("combined_figure.png", combined_figure, width = 6, height = 4, dpi = 300)
ggsave("piechart.png", piechart, width = 6, height = 4, dpi = 300)
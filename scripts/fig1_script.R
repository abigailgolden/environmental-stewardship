# Create figure 1

# Setup -------------------------------------------------------------------


rm(list = ls())
options(scipen = 999)
library(tidyverse)
library(here)
library(insight)
library(gridExtra)
library(lubridate)
library(patchwork)

datadir <- here::here("data")
figdir <- here::here("figures")

raw <- read.csv(paste(datadir, "policy-delphi-data.csv", sep = "/"), 
                header = TRUE, as.is = TRUE, na.strings = "")

# lower column names
colnames(raw) <- tolower(colnames(raw))

# save question text separately
q_text <- unname(unlist(raw[1,]))

# remove extra header rows

dat1 <- tail(raw, -2)

# extract multiple choice questions ---------------------------------------

mc_q_text_key <- data.frame(question = q_text[c(19, 21, 23, 25, 27, 28, 30, 32)],
                            question_id = c("q3", "q7", "q10", "q13", "q16", "q17", "q20", "q22"))

mc_dat <- dat1 |> 
  select(recipientfirstname, recipientlastname, q3, q7, q10, q13, q16, q17, q20, q22) |> 
  pivot_longer(cols = 3:10, values_to = "response", names_to = "question_id") |> 
  left_join(mc_q_text_key, by = "question_id") |> 
  mutate(response = ifelse(response == "I don't have enough information to answer this question",
                           "I don't have enough information\nto answer this question",
                           ifelse(response == "Anyone who takes an environmentally conscious action is acting as a steward, regardless of the outcome of that action",
                                  "Taking action",
                                  ifelse(response == "Only someone whose actions have positive environmental outcomes can be considered to be an environmental steward",
                                         "Having positive outcomes",
                                         ifelse(response == "Both", "Both organisms and ecosystems", response)))))

# group data by response options to ensure that non-chosen responses still show up in plots

allq_dat <- mc_dat |> 
  group_by(question_id) |> 
  count(response)


# plot as horizontal stacked bar plots --------------------------------

allq_dat_factor <- allq_dat |> 
  mutate(question_id = factor(question_id, 
                              levels = c("q22", "q20",
                                         "q13", "q17",
                                         "q16", "q10",
                                         "q7", "q3")),
         response = factor(response, 
                           levels = c("Yes", "No",
                                      "Taking action",
                                      "Having positive outcomes",
                                      "Organism/taxon level",
                                      "Ecosystem level",
                                      "Both organisms and ecosystems",
                                      "Individuals",
                                      "Individuals and groups",
                                      "I don't have enough information\nto answer this question")))

x_axis_labs <- c("Can groups or organizations\nact as stewards,\nor is it strictly an individual trait? (3.7)",
                 "Can one be a steward\nof an organism/taxon,\nan ecosystem, or both? (3.6)",
                 "Does environmental stewardship\nconsist of taking action or of\ngenerating specific outcomes? (3.4)",
                 "Does identifying as an environmental steward\nautomatically make a person a steward? (3.5)",
                 "Can someone be an environmental steward\nwithout identifying as one? (3.5)",
                 "Can someone be a steward\nwithout having the agency to act\nwithin a particular ecosystem? (3.3)",
                 "Can someone be a steward\nif they are not motivated by care\nfor the environment? (3.2)",
                 "Can someone be a steward\nby refraining from interacting\nwith a target resource or ecosystem? (3.1)")

# color palette from here: https://r-charts.com/color-palettes/#discrete
# paletteer::paletteer_d("ggthemes::Hue_Circle")

cols <- c("#4F7CBAFF", "#30BCADFF",  "#57A337", "#98DF8A", "#F9D23C", "#F8B620", "#F89217",
          "#F7B6D2", "#CE69BE", "#A26DC2FF")


figname <- "Figure 1.png"
png(paste(figdir, figname, sep = "/"), width = 15, height = 8, units = "in", res = 1000)


ggplot(allq_dat_factor, aes(x = question_id, y = n, fill = response))+
  geom_bar(position = "stack", stat = "identity",
           show.legend = TRUE)+
  coord_flip()+
  labs(x = NULL,
       y = "Number of responses")+
  scale_x_discrete(labels = x_axis_labs)+
  scale_y_continuous(breaks = c(1:11))+
  scale_fill_manual(name = "Response",
                    values = cols, drop = FALSE
  )+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.key.spacing.y = unit(0.25, "cm"))

dev.off()
graphics.off()



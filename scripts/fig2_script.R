# visualize responses to controversial questions by individual
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
  select(responseid, q3, q7, q10, q13) |> 
  pivot_longer(cols = 2:5, values_to = "response", names_to = "question_id") |> 
  left_join(mc_q_text_key, by = "question_id") |> 
  mutate(response = ifelse(response == "I don't have enough information to answer this question",
                           "I don't have enough information\nto answer this question",
                           ifelse(response == "Anyone who takes an environmentally conscious action is acting as a steward, regardless of the outcome of that action",
                                  "Taking action",
                                  ifelse(response == "Only someone whose actions have positive environmental outcomes can be considered to be an environmental steward",
                                         "Having positive outcomes",
                                         ifelse(response == "Both", "Both organisms and ecosystems", response))))) |> 
  mutate(response = factor(response, 
                           levels = c("Yes", "No",
                                      "Taking action",
                                      "Having positive outcomes",
                                      "I don't have enough information\nto answer this question")),
         question_id = factor(question_id,
                              levels = c("q3", "q7", "q10", "q13")))

question_labs <- c("Non-interaction\nas stewardship (3.1)",
                   "Stewardship\nwithout care (3.2)",
                   "Stewardship\nwithout agency (3.3)",
                   "Action vs outcome (3.4)")

figname <- "Figure 2.png"
png(paste(figdir, figname, sep = "/"), width = 12, height = 7, units = "in", res = 500)


ggplot(mc_dat, aes(x = question_id, y = responseid, fill = response))+
  geom_tile()+
  scale_x_discrete(labels = question_labs)+
  scale_y_discrete(labels = rev(LETTERS[1:11]))+
  labs(x = "Question Theme", 
       y = "Respondent")+
  scale_fill_manual(name = "Response",
                    values = c("#4F7CBAFF", "#30BCADFF","#57A337", "#98DF8A","#A26DC2FF"), drop = FALSE
  )+
  theme_classic()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 17))

dev.off()
graphics.off()


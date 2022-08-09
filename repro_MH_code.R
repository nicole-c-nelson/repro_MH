library(tidyverse)
library(stringr)
library(moderndive)
library(skimr)
library(infer)
library(PMCMRplus)
library(viridis)
library(ggplot2)
library(networkD3)

## read data files---------------------------------------------------------------
survey <- read.csv("data_files/survey_data.csv")
interview <- read.csv("data_files/interview_data.csv")

# create new variables for probable depression and anxiety
survey <- survey %>%
  mutate(prob_depress = case_when(
    PHQ_8 >= 10 ~ "Yes",
    TRUE ~ "No")) %>%
  mutate(prob_anx = case_when(
    GAD_7 >= 10 ~ "Yes",
    TRUE ~ "No"))

interview <- interview %>%
  mutate(prob_depress = case_when(
    PHQ_8 >= 10 ~ "Yes",
    TRUE ~ "No")) %>%
  mutate(prob_anx = case_when(
    GAD_7 >= 10 ~ "Yes",
    TRUE ~ "No"))

## HMS data ---------------------------------------------------------------------
# HMS_STEMM_grads_2019_2020 <- HMS_2019_2020 %>%
#   filter(degree_phd==1,
#          field_nat|field_med|field_nur|field_pharm|field_ph|field_eng==1) %>%
#   select(gender,starts_with("GAD7"), starts_with("phq9"),
#          race_black, race_ainaan, race_asian, race_his_temp, race_pi,
#          race_mides, race_white, race_other, finpast) %>%
#   select(-"phq9_9",-"gad7_impa") %>%
#   mutate(across(starts_with(c("phq", "GAD")),
#                 ~recode(.,`1`=0,`2`=1,`3`=2,`4`=3))) %>%
#   mutate(PHQ_8 = rowSums(across(starts_with("phq")))) %>%
#   mutate(GAD_7 = rowSums(across(starts_with("GAD")))) %>%
#   mutate(gender = recode(gender,
#                          `1` = "Man",
#                          `2`="Woman",
#                          `3`="Trans Man",
#                          `4`="Trans Woman",
#                          `5`="Nonbinary/Genderqueer/other",
#                          `6`="Nonbinary/Genderqueer/other"))%>%
#   mutate(URG = case_when(
#     race_ainaan==1 ~ "Yes",
#     race_his_temp==1 ~ "Yes",
#     race_pi==1 ~ "Yes",
#     race_black==1 ~ "Yes",
#     race_other==1 ~ "No",
#     race_mides==1 ~ "No",
#     race_white==1 ~ "No",
#     race_asian==1 ~ "No")) %>%
#   mutate(prob_depress = case_when(
#     PHQ_8 >= 10 ~ "Yes",
#     TRUE ~ "No")) %>%
#   mutate(prob_anx = case_when(
#     GAD_7 >= 10 ~ "Yes",
#     TRUE ~ "No"))

## Table 1 ----------------------------------------------------------------------
# create data frame for Table 1
survey_join <- survey %>%
  select(gender, URG, GAD_7, PHQ_8, GLS, disad_back, prob_anx, prob_depress) %>%
  mutate(source = "survey")
interview_join <- interview %>%
  filter(Experience == 1) %>%
  select(gender, URG, GAD_7, PHQ_8, GLS, disad_back, prob_anx, prob_depress) %>%
  mutate(source = "interview")
# HMS_join <- HMS_STEMM_grads_2019_2020 %>%
#   select(gender, URG, GAD_7, PHQ_8, prob_anx, prob_depress) %>%
#   mutate(source = "HMS")

table_1 <- full_join(survey_join, interview_join)
# table_1 <- full_join(table_1, HMS_join)

# generate numbers for Table 1
table_1 %>%
  group_by(gender, source) %>%
  count()
table_1 %>%
  group_by(URG, source) %>%
  count()
table_1 %>%
  group_by(disad_back, source) %>%
  count()
table_1 %>%
  select(PHQ_8, GAD_7, GLS, source) %>%
  group_by(source) %>%
  skim()
table_1 %>%
  group_by(prob_anx, prob_depress, source) %>%
  count()

# test for significant differences between survey and interview populations
chisq_test(x = filter(table_1, source != "HMS"), 
           formula = gender ~ source)
chisq_test(x = filter(table_1, source != "HMS"), 
           formula = URG ~ source)
chisq_test(x = filter(table_1, source != "HMS"), 
           formula = disad_back ~ source)
t_test(x = filter(table_1, source != "HMS"), 
       formula = PHQ_8 ~ source, 
       order = c("survey", "interview"),
       alternative = "two-sided")
t_test(x = filter(table_1, source != "HMS"), 
       formula = GAD_7 ~ source, 
       order = c("survey", "interview"),
       alternative = "two-sided")
t_test(x = filter(table_1, source != "HMS"), 
       formula = GLS ~ source, 
       order = c("survey", "interview"),
       alternative = "two-sided")
chisq_test(x = filter(table_1, source != "HMS"),
           formula = prob_anx ~ source)
chisq_test(x = filter(table_1, source != "HMS"),
           formula = prob_depress ~ source)

# test for significant differences between survey and HMS populations
# chisq_test(x = filter(table_1, source != "interview"), 
#            formula = gender ~ source)
# chisq_test(x = filter(table_1, source != "interview"), 
#            formula = URG ~ source)
# chisq_test(x = filter(table_1, source != "interview"), 
#            formula = disad_back ~ source)
# t_test(x = filter(table_1, source != "interview"), 
#        formula = PHQ_8 ~ source, 
#        order = c("survey", "HMS"),
#        alternative = "two-sided")
# t_test(x = filter(table_1, source != "interview"), 
#        formula = GAD_7 ~ source, 
#        order = c("survey", "HMS"),
#        alternative = "two-sided")
# t_test(x = filter(table_1, source != "interview"), 
#        formula = GLS ~ source, 
#        order = c("survey", "HMS"),
#        alternative = "two-sided")
# chisq_test(x = filter(table_1, source != "interview"),
#            formula = prob_anx ~ source)
# chisq_test(x = filter(table_1, source != "interview"),
#            formula = prob_depress ~ source)

## Figure 1 ---------------------------------------------------------------------
fig_1 <- survey %>%
  select(ID, starts_with("Q2")) %>%
  pivot_longer(!ID, names_to = "question", values_to = "response") %>%
  group_by(question, response) %>%
  count() %>%
  mutate(question = recode(question, 
                           Q2.2 ="Gave you results that were not consistent with what you expected",
                           Q2.3 = "Had a 'right answer' and you did not get that answer",
                           Q2.4 = "You yourself had done before, but got results that 
                  different from your previous attempts",
                           Q2.5 = "Someone else in your lab had done before, but you got results 
                  that differed from theirs",
                           Q2.6 = "Was similar to an experiment you'd seen in the published 
                  literature, but the results you got differed from what was reported 
                  in the publication", 
                           Q2.7 = "Had a control group or comparison group that was supposed to 
                  produce an expected result, but you got a different result")) 

ggplot(fig_1, aes(fill=response, x=question, y=n))+
  geom_bar(position="fill", stat="identity")+
  coord_flip()+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = "bottom", 
        plot.title.position = "plot")+
  scale_fill_viridis(discrete = T, direction = -1, 
                     breaks=c("Several times", "One time", "Not sure", "Never experienced"))+
  labs(x=NULL, y=NULL, fill = NULL,
       title = "Have you experienced a situation where you were doing an experiment that:")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 37))+
  scale_y_continuous(labels = scales::percent_format())+
  geom_hline(yintercept = c(.25,.50,.75), color="lightgrey")

## Interview characteristics ----------------------------------------------------
interview %>%
  group_by(Type.of.irreproducibility) %>%
  count()
interview %>%
  group_by(Time.period) %>%
  count()
interview %>%
  group_by(Eventual.outcome) %>%
  count()
interview %>%
  group_by(Overall.impact..0.negative.impact.) %>%
  count()

## Figure 2 --------------------------------------------------------------------
fig_2_nodes <- interview %>%
  select(ID, Initial.attribution..if.mentioned., Most.prominent.attribution,
         Was.the.data.published.) %>%
  rename(A = Initial.attribution..if.mentioned.,
         B = Most.prominent.attribution,
         C = Was.the.data.published.) %>%
  filter(A != "Not mentioned") %>%
  pivot_longer(!ID,
               names_to="time", 
               values_to="attribution") %>%
  group_by(time, attribution) %>%
  count() %>%
  ungroup() %>%
  select(attribution) %>%
  mutate(attribution = recode(attribution,
                              `Difference between original and new experiment` =
                                "Difference between experiments",
                              `Original result/hypothesis is wrong` = 
                                "Original result wrong",
                              `Tricky/complex phenomena being studied` = 
                                "Complex phenomenon",
                              `N` = "Not published",
                              `Y` = "Published"))

fig_2_links_1 <- interview %>%
  rename(source = Initial.attribution..if.mentioned.,
         target = Most.prominent.attribution,) %>%
  filter(source != "Not mentioned") %>%
  group_by(source, target) %>%
  count() %>%
  mutate(source = recode(source,
                         "Bad reagents/equipment" = 0,
                         "Difference between original and new experiment" = 1, 
                         "Own fault" = 2)) %>%
  mutate(target = recode(target,
                         "Bad reagents/equipment" = 3,
                         "Difference between original and new experiment" = 4, 
                         "Original result/hypothesis is wrong" = 5,
                         "Problem with protocol" = 6,
                         "Tricky/complex phenomena being studied" = 7))

fig_2_links_2 <- interview %>%
  filter(Initial.attribution..if.mentioned. != "Not mentioned") %>%
  rename(source = Most.prominent.attribution,
         target = Was.the.data.published.) %>%
  group_by(source, target) %>%
  count() %>%
  mutate(source = recode(source,
                         "Bad reagents/equipment" = 3,
                         "Difference between original and new experiment" = 4, 
                         "Original result/hypothesis is wrong" = 5,
                         "Problem with protocol" = 6,
                         "Tricky/complex phenomena being studied" = 7)) %>%
  mutate(target = recode(target,
                         "Y" = 8, "N" = 9))

fig_2_links <- full_join(fig_2_links_1, fig_2_links_2)

color_fig_2 = 'd3.scaleOrdinal() .range(["#440154FF", "#46337EFF", "#365C8DFF",
"#277F8EFF", "#1FA187FF", "#4AC16DFF", "#9FDA3AFF", "#FDE725FF"])'

sankeyNetwork(Links = fig_2_links, Nodes = fig_2_nodes,
              Source = "source", Target = "target",
              Value = "n", NodeID = "attribution",
              fontSize= 10, nodeWidth = 30,
              sinksRight = FALSE, fontFamily = "sans-serif",
              nodePadding = 0,
              colourScale = color_fig_2)

## Figure 3 --------------------------------------------------------------------
fig_3 <- interview %>%
  select(ID, Most.prominent.emotional.response, 
         Did.response.interfere.with.daily.activities.) %>%
  mutate(interfering = recode(Did.response.interfere.with.daily.activities.,
                              "Y" = "Yes",
                              "N" = "No")) %>%
  mutate(emotion_response = recode(Most.prominent.emotional.response,
                                   "Panic/anxiety" = "Worry/anxiety/panic",
                                   "Worry about wasting time/resources" =  "Worry/anxiety/panic",
                                   "Self doubt/loss of confindence" = "Self-doubt/loss of confidence"))%>%
  group_by(emotion_response, interfering) %>%
  count() 

emotion_order <- c("Worry/anxiety/panic", "Depressed/demotivated/tired",
                   "Indifference/lack of surprise", "Annoyed/frustrated",
                   "Self-doubt/loss of confidence")

ggplot(fig_3)+
  geom_bar(aes(x=emotion_response, y=n, fill=interfering),
           stat = "identity")+
  scale_x_discrete(limits = emotion_order)+
  coord_flip()+
  scale_y_continuous(breaks = c(0, 5, 10, 15))+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = "bottom")+
  geom_hline(yintercept = c(5,10), color="lightgrey")+
  labs(x=NULL, y= "Number of students", 
       fill = "Response interfered with daily activities   ")+
  scale_fill_viridis(discrete = T, direction = -1, 
                     breaks=c("Yes", "No"))

## Figure 4 --------------------------------------------------------------------
fig_4 <- interview %>%
  mutate(impact = recode(Overall.impact..0.negative.impact.,
                         "0" = "Negative",
                         "1" = "Neutral",
                         "2" = "Positive")) %>%
  mutate(outcome = recode(Eventual.outcome,
                          "Solved problem" = "Identified problem")) %>%
  group_by(outcome, impact) %>%
  count()

ggplot(fig_4, aes(fill=impact, x=outcome, y=n))+
  geom_bar(position="fill", stat="identity")+
  coord_flip()+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.position = "bottom", 
        plot.title.position = "plot")+
  scale_fill_viridis(discrete = T, 
                     breaks = c("Positive", "Neutral", "Negative"))+
  labs(x=NULL, y=NULL, fill = "Overall impact  ")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 37),
                   limits = c("Abandon research question", "Shift research question",
                              "Identified problem"))+
  scale_y_continuous(labels = scales::percent_format())+
  geom_hline(yintercept = c(.25,.50,.75), color="lightgrey")

# test for significant differences in impact depending on outcome
# using Kruskal-Wallis test since outcome is an ordinal variable with more than two levels
# follow Kruskal-Wallis with Dunn's test to see which pairs have significant differences
outcomes <- interview %>%
  mutate(impact = Overall.impact..0.negative.impact.) %>%
  mutate(outcome = factor(interview$Eventual.outcome,
                          levels = c("Abandon research question",
                                     "Shift research question",
                                     "Solved problem"))) %>%
  select(impact, outcome)

kruskalTest(impact ~ outcome, data = outcomes, dist = "Chisquare")
kwAllPairsDunnTest(impact ~ outcome, 
                   data = outcomes, method = "bh")


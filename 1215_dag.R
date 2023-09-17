
#####################################################################################################
#### Directed Acyclic graphs_DAG ####
## Acknowledgement to  https://cran.r-project.org/web/packages/ggdag/vignettes/intro-to-dags.html ##

####################################################################################################
library(ggdag)
library(ggplot2)
theme_set(theme_dag())



#### two paths ####
DAG.12 <- dagify(
  Workload ~ MC,
  Workload ~ OSR,
  Workload ~ Age,
  Workload ~ Sex,
  Workload ~ RW,
  Workload ~ BMI,
  BMI ~ Age,
  Workload ~ PS,
  Workload ~ HZ,
  MC ~ Culture,
  OSR ~ MC,
  labels = c(
    "Workload" = "Workload",
    "MC" = "Monastic celibacy",
    "OSR" = "Operational Sex Ratio",
    "Culture" = "Ethnicity",
    "HZ" = "Household size",
    "PS" = "Physical state",
    "RW" = "Removal of wristband",
    "Age" = "Age group",
    "Sex" = "Sex",
    "BMI" = "BMI"
    ),
  latent = "Culture",
  exposure = "MC",
  outcome = "Workload"
  )


#### Identify Directed Paths ####
ggdag_paths(DAG.12, text = FALSE, use_labels = "label", shadow = TRUE)

ggdag_adjustment_set(DAG.12, text = FALSE, use_labels = "label", shadow = TRUE)
ggsave('Stylized DAG path12154.tiff',dpi = 800)


DAG.12 %>% 
  tidy_dagitty(layout = "circle") %>%
  ggdag_paths_fan(text = FALSE, use_labels = "label", shadow = TRUE, 
                directed=FALSE,  
                node_size = 9,   text_size = 3, stylized = TRUE)+theme(legend.position = c(0.13,0.85))


ggsave('Stylized DAG path12155.tiff',dpi = 300)


##### removing ethnicity ####
DAG.12 <- dagify(
  Workload ~ MC,
  Workload ~ OSR,
  Workload ~ Age,
  Workload ~ Sex,
  Workload ~ RW,
  Workload ~ BMI,
  BMI ~ Age,
  Workload ~ PS,
  OSR ~ MC,
  labels = c(
    "Workload" = "Workload",
    "MC" = "Monastic celibacy",
    "OSR" = "Operational Sex Ratio",
    "PS" = "Physical state",
    "RW" = "Removal of wristband",
    "Age" = "Age group",
    "Sex" = "Sex",
    "BMI" = "BMI"
  ),
  exposure = "MC",
  outcome = "Workload"
)


#### Identify Directed Paths ####
ggdag_paths(DAG.12, text = FALSE, use_labels = "label", shadow = TRUE)

ggdag_adjustment_set(DAG.12, text = FALSE, use_labels = "label", shadow = TRUE)


DAG.12 %>% 
  tidy_dagitty(layout = "circle") %>%
  ggdag_paths_fan(text = FALSE, use_labels = "label", shadow = TRUE, 
                  directed=FALSE,  
                  node_size = 9,   text_size = 3, stylized = TRUE)+theme(legend.position = c(0.13,0.85))


ggsave('Stylized DAG path12156.tiff',dpi = 300)

ggdag_dseparated(DAG.12,
                 controlling_for = c("OSR","Sex","Age","RW","PS"),
                 text = FALSE, use_labels = "label", collider_lines = FALSE
)

DAG.12 %>% 
  tidy_dagitty(layout = "nicely") %>%
  ggdag_paths_fan(text = FALSE, use_labels = "label", shadow = TRUE, 
                  directed=FALSE,  
                  node_size = 9,   text_size = 3, stylized = TRUE)+theme(legend.position = c(0.13,0.85))


ggsave('Stylized DAG path12157.tiff',dpi = 300)

DAG.12 %>% 
  tidy_dagitty(layout = "auto") %>%
  ggdag_paths_fan(text = FALSE, use_labels = "label", shadow = TRUE, 
                  directed=FALSE,  
                  node_size = 9,   text_size = 3, stylized = TRUE)+theme(legend.position = c(0.13,0.85))


ggsave('Stylized DAG path12158.tiff',dpi = 300)

ggdag_paths_fan(DAG.12,text = FALSE, use_labels = "label", shadow = TRUE, 
                directed=FALSE,  
                node_size = 9,   text_size = 3, stylized = TRUE)+theme(legend.position = c(0.13,0.85))


##### solution 1 ####
#####################

#### random bias by random intercepts, check if we need random effects in the causal model
w.dag <- dagify(Workload ~ MC,
                Workload ~ OSR,
                Workload ~ Age,
                Workload ~ Sex,
                Workload ~ RW,
                Workload ~ PS,
                OSR ~ MC,
                Age ~ ID,
                Sex ~ ID,
                RW ~ ID,
                PS ~ ID,
                Workload ~ ID,
                Workload ~ Ethnicity,
                OSR ~ Ethnicity,
                MC ~ Ethnicity,
                Workload ~ Ethnicity,
                labels = c(
                  "Workload" = "Workload",
                  "MC" = "Monastic celibacy",
                  "OSR" = "Operational Sex Ratio",
                  "PS" = "Physical state",
                  "RW" = "Removal of wristband",
                  "Age" = "Age group",
                  "Sex" = "Sex",
                  "Ethnicity" = "Between culture bias",
                  "ID" = "Between individuals bias"
                ),
                latent = "ID",
                exposure = "MC",
                outcome = "Workload"
)


w.dag %>% 
  tidy_dagitty(layout = "auto") %>%
  ggdag(text = FALSE, use_labels = "label")

#### Identify Minimally Sufficient Set ####
w.dag %>%
  tidy_dagitty(layout = "auto") %>% 
  ggdag_adjustment_set(text = FALSE, use_labels = "label", 
                       shadow = TRUE, stylized = TRUE, node_size = 10,   text_size = 2.8)

ggsave('MSS_12156.tiff',dpi = 300)

#### Identify Minimally Sufficient Set ####
w.dag %>%
  tidy_dagitty(layout = "nicely") %>% 
  ggdag_adjustment_set(text = FALSE, use_labels = "label", 
                       shadow = TRUE, stylized = TRUE, node_size = 10,   text_size = 2.8)
ggsave('MSS_12166.tiff',dpi = 300)




##### solution 3, if they are nested, all these random biases are correlated ######
#### IF ETHNICITY OPERATES/adjusts at village level, modulates all the village-level variables rather than individual, then it becomes a confounding factor, which requires to be controlled for.
## VID adjusts something except OSR, and ethnicity can adjust these random bias at village level.
#### If the GLMM is nested ####

w.dag <- dagify(Workload ~ MC,
                Workload ~ OSR,
                Workload ~ Age,
                Workload ~ Sex,
                Workload ~ RM,
                Workload ~ PS,
                Workload ~ BMI,
                Age ~ ID,
                Sex ~ ID,
                RM ~ ID,
                PS ~ ID,
                BMI ~ ID,
                #HID ~ OSR,# does OSR generate variances among households? what about MC? Will MC produce variances among households? No.
                #HID ~ MC, #NO
                OSR ~ VID,
                OSR ~ MC,
                MC ~ Ethnicity,
                PS ~ Ethnicity,
                RM ~ Ethnicity,
                Age ~ Ethnicity,
                Sex ~ Ethnicity,
                BMI ~ Ethnicity,
                Workload ~ ID,
                Workload ~ HID,
                Workload ~ VID,
                Workload ~ Ethnicity,
                ID ~ HID,
                ID ~ VID,
                HID ~ VID,
                ID ~ Ethnicity,
                HID ~ Ethnicity,
                VID ~ Ethnicity,
                labels = c(
                  "Workload" = "Workload",
                  "MC" = "Monastic celibacy",
                  "OSR" = "Operational Sex Ratio",
                  "PS" = "Physical state",
                  "RM" = "Removal of wristband",
                  "Age" = "Age group",
                  "BMI" = "BMI",
                  "Sex" = "Sex",
                  "VID"  = "Between-village bias",
                  "Ethnicity" = "Between-group bias",
                  "HID" = "Between-household bias",
                  "ID" = "Between-individual bias"
                ),
                exposure = "MC",
                outcome = "Workload"
)


#### Identify Minimally Sufficient Set ####
w.dag %>%
  tidy_dagitty(layout = "auto") %>% 
  ggdag_adjustment_set(text = FALSE, use_labels = "label", 
                       shadow = TRUE, stylized = TRUE, node_size = 10,   text_size = 2.8)+theme(legend.position = c(0.13,0.76))

ggsave('Solution3_MSS_12176.tiff',dpi = 300)


ggsave('Solution3_MSS_12177.tiff',dpi = 300)

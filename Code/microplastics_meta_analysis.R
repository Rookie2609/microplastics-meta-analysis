
#调用R包
library(openxlsx)
library(metafor)
library(ggplot2)
library(glmulti)
library(tidyverse)
library(tidyr)
library(dplyr)
rm(list = ls())
#读取数据----
d1<- read.xlsx('D:\\Research_data.xlsx',
               sheet = 'Sheet1') %>%
  dplyr::select(2,36,37) 
colnames(d1) <- c("Independent", "y", "v")
# Overall effect----
d2 <- d1[!is.na(d1$y), ]
m0 <- rma.mv(y,v, data=d2,
             random =~1|Independent,method="ML",level=95,digits=5)
summary(m0)
length(d2$Independent)

# Subgroup effect----

colnames(d1) <- c("Independent", colnames(d1)[2], "y", "v")

# MPs type effect----
d2 <- d1[!is.na(d1$Type1)& !is.na(d1$y), ]

res2 <- rma.mv(y,v,
               mods=~factor(Type1)-1,random=~1|Independent,
               data=d2,method="ML",level=95,digits=5)
summary(res2)

length(d2$Independent[d2$Type1 == "BIO"])
length(d2$Independent[d2$Type1 == "COV"])


# MPs dose effect----

d2 <- d1[!is.na(d1$Sizeμm1)& !is.na(d1$y), ]

res2 <- rma.mv(y,v,
               mods=~factor(Sizeμm1)-1,random=~1|Independent,
               data=d2,method="ML",level=95,digits=5)
summary(res2)

length(d2$Independent[d2$Sizeμm1 == "Big"])
length(d2$Independent[d2$Sizeμm1 == "Small"])

# Ecper_duration effect----
d2 <- d1[!is.na(d1$Duration1)& !is.na(d1$y), ]

res2 <- rma.mv(y,v,
               mods=~factor(Duration1)-1,random=~1|Independent,
               data=d2,method="ML",level=95,digits=5)
summary(res2)

length(d2$Independent[d2$Duration1 == "<30days"])
length(d2$Independent[d2$Duration1 == ">30days"])
length(d2$Independent[d2$Duration1 == ">60days"])


# Presence or absence of Plant----
d2 <- d1[!is.na(d1$Plant1)& !is.na(d1$y), ]

res2 <- rma.mv(y,v,
               mods=~factor(Plant1)-1,random=~1|Independent,
               data=d2,method="ML",level=95,digits=5)
summary(res2)

length(d2$Independent[d2$Plant1 == "No"])
length(d2$Independent[d2$Plant1 == "Yes"])


# exper_type effect----
d2 <- d1[!is.na(d1$exper_type)& !is.na(d1$y), ]

res2 <- rma.mv(y,v,
               mods=~factor(exper_type)-1,random=~1|Independent,
               data=d2,method="ML",level=95,digits=5)
summary(res2)

length(d2$Independent[d2$exper_type == "field"])
length(d2$Independent[d2$exper_type == "lab"])
length(d2$Independent[d2$exper_type == "pot"])

# MPs dose effect----
res2 <- rma.mv(y_fungidiv,v_fungidiv,
               mods=~factor(Concentration1)-1,random=~1|Independent,
               data=d2,method="ML",level=95,digits=5)
summary(res2)

length(d2$Independent[d2$Concentration1 == "High>1"])
length(d2$Independent[d2$Concentration1 == "Low<0.5"])
length(d2$Independent[d2$Concentration1 == "Midumn0.5-1"])


# Global distribution----
library(tidyverse)
library(eoffice)

volcano<- read.xlsx('D:\\Research_data.xlsx',
                                   sheet = 'Sheet1') %>%
                      dplyr::select(3,4)

world <- map_data("world")
world

volcano$Lon <- as.numeric(as.character(volcano$Lon))
volcano$Lat <- as.numeric(as.character(volcano$Lat))

library(ggsci)
p1<-ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size =0.1
  ) +
  geom_point(
    data = volcano,
    aes(Lon, Lat),
    shape = 21, size =5, fill='#D55C00',color = "black"
  ) +
  # coord_fixed(1.3) +
  xlim(lon_min, lon_max) + 
  ylim(lat_min, lat_max)+ 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
p1

# Paired analysis----
d1<- read.xlsx('D:\\Research_data.xlsx',
               sheet = 'Sheet1') %>% 
  select(22,23,38,39)


plot.dat <- d1 %>% 
  na.omit()


plot.dat <- plot.dat %>%
  mutate(
    weight_biomass = 1 / v_totalbiomass,
    weight_soc = 1 / v_SOC,
    weight = pmin(weight_biomass, weight_soc),
    y_totalbiomass1 = (exp(y_totalbiomass) - 1) * 100,
    y_SOC1 = (exp(y_SOC) - 1) * 100
  )


plot.dat <- plot.dat %>%
  mutate(
    Scenario = case_when(
      y_totalbiomass1 > 0 & y_SOC1 > 0 ~ "Win-win",
      y_totalbiomass1 > 0 & y_SOC1 <= 0 ~ "Biomass gain\nSOC loss",
      y_totalbiomass1<= 0 & y_SOC1 > 0 ~ "SOC gain\nBiomass loss",
      TRUE ~ "Lose-lose"
    )
  )

prob <- plot.dat %>%
  group_by(Scenario) %>%
  summarise(
    n = n(),
    weight_sum = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent = round(weight_sum / sum(weight_sum) * 100, 1)
  )

p <- setNames(prob$percent, prob$Scenario)


p1<-ggplot(plot.dat,aes(y_totalbiomass1, y_SOC1,size = weight)) +
  geom_point(shape = 21, fill = "#FF8C00",colour = "black",alpha = .75) +
  geom_vline(xintercept = 0,linetype = 2) +
  geom_hline(yintercept = 0,linetype = 2) +
  annotate("text",
           x = Inf,
           y = Inf,
           label = paste0("Win-win\n", p["Win-win"], "%"),
           hjust = 1.1,
           vjust = 1.2,
           size = 5) +
  annotate("text",
           x = Inf,
           y = -Inf,
           label = paste0("Trade-off\n", p["Biomass gain\nSOC loss"], "%"),
           hjust = 1.1,
           vjust = -0.2,
           size = 5) +
  annotate("text",
           x = -Inf,
           y = Inf,
           label = paste0("Trade-off\n", p["SOC gain\nBiomass loss"], "%"),
           hjust = -0.1,
           vjust = 1.2,
           size = 5) +
  annotate("text",
           x = -Inf,
           y = -Inf,
           label = paste0("Lose-lose\n", p["Lose-lose"], "%"),
           hjust = -0.1,
           vjust = -0.2,
           size = 5) +
  scale_size(range = c(2,10)) +
  ylab('Microplastic effect on SOC (%)')+
  xlab('Microplastic effect on plant biomass (%)')+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 16, colour = 'black'),
    axis.text.y = element_text(size = 16, colour = 'black'),
    axis.title.y = element_text(size = 18, colour = 'black'),
    axis.title.x = element_text(size = 18, colour = 'black'),
    strip.text = element_text(size = 14,face = "bold",color = 'black'),
    legend.position = 'none')
p1


# =========================================
#  Mixed-effects random forest analysis----
# =========================================
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(MixRF)
library(randomForest)
library(lme4)
library(car)
library(progress)



# 1. Mixed-effects random forest for SOC
data.1 <- read.xlsx('D:\\Research_data.xlsx',
                    sheet = 'Sheet1')

data_merf_SOC <- data.1 %>%
  select(
    Independent,
    Duration,
    Mpstype,
    Mpsdose,
    InitialpH,
    InitialsoilCN,
    MPssize,
    y_SOC
  ) %>%
  na.omit() %>%
  mutate(
    Independent = factor(Independent),
    Mpstype = factor(
      Mpstype,
      levels = c(0, 1),
      labels = c("COV", "BIO")
    )
  )

# 1.2 Summary of clustering structure
cluster_summary_SOC <- data_merf_SOC %>%
  count(Independent) %>%
  summarise(
    n_observations = sum(n),
    n_clusters = n(),
    min_cluster_size = min(n),
    median_cluster_size = median(n),
    mean_cluster_size = mean(n),
    max_cluster_size = max(n),
    singleton_clusters = sum(n == 1)
  )

cluster_summary_SOC

table(table(data_merf_SOC$Independent))

# 1.3 Predictor matrix
predictors_SOC <- data_merf_SOC %>%
  select(
    Duration,
    Mpstype,
    Mpsdose,
    InitialpH,
    InitialsoilCN,
    MPssize
  )

# 1.4 Fit mixed-effects random forest
set.seed(123)

merf_SOC <- MixRF(
  Y = data_merf_SOC$y_SOC,
  X = predictors_SOC,
  random = "(1 | Independent)",
  data = data_merf_SOC,
  initialRandomEffects = 0,
  ErrorTolerance = 0.001,
  MaxIterations = 500
)

merf_SOC$forest
summary(merf_SOC$MixedModel)

# 1.5 Extract variable importance
importance_SOC <- importance(merf_SOC$forest)

importance_SOC <- importance_SOC[
  order(
    importance_SOC[, "IncNodePurity"],
    decreasing = TRUE
  ),
  ,
  drop = FALSE
]

importance_SOC <- as.data.frame(importance_SOC) %>%
  rownames_to_column("Variable") %>%
  mutate(Response = "SOC")

importance_SOC

# 2. Mixed-effects random forest for plant biomass

# 2.1 Data preparation
data_merf_Biomass <- data.1 %>%
  select(
    Independent,
    Duration,
    Mpstype,
    Mpsdose,
    InitialpH,
    InitialsoilCN,
    MPssize,
    y_totalbiomass
  ) %>%
  na.omit() %>%
  mutate(
    Independent = factor(Independent),
    Mpstype = factor(
      Mpstype,
      levels = c(0, 1),
      labels = c("COV", "BIO")
    )
  )


# 2.2 Summary of clustering structure
cluster_summary_Biomass <- data_merf_Biomass %>%
  count(Independent) %>%
  summarise(
    n_observations = sum(n),
    n_clusters = n(),
    min_cluster_size = min(n),
    median_cluster_size = median(n),
    mean_cluster_size = mean(n),
    max_cluster_size = max(n),
    singleton_clusters = sum(n == 1)
  )

cluster_summary_Biomass

table(table(data_merf_Biomass$Independent))

# 2.3 Predictor matrix
predictors_Biomass <- data_merf_Biomass %>%
  select(
    Duration,
    Mpstype,
    Mpsdose,
    InitialpH,
    InitialsoilCN,
    MPssize
  )

# 2.4 Fit mixed-effects random forest
set.seed(123)

merf_Biomass <- MixRF(
  Y = data_merf_Biomass$y_totalbiomass,
  X = predictors_Biomass,
  random = "(1 | Independent)",
  data = data_merf_Biomass,
  initialRandomEffects = 0,
  ErrorTolerance = 0.001,
  MaxIterations = 500
)

merf_Biomass$forest
summary(merf_Biomass$MixedModel)

# 2.5 Extract variable importance
importance_Biomass <- importance(merf_Biomass$forest)

importance_Biomass <- importance_Biomass[
  order(
    importance_Biomass[, "IncNodePurity"],
    decreasing = TRUE
  ),
  ,
  drop = FALSE
]

importance_Biomass <- as.data.frame(importance_Biomass) %>%
  rownames_to_column("Variable") %>%
  mutate(Response = "Plant biomass")

importance_Biomass

# 3. Cluster-bootstrap function
boot_mixrf <- function(data) {
  
  # Identify independent experiments
  clusters <- unique(data$Independent)
  
  # Resample independent experiments with replacement
  sampled_clusters <- sample(
    clusters,
    size = length(clusters),
    replace = TRUE
  )
  
  # Extract all observations belonging to each sampled
  # experiment. Repeatedly sampled experiments are assigned
  # new bootstrap cluster IDs.
  boot_list <- lapply(
    seq_along(sampled_clusters),
    function(i) {
      
      temp <- data[
        data$Independent == sampled_clusters[i],
      ]
      
      temp$Independent_boot <- paste0(
        "boot_",
        i
      )
      
      temp
    }
  )
  
  boot_data <- bind_rows(boot_list)
  
  boot_data$Independent_boot <- factor(
    boot_data$Independent_boot
  )
  
  # Predictor matrix
  X_boot <- boot_data %>%
    select(
      Duration,
      Mpstype,
      Mpsdose,
      InitialpH,
      InitialsoilCN,
      MPssize
    )
  
  # Fit mixed-effects random forest
  model_boot <- MixRF(
    Y = boot_data$y,
    X = X_boot,
    random = "(1 | Independent_boot)",
    data = boot_data,
    initialRandomEffects = 0,
    ErrorTolerance = 0.001,
    MaxIterations = 100
  )
  
  # Extract IncNodePurity-based variable importance
  imp <- importance(model_boot$forest)
  
  imp[, "IncNodePurity"]
}


# ============================================================
# 4. Cluster bootstrap for SOC
# ============================================================

set.seed(123)

B <- 999

boot_results_SOC <- matrix(
  NA_real_,
  nrow = B,
  ncol = 6
)

colnames(boot_results_SOC) <- c(
  "Duration",
  "Mpstype",
  "Mpsdose",
  "InitialpH",
  "InitialsoilCN",
  "MPssize"
)

pb_SOC <- progress_bar$new(
  format =
    "SOC bootstrap [:bar] :percent | :current/:total | elapsed: :elapsed | ETA: :eta",
  total = B,
  clear = FALSE,
  width = 80,
  show_after = 0
)

n_success_SOC <- 0

for (i in seq_len(B)) {
  
  temp_imp <- try(
    suppressWarnings(
      suppressMessages(
        boot_mixrf(
          data_merf_SOC
        )
      )
    ),
    silent = TRUE
  )
  
  if (!inherits(
    temp_imp,
    "try-error"
  )) {
    
    boot_results_SOC[
      i,
      names(temp_imp)
    ] <- temp_imp
    
    n_success_SOC <-
      n_success_SOC + 1
  }
  
  pb_SOC$tick()
}

cat(
  "\nSuccessfully fitted:",
  n_success_SOC,
  "of",
  B,
  "SOC bootstrap models.\n"
)

# 4.1 Summarize bootstrap uncertainty for SOC
SOC_boot_summary <- data.frame(
  Variable = colnames(
    boot_results_SOC
  ),
  
  Mean_importance = apply(
    boot_results_SOC,
    2,
    mean,
    na.rm = TRUE
  ),
  
  Lower_95CI = apply(
    boot_results_SOC,
    2,
    quantile,
    probs = 0.025,
    na.rm = TRUE
  ),
  
  Upper_95CI = apply(
    boot_results_SOC,
    2,
    quantile,
    probs = 0.975,
    na.rm = TRUE
  )
) %>%
  arrange(
    desc(
      Mean_importance
    )
  )

SOC_boot_summary

# 5. Cluster bootstrap for plant biomass
set.seed(123)

boot_results_Biomass <- matrix(
  NA_real_,
  nrow = B,
  ncol = 6
)

colnames(
  boot_results_Biomass
) <- c(
  "Duration",
  "Mpstype",
  "Mpsdose",
  "InitialpH",
  "InitialsoilCN",
  "MPssize"
)

pb_Biomass <- progress_bar$new(
  format =
    "Biomass bootstrap [:bar] :percent | :current/:total | elapsed: :elapsed | ETA: :eta",
  total = B,
  clear = FALSE,
  width = 80,
  show_after = 0
)

n_success_Biomass <- 0

for (i in seq_len(B)) {
  
  temp_imp <- try(
    suppressWarnings(
      suppressMessages(
        boot_mixrf(
          data_merf_Biomass
        )
      )
    ),
    silent = TRUE
  )
  
  if (!inherits(
    temp_imp,
    "try-error"
  )) {
    
    boot_results_Biomass[
      i,
      names(temp_imp)
    ] <- temp_imp
    
    n_success_Biomass <-
      n_success_Biomass + 1
  }
  
  pb_Biomass$tick()
}

cat(
  "\nSuccessfully fitted:",
  n_success_Biomass,
  "of",
  B,
  "plant biomass bootstrap models.\n"
)

# 5.1 Summarize bootstrap uncertainty for plant biomass
Biomass_boot_summary <- data.frame(
  Variable = colnames(
    boot_results_Biomass
  ),
  
  Mean_importance = apply(
    boot_results_Biomass,
    2,
    mean,
    na.rm = TRUE
  ),
  
  Lower_95CI = apply(
    boot_results_Biomass,
    2,
    quantile,
    probs = 0.025,
    na.rm = TRUE
  ),
  
  Upper_95CI = apply(
    boot_results_Biomass,
    2,
    quantile,
    probs = 0.975,
    na.rm = TRUE
  )
) %>%
  arrange(
    desc(
      Mean_importance
    )
  )

Biomass_boot_summary

# 6. Collinearity assessment for SOC predictors
vif_model_SOC <- lm(
  y ~
    Duration +
    Mpstype +
    Mpsdose +
    InitialpH +
    InitialsoilCN +
    MPssize,
  data = data_merf_SOC
)

vif_result_SOC <- vif(
  vif_model_SOC
)

vif_table_SOC <- data.frame(
  Variable = names(
    vif_result_SOC
  ),
  VIF = as.numeric(
    vif_result_SOC
  )
)

vif_table_SOC

# 7. Collinearity assessment for plant biomass predictors
vif_model_Biomass <- lm(
  y ~
    Duration +
    Mpstype +
    Mpsdose +
    InitialpH +
    InitialsoilCN +
    MPssize,
  data = data_merf_Biomass)

vif_result_Biomass <- vif(
  vif_model_Biomass)

vif_table_Biomass <- data.frame(
  Variable = names(
    vif_result_Biomass),
  VIF = as.numeric(
    vif_result_Biomass))

vif_table_Biomass

# 8. Combine SOC and plant biomass variable importance
importance_final <- bind_rows(
  importance_SOC,
  importance_Biomass
) %>%
  mutate(
    Variable = recode(
      Variable,
      Mpstype = "MPs type",
      MPssize = "MPs size",
      Duration = "Exp. duration",
      Mpsdose = "MPs dose",
      InitialsoilCN =
        "Initial soil C:N ratio",
      InitialpH =
        "Initial soil pH"
    ),
    
    Variable = factor(
      Variable,
      levels = c(
        "MPs type",
        "MPs size",
        "Exp. duration",
        "MPs dose",
        "Initial soil C:N ratio",
        "Initial soil pH"
      )
    ),
    
    Response = factor(
      Response,
      levels = c(
        "SOC",
        "Plant biomass"
      )
    )
  )

# 9. Plot variable importance
p <- ggplot(
  importance_final,
  aes(
    x = Variable,
    y = IncNodePurity,
    fill = Response
  )
) +
  geom_col(
    position =
      position_dodge(
        width = 0.9
      ),
    width = 0.8
  ) +
  scale_fill_manual(
    values = c(
      "#FF8C00",
      "#4DAF4A"
    )
  ) +
  xlab(NULL) +
  ylab(
    "Variable importance (IncNodePurity)"
  ) +
  theme_bw() +
  theme(
    panel.grid.major =
      element_blank(),
    panel.grid.minor =
      element_blank(),
        axis.text.x =
      element_text(
        size = 18,
        colour = "black"
      ),
        axis.text.y =
      element_text(
        size = 18,
        colour = "black"
      ),
        axis.title.y =
      element_text(
        size = 18,
        colour = "black"
      ),
        axis.title.x =
      element_text(
        size = 18,
        colour = "black"
      ),
        strip.text =
      element_text(
        size = 18,
        colour = "black"
      ),
        legend.text =
      element_text(
        size = 15,
        colour = "black"
      ),
        legend.position = "none"
  )

p


# ============================================================
# Pairwise relationship analyses and segmented meta-regression
# ============================================================
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(performance)
library(metafor)


# ============================================================
# 1. General pairwise relationship analysis
# ============================================================

# Example input data structure:
# Independent : independent experiment identifier
# Type1       : microplastic type (BIO or COV)
# X           : predictor variable
# Y           : response variable

data_pairwise <- read.xlsx(
  "Research_data.xlsx",
  sheet = "Sheet1"
)

data_pairwise <- data_pairwise %>%
  select(
    Independent,
    Type1,
    X,
    Y
  ) %>%
  filter(
    !is.na(X),
    !is.na(Y)
  ) %>%
  mutate(
    Independent = factor(Independent),
    Type1 = factor(
      Type1,
      levels = c("BIO", "COV")
    )
  )

# 1.1 Linear mixed-effects models by microplastic type
models_linear <- lapply(
  split(data_pairwise, data_pairwise$Type1),
  function(df) {
    
    lmer(
      Y ~ X + (1 | Independent),
      data = df
    )
    
  }
)


# Model summaries
summary(models_linear$BIO)
summary(models_linear$COV)

# Marginal and conditional R2
r2(models_linear$BIO)
r2(models_linear$COV)

# Sample sizes
table(data_pairwise$Type1)

# ============================================================
# 2. Quadratic mixed-effects models
# ============================================================

# Use this section only for relationships for which a nonlinear
# response was hypothesized and evaluated.

data_pairwise <- data_pairwise %>%
  mutate(
    X_log = log(X)
  )


models_quadratic <- lapply(
  split(data_pairwise, data_pairwise$Type1),
  function(df) {
    
    lmer(
      Y ~ poly(X_log, 2) + (1 | Independent),
      data = df
    )
    
  }
)


summary(models_quadratic$BIO)
summary(models_quadratic$COV)

r2(models_quadratic$BIO)
r2(models_quadratic$COV)



# 3. Initial soil properties and MP dose in relation to SOC and plant biomass responses


# Expected structure:
# Independent : experiment identifier
# X           : original predictor
# Y           : original response
# X1          : predictor used in the statistical model
# Y1          : response used in the statistical model

data_initial <- read.xlsx(
  "Research_data.xlsx",
  sheet = "Sheet1"
) %>%
  filter(
    !is.na(X1),
    !is.na(Y1)
  ) %>%
  mutate(
    Independent = factor(Independent)
  )


# 3.1 Linear mixed-effects model

model_initial_linear <- lmer(
  Y1 ~ X1 + (1 | Independent),
  data = data_initial
)

summary(model_initial_linear)
r2(model_initial_linear)

# 3.2 Quadratic mixed-effects model
model_initial_quadratic <- lmer(
  Y1 ~ poly(X1, 2) + (1 | Independent),
  data = data_initial
)

summary(model_initial_quadratic)
r2(model_initial_quadratic)


# 4. Convert lnRR to percentage change for visualization

data_initial <- data_initial %>%
  mutate(
    Percent_change = (exp(Y1) - 1) * 100
  )



# 5. Mechanistic pairwise relationships

# Expected structure:
# Independent : experiment identifier
# Type1       : BIO or COV
# X           : predictor
# Y           : response
# Index1      : relationship identifier

data_mechanism <- read.xlsx(
  "Research_data.xlsx",
  sheet = "Sheet1"
) %>%
  filter(
    !is.na(X),
    !is.na(Y)
  ) %>%
  mutate(
    Independent = factor(Independent),
    Type1 = factor(
      Type1,
      levels = c("BIO", "COV")
    )
  )


# 5.1 Fit linear mixed-effects models separately for BIO and COV

mechanism_models <- lapply(
  split(data_mechanism, data_mechanism$Type1),
  function(df) {
    
    lmer(
      Y ~ X + (1 | Independent),
      data = df
    )
    
  }
)


summary(mechanism_models$BIO)
summary(mechanism_models$COV)

r2(mechanism_models$BIO)
r2(mechanism_models$COV)

table(data_mechanism$Type1)


# 6. Example visualization of BIO and COV relationships

# Note:
# The lines generated below are ordinary least-squares trend lines
# and are included only for visualization. Statistical inference
# is based on the mixed-effects models fitted above.

p_relationship <- ggplot(
  data_mechanism,
  aes(
    x = X,
    y = Y,
    fill = Type1
  )
) +
  geom_point(
    size = 4,
    alpha = 0.6,
    colour = "black",
    shape = 21
  ) +
  geom_smooth(
    aes(
      colour = Type1,
      fill = Type1
    ),
    method = "lm",
    formula = y ~ x,
    linewidth = 1,
    se = TRUE
  ) +
  scale_fill_manual(
    values = c(
      BIO = "#5DA8D9",
      COV = "#E07E4D"
    )
  ) +
  scale_colour_manual(
    values = c(
      BIO = "#5DA8D9",
      COV = "#E07E4D"
    )
  ) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(
      size = 18,
      colour = "black"
    ),
    axis.text.y = element_text(
      size = 18,
      colour = "black"
    ),
    axis.title.x = element_text(
      size = 18,
      colour = "black"
    ),
    axis.title.y = element_text(
      size = 18,
      colour = "black"
    ),
    strip.text = element_text(
      size = 18,
      colour = "black"
    )
  ) +
  facet_wrap(
    ~ Index1,
    scales = "free",
    ncol = 3
  )

p_relationship

# 7. Segmented SOC-total N meta-regression

breakpoint_SOC <- 0.185  #Conventional MPs
breakpoint_SOC <- -0.027  #Biodegradable MPs

# Select conventional microplastics
data_COV <- data_SOC_TN %>%
  filter(
    Mpstype == "COV",
    !is.na(y_SOC),
    !is.na(y_TN),
    !is.na(v_TN),
    !is.na(Mpsdose)
  ) %>%
  mutate(
    Independent = factor(Independent),
    
    # Log10-transformed MP dose
    logDose = log10(Mpsdose),
    
    # Piecewise terms below and above the empirical breakpoint
    SOC_before = pmin(
      y_SOC,
      breakpoint_SOC
    ),
    
    SOC_after = pmax(
      y_SOC - breakpoint_SOC,
      0
    )
  )

# 7.1 Segmented multilevel meta-regression
model_SOC_TN_segmented <- rma.mv(
  yi = y_TN,
  V = v_TN,
  
  mods =
    ~ SOC_before * logDose +
    SOC_after * logDose,
  
  random =
    ~ 1 | Independent,
  
  data = data_COV,
  
  method = "REML"
)

summary(model_SOC_TN_segmented)

# 8. Conditional slopes of the segmented relationship

coef_segmented <- coef(
  model_SOC_TN_segmented
)

coef_segmented


# Slope below the SOC breakpoint:
#
# beta_before =
#   beta_SOC_before +
#   beta_SOC_before:logDose * logDose
#
# Slope above the SOC breakpoint:
#
# beta_after =
#   beta_SOC_after +
#   beta_SOC_after:logDose * logDose

# 9. Functions for calculating dose-dependent slopes

slope_before <- function(dose) {
  
  log_dose <- log10(dose)
  
  coef_segmented["SOC_before"] +
    coef_segmented["SOC_before:logDose"] *
    log_dose
}


slope_after <- function(dose) {
  
  log_dose <- log10(dose)
  
  coef_segmented["SOC_after"] +
    coef_segmented["SOC_after:logDose"] *
    log_dose
}


# Example
slope_before(1)
slope_after(1)


# ============================================================
# 10. Model information
# ============================================================

cat(
  "Number of observations:",
  model_SOC_TN_segmented$k,
  "\n"
)

cat(
  "Number of independent experiments:",
  nlevels(data_COV$Independent),
  "\n"
)


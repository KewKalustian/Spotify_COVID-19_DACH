
remove(list = ls(all = T)); gc(T,T,T)

################
### Packages ###
################

if (!require(pacman))
  install.packages("pacman", repo = "http://cran.us.r-project.org")

pacman::p_load("tidyverse", "magrittr","car", "lubridate", "scales", "ggbeeswarm",
               "ggpubr", "caret", "e1071", "factoextra", 
               "rgl", "rstatix", "cluster", "parallel",
               "parallelMap", "vip","ModelMetrics","pdp")


# importing the data that has previously been saved in the working directory 
# (cf. code/script: "Analysis_Step2_kMeans+Difference Tests.")

df_ml_full <- read_csv("df_ml_full.csv") 

# Selecting model variables

mod_df <- df_ml_full  %>% 
  dplyr::select(Pandemic,
         country, 
         track_id, 
         stream_count_rescaled, 
         chart_position_rescaled , 
         acousticness,
         speechiness,
         instrumentalness,
         liveness,
         duration_ms_rescaled, 
         mood_clust_fct) %>%
  mutate(Pandemic = as.factor(Pandemic),
         country = as.factor(country))

str(mod_df)


#############
### MODEL ###
#############


set.seed(1, sample.kind = "Rounding")

# 80/20 split

test_index <- createDataPartition(y = mod_df$Pandemic, times = 1, p = .2, 
                                  list = F)

DACH_train <- mod_df[-test_index,]

temp <- mod_df[test_index,]

# Making sure that track_id and countries in the test set are also in train set 

DACH_test <- temp %>% 
  semi_join(DACH_train, by = "track_id") %>%
  semi_join(DACH_train, by = "country") 

# Adding rows that are removed from the test set back into train set
removed <- anti_join(temp, DACH_test)
DACH_train <- rbind(DACH_train, removed)


str(DACH_test)

str(DACH_train)

# ######### #
# 5-fold CV #
# ######### #

set.seed(3271, sample.kind = "Rounding")
# Only 20% of the data for cv due to computational costs.
train <- sample_frac(DACH_train, 0.2)

start_time <- Sys.time()

parallelStartSocket(cpus = detectCores())

set.seed(42, sample.kind = "Rounding")
hyperparam <-  tune(svm,Pandemic ~ ., 
                    data = train[-3], 
                    kernel = "radial", ranges = list(cost = 10^(-1:4), 
                                                     gamma = c(0.5,1:5)), 
                    tunecontrol = tune.control(cross = 5))
end_time <- Sys.time()

comput_time <-  end_time - start_time
print(comput_time)

summary(hyperparam)


# ############## #
# Model training #
# ############## #

start_time_2 <- Sys.time()
set.seed(1, sample.kind = "Rounding")

svm_fit <-  svm(Pandemic ~ ., DACH_train[,-3],
                kernel = "radial",      # see summary(hyperparam)
                cost = 10,            # see summary(hyperparam)
                gamma = 2,            # see summary(hyperparam)
                scale = T,
                probability = T)


# ############# #
# Test scenario #
# ############# #


prd <- predict(svm_fit, DACH_test[,-3], probability = T)

# ########### #
# Performance #
# ########### #

head(attr(prd, "probabilities"))


data$table <- caret::confusionMatrix(prd, DACH_test$Pandemic, mode = "prec_recall")

end_time_2 <- Sys.time()


# ################### #
# Variable Importance #
# ################### #

# Pred/classification function
prob_no_pan <- function(object, newdata) {
  res <-as.vector(predict(object, newdata = newdata))
  return(res)}


set.seed(2827)  # for reproducibility
imp <-vip(svm_fit, 
          train = as.data.frame(DACH_train[,-3]),
          method = "permute", 
          target = "Pandemic",
          metric = "auc",
          nsim = 5,
          pred_wrapper =  prob_no_pan,
          reference_class = "Pandemic",
          all_permutations = T)

# Extracting values
imp_values <- imp$data

# Plotting

# color palette

cols <- c("#3288BD", "#F46D43", "darkgreen" , "#5E4FA2")

# Global Plotting Layout
layout <- theme_bw(base_size = 14) +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = "top",
        legend.key = element_rect(color = "white"),
        legend.background = element_rect(fill = "white",
                                         linetype = "solid",
                                         color = "grey90"),
        plot.margin = unit(c(.66, .33, .66, .66), "cm"))

ggplot(data= imp_values, aes( x = reorder(Variable, Importance), y = Importance,
                              label = round(Importance,3) )) +
  geom_segment(aes(x=reorder(Variable, Importance), xend=reorder(Variable, Importance),
                   y=0, yend = Importance-0.007), color = "#2e4057", size =4, alpha=.75)+
  geom_point(size= 14, shape = 21, color = "#2e4057", fill = "#2e4057", alpha = 0.75) +
  geom_label(nudge_y = 0, nudge_x = 0, size = 3)+
  labs(y = "Importance after Permutation\n(Probabilities)\n",  x ="\nVariables")+
  layout+
  coord_flip()

# ################## #
# Partial Dependence #
# ################## #

# Pred/classification function
custom_pred <- function(object, newdata) {
  res <- as.vector(predict(object, data.frame(newdata), type = "prob"))[,2]
  return(res)}

# Extracting values
pd_values <- pdp::partial(svm_fit, train = as.data.frame(DACH_train[,-3]), 
                          pred.var = "mood_clust_fct", prob = TRUE,
                          which.class = "Pandemic")


# Plotting

ggplot(data = pd_values, aes(reorder(mood_clust_fct, yhat), yhat, 
                             label = round(yhat,3)))+
  geom_segment(aes(x=reorder(mood_clust_fct, yhat), 
                   xend=reorder(mood_clust_fct, yhat), y=0, 
                   yend=yhat), color= I(cols), size =4, alpha=.75)+
  geom_point(size= 15, shape = 21, color = I(cols), fill = I(cols), 
             alpha = 0.75) +
  geom_label(nudge_y = 0, nudge_x = 0, size = 3)+
  scale_x_discrete(limits= c("3","4","2","1"), 
                   labels= c("\nHigher Arousal-Potential\npos Emotionality (major)", 
                             "\nModerate Arousal-Potential\nneg Emotionality (minor)", 
                             "\nHigher Arousal-Potential\npos Emotionality (minor)", 
                             "\nModerate Arousal-Potential\nneg Emotionality (major)"))+
  labs(x="\nMood Clusters", y="Estimated Probabilities\n")+
  coord_flip()+
  layout

parallelStop()

# ### #
# END #
# ### #


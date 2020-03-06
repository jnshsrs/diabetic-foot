# Import Local Package "PEDIS"
unloadNamespace("pedis")
devtools::load_all("~/r-projects/proj_wunde/pedis/")
# Import tidyverse
library(tidyverse)

pedis <- pedis::read_all_pedis("~/r-projects/proj_wunde/pedis-diabetic-care/datasets/sicherheitskopien/")

# NHST independent t-Test
t.test(pedis$pedis_wert_5 ~ pedis$major_amputation)
t.test(pedis$pedis_wert_5 ~ pedis$any_amputation)
# Both tests are significant (H0 rejected)

pedis %>% 
  mutate_at("major_amputation", as_factor) %>% 
  ggplot(aes(y = pedis_wert_5, x = major_amputation)) +
  geom_boxplot()

# Bayesian Analysis t-TEst
bayes_t_test <- BayesianFirstAid::bayes.t.test(pedis$pedis_wert_5 ~ pedis$any_amputation, n.iter = 4e3)
plot(bayes_t_test)

bayes_t_test <- BayesianFirstAid::bayes.t.test(pedis$pedis_wert_5 ~ pedis$major_amputation, n.iter = 4e3)
plot(bayes_t_test)

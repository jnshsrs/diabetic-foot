library(tidyverse)
# Obtaining Odd Ratios from the Eurodiale study - Pickwell et al. 2015

# Fixed amputation status
# The table 1 of Pickwell et al. 2015 shows:
# The proportion of females under the non-amputees

# female given no amputation, male given no amputation, female given amputation, male given amputation
matrix(c("female and no amputation", "male and no amputation", "female and amputation", "male and amputation"),
       byrow = FALSE, nrow = 2, ncol = 2)

# no amputation, amputation, no amputation, amputation
verbundwahrscheinlichkeiten <- c(1 - .28, 1 - .28, .28, .28) * c(.406, 1 - .406, .295, 1 - .295)

verbundwahrscheinlichekeiten <- matrix(c(verbundwahrscheinlichkeiten), nrow = 2, ncol = 2, byrow = F)
colnames(verbundwahrscheinlichekeiten) <- c("no", "yes")
rownames(verbundwahrscheinlichekeiten) <- c("female", "male")
chisq.test(round(verbundwahrscheinlichekeiten * 575, 0))

# Leg characteristics
# Fixed amputation status
# Three groups of PAD (no PAD, PAD, severe PAD)
# Any amputation
# Leg Characterstics given the amputation status
# Verbundwahrscheinlichkeiten
# Given No Amputation
no_amp <- c("No PAD" = 1 - (.374 + .068), "PAD" = .374, "severe PAD" = .068) *  c(1 - .28, 1 - .28, 1 - .28)
# Given Amputation
yes_amp <- c("No PAD" = 1 - (.558 + .136), "PAD" = .558, "severe PAD" = .136) *  c(.28, .28, .28)
pad <- inner_join(enframe(no_amp), enframe(yes_amp), by = "name", suffix = c("_no", "_yes"))

pad <- pad %>%
  gather(key = amputation, value = prop, value_no, value_yes) %>%
  mutate_at("amputation", function(x) stringr::str_extract(pattern = "yes|no", string = x)) %>%
  mutate(abs = round(prop * 575, 0)) %>%
  mutate(data = map2(abs, amputation, function(x, y) rep(x = y, length.out = x))) %>%
  unnest(data) %>%
  mutate_at("amputation", function(x) if_else(x == "yes", 1L, 0L)) %>%
  mutate("predictor" = case_when(
    name == "No PAD" ~ 1L,
    name == "PAD" ~ 2L,
    name == "severe PAD" ~ 3L
  )) %>%
  select(predictor, amputation)

fit_pad <- glm(amputation ~ predictor, data = pad, family = binomial(link = "logit"))
coef(fit_pad)
coef(fit_pad) %>%  exp()

# OR = exp(0.767) = 2.15

# Ulcer Size
# No Amputation
no_amp <- c("< 1" = 1 - (.518 + .137), "1-5" = .518, "> 5" = .137) *  c(1 - .28, 1 - .28, 1 - .28)
# Yes Amputation
yes_amp <- c("< 1" = 1 - (.597 + .222), "1-5" = .597, "> 5" = .222) *  c(.28, .28, .28)
# Merge data (Verbundwahrscheinlichkeiten)
ulcer_size <- inner_join(enframe(no_amp), enframe(yes_amp), by = "name", suffix = c("_no", "_yes"))


ulcer_size <- ulcer_size %>%
  gather(key = amputation, value = prop, value_no, value_yes) %>%
  mutate_at("amputation", function(x) stringr::str_extract(pattern = "yes|no", string = x)) %>%
  mutate(abs = round(prop * 575, 0)) %>%
  mutate(data = map2(abs, amputation, function(x, y) rep(x = y, length.out = x))) %>%
  unnest(data) %>%
  mutate_at("amputation", function(x) if_else(x == "yes", 1L, 0L)) %>%
  mutate("predictor" = case_when(
    name == "< 1" ~ 1L,
    name == "1 - 5" ~ 2L,
    name == "> 5" ~ 3L
  )) %>%
  select(predictor, amputation)

fit_size <- glm(amputation ~ predictor, data = ulcer_size, family = binomial(link = "logit"))
fit_size %>%  coef()
fit_size %>%  coef() %>% exp()

# OR = exp(0.568) = 1.76

# Ulcer depth
# No Amputation
no_amp <- c("Superficial" = 1 - (.227 + .241), "Without probing to bone" = .227, "With probing to bone" = .241) *  c(1 - .28, 1 - .28, 1 - .28)
# Yes Amputation
yes_amp <- c("Superficial" = 1 - (.265 + .605), "Without probing to bone" = .265, "With probing to bone" = .605) *  c(.28, .28, .28)
# Merge data (Verbundwahrscheinlichkeiten)
ulcer_depth <- inner_join(enframe(no_amp), enframe(yes_amp), by = "name", suffix = c("_no", "_yes"))

ulcer_depth <- ulcer_depth %>%
  gather(key = amputation, value = prop, value_no, value_yes) %>%
  mutate_at("amputation", function(x) stringr::str_extract(pattern = "yes|no", string = x)) %>%
  mutate(abs = round(prop * 575, 0)) %>%
  mutate(data = map2(abs, amputation, function(x, y) rep(x = y, length.out = x))) %>%
  unnest(data) %>%
  mutate_at("amputation", function(x) if_else(x == "yes", 1L, 0L)) %>%
  mutate("predictor" = case_when(
    name == "Superficial" ~ 1L,
    name == "Without probing to bone" ~ 2L,
    name == "With probing to bone" ~ 3L
  )) %>%
  select(predictor, amputation)

fit_depth <- glm(amputation ~ predictor, data = ulcer_depth, family = binomial(link = "logit"))
coef(fit_depth)
exp(coef(fit_depth))

# OR = exp(1.105) = 3.02

prior_information <- list("PAD" = fit_pad, "Ulcer size" = fit_size, "Ulcer depth" = fit_depth) %>%
  map(broom::tidy) %>%
  bind_rows(.id = "model") %>%
  rename(beta = estimate) %>%
  mutate(exp_beta = exp(beta)) %>%
  filter(term != "(Intercept)") %>%
  mutate(prior_location_scaled = beta - 2 * `std.error`) %>%
  mutate(prior_location_scaled_exp = exp(prior_location_scaled))

avg_beta <- prior_information %>% pull(beta) %>% mean
avg_se <- prior_information %>% pull(std.error) %>% mean

exp_or <- exp(avg_beta)
exp_se <- exp(avg_se)

 x <- prior_information %>%
  summarise_if(is.numeric, mean) %>% 
  summarise_if(is.numeric, exp) %>% 
  bind_rows(prior_information %>% summarise_if(is.numeric, mean))






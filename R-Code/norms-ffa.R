

# Setup -------------------------------------------------------------------


library(easystats)
library(here)
library(tidyverse)



d_filename <- "Achtsamkeit_Daten_FFAEichung_rekodiert_2.sav"

d <- data_read(here("raw-data", d_filename))


ffa_items <-
  d %>% 
  select(starts_with("FFA_"))


ffa_items2 <-
  ffa_items %>% 
  mutate(across(.cols = everything(),
                .fns = ~ case_when(. == "fast nie" ~ 0,
                                   . == "eher selten" ~ 1,
                                   . == "relativ oft" ~ 2,
                                   . == "fast immer" ~ 3)))



# Check items -------------------------------------------------------------


ffa_items2 %>% 
  describe_distribution()


ffa13_items <-
  ffa_items2 %>% 
  select(-FFA_13_rek)




# Item distributions -------------------------------------------------------


items_names <-
  ffa_items %>% 
  names() %>% 
  set_names()

map(items_names,
    ~ ggplot(data = ffa_items2) + geom_bar(aes(x = .data[[.x]])) + labs(y = .x))




# Scale distribution

ggplot(d, aes(x = Summe)) + geom_density()
ggplot(d, aes(x = Presence)) + geom_density()
ggplot(d, aes(x = Acceptance)) + geom_density()
ggplot(d, aes(x = Acceptance13)) + geom_density()



d %>% 
  select(Summe, SummeFFA13, Presence, Acceptance, Acceptance13) %>% 
  describe_distribution()


# Quantiles ---------------------------------------------------------------

plot(ecdf(d$Presence))

pres_q <- ecdf(d$Presence)
pres_q(6:24)


norms_presence <-
  tibble(
    presence_score = 6:24,
    presence_percentile = pres_q(presence_score))
  )

norms_presence


acc13_q <- ecdf(d$Acceptance13)

norms_acceptance13 <-
  tibble(
    acc13_score = 7:28,
    acc13_percedntile = acc13_q(acc13_score)
  )

norms_acceptance13

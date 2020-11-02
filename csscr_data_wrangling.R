## -----------------------------------------------------------------------------------------------------------
install.packages("tidyverse")
library(tidyverse)


## -----------------------------------------------------------------------------------------------------------
install.packages("gapminder")
install.packages("billboard")
library(gapminder); library(billboard)

## -----------------------------------------------------------------------------------------------------------
head(gapminder, 3)
gapminder %>% filter(country == "Canada") %>% head(2)


## -----------------------------------------------------------------------------------------------------------
## take_these_data %>%
##     do_first_thing(with = this_value) %>%
##     do_next_thing(using = that_value) %>% ...


## -----------------------------------------------------------------------------------------------------------
gapminder %>% 
  lm(pop ~ year, data = .)


## -----------------------------------------------------------------------------------------------------------
lm_pop_year <- gapminder %>%
   filter(continent == "Americas") %>%
   lm(pop ~ year, data = .)


## -----------------------------------------------------------------------------------------------------------
gapminder %>% 
  filter(country == "Oman") %>% head(8)


## -----------------------------------------------------------------------------------------------------------
gapminder %>%
    filter(country == "Oman" &
           year > 1980 &
           year <= 2000 )


## -----------------------------------------------------------------------------------------------------------
unique(gapminder$country)

former_yugoslavia <- c("Bosnia and Herzegovina", "Croatia", 
              "Macedonia", "Montenegro", "Serbia", "Slovenia")

yugoslavia <- gapminder %>% filter(country %in% former_yugoslavia)
tail(yugoslavia, 2)


## -----------------------------------------------------------------------------------------------------------
yugoslavia %>%
  arrange(year, desc(pop))


## -----------------------------------------------------------------------------------------------------------
yugoslavia %>% 
  select(country, year, pop) %>% head(4)


## -----------------------------------------------------------------------------------------------------------
yugoslavia %>% 
  select(-continent, -pop, -gdpPercap) %>% head(4)


## -----------------------------------------------------------------------------------------------------------
names(billboard)

billboard %>% 
  select(artist:wk3) %>% 
  head(2)


## -----------------------------------------------------------------------------------------------------------
billboard %>% 
  select(artist, track, num_range("wk", 10:13)) %>% 
  head(3)

## -----------------------------------------------------------------------------------------------------------
billboard %>%
  select(artist, date = date.entered) %>%
  head(4)

## -----------------------------------------------------------------------------------------------------------
yugoslavia %>%
    select(country, year, lifeExp) %>%
    rename(Life_Expectancy = lifeExp) %>%
    head(4)

# CREATING VARIABLES
## -----------------------------------------------------------------------------------------------------------
yugoslavia %>% filter(country == "Serbia") %>%
    select(year, pop, lifeExp) %>%
    mutate(pop_million = pop / 1000000, #<<
           life_exp_past_40 = lifeExp - 40) %>% #<<
    head(5)


## -----------------------------------------------------------------------------------------------------------
## ifelse(test = x==y, yes = first_value , no = second_value)


## -----------------------------------------------------------------------------------------------------------
example <- c(1, 0, NA, -2)
ifelse(example > 0, "Positive", "Not Positive")


## -----------------------------------------------------------------------------------------------------------
yugoslavia %>% mutate(short_country = 
                 ifelse(country == "Bosnia and Herzegovina", 
                        "B and H", as.character(country))) %>%
    select(short_country, year, pop) %>%
    arrange(year, short_country) %>%
    head(3)


## -----------------------------------------------------------------------------------------------------------
gapminder %>% 
  mutate(gdpPercap_ordinal = 
    case_when(
      gdpPercap <  700 ~ "low",
      gdpPercap >= 700 & gdpPercap < 800 ~ "moderate",
      TRUE ~ "high" )) %>% # Value when all other statements are TRUE
  slice(6:9) # get rows 6 through 9


# SUMMARIZING DATA
## -----------------------------------------------------------------------------------------------------------
yugoslavia %>%
    filter(year == 1982) %>%
    summarize(n_obs = n(),
              total_pop = sum(pop),
              mean_life_exp = mean(lifeExp),
              range_life_exp = max(lifeExp) - min(lifeExp))


## -----------------------------------------------------------------------------------------------------------
yugoslavia %>%
  filter(year == 1982) %>%
  summarize(across(c(lifeExp, pop), list(avg = ~mean(.), sd = ~sd(.))))


## -----------------------------------------------------------------------------------------------------------
yugoslavia %>% 
  select(year, lifeExp, pop, gdpPercap) %>% 
  summarize_all(funs(mean, sd))


## -----------------------------------------------------------------------------------------------------------
yugoslavia %>% 
  summarize_if(is.numeric, funs(mean, sd))


## -----------------------------------------------------------------------------------------------------------
yugoslavia %>%
  group_by(year) %>% #<<
    summarize(num_countries = n_distinct(country),
              total_pop = sum(pop),
              total_gdp_per_cap = sum(pop*gdpPercap)/total_pop) %>%
    head(5)


## -----------------------------------------------------------------------------------------------------------
yugoslavia %>% 
  select(country, year, pop) %>%
  filter(year >= 2002) %>% 
  group_by(country) %>%
  mutate(lag_pop = dplyr::lag(pop, order_by = year),
         pop_chg = pop - lag_pop) %>%
  head(4)


## -----------------------------------------------------------------------------------------------------------
enrollment <- 
  tibble(Program = c("Evans School", "Arts & Sciences",
                   "Public Health", "Other"),
       Female  = c(10, 5, 2, 5),
       Male    = c(6, 6, 3, 1))
pander::pander(enrollment, style="rmarkdown")


## -----------------------------------------------------------------------------------------------------------
(enrollment_long <- enrollment %>%
  pivot_longer(-Program, names_to="Gender", values_to="Count"))


## -----------------------------------------------------------------------------------------------------------
(enrollment_wide <- enrollment_long %>%
  pivot_wider(names_from="Gender", values_from="Count"))


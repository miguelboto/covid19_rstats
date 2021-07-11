
library(tidyverse)
library(broom)
library(patchwork)
library(lubridate)
library(data.table)
library(caret)
library(ggridges)
library(xts)
library(dygraphs)
library(readr)
library(openxlsx)
library(readxl)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(rpart)
library(cvms)
library(tibble) 
library(psych)
library(PerformanceAnalytics)
library(corrplot)
theme_set(theme_bw())


url <-"https://covid.ourworldindata.org/data/owid-covid-data.csv"

download.file(url, destfile = "owid_covid_data", mod="wb")

owid_covid_data <- read.csv("owid_covid_data")

owid_covid_data$date <- as.Date(owid_covid_data$date)

summary(owid_covid_data)

x <- owid_covid_data %>% filter(date == "2021-03-21" & location %in% c("Germany", "Finland"))



x <- x[,c(-18,	-19,	-20,	-21,	-22,	-23,	-24,	-25,	-26,	-27,	-28,	-29,	-30,	-31,	-32,	-33,	-34, -44, -51, -56)]

x <- cor(x[5:39]) 



corrplot(x, type ="upper")

corrplot.mixed(x, lower = "ellipse", upper = "circle")

pairs.panels(x)

covid19_bras0407 <- covid19_bras0407 %>% mutate(deaths_per_million = deaths/(estimated_population_2019/10^6))

covid19_bras0407 <- left_join(covid19_bras0407, demo_brasil, by = "state")



#boxplot country rate deaths per million
owid_covid_data  %>%  group_by(location) %>% top_n(30, total_deaths) %>% mutate(location = reorder(total_deaths_per_million, location)) %>%
  ggplot(aes(location, total_deaths_per_million, fill = total_deaths_per_million)) +
  geom_bar(stat="identity") +
  xlab("Country") +
  ylab("Deaths per million") +
  ggtitle("Deaths Per Million Rate by Country @Miguel A. Boto") + 
  coord_flip() +
  scale_fill_gradient(low = "green", high = "blue", na.value = NA)


owid_covid_data %>% group_by(iso_code, continent)  %>%
  filter(!people_fully_vaccinated_per_hundred & !is.na(aged_70_older) &!is.na(total_deaths) & !is.na(total_deaths) & 
           !is.na(gdp_per_capita) & !is.na(hospital_beds_per_thousand)) %>% summarise(total_deaths = max(total_deaths), population = max(population), gdp_per_capita =
                                               max(gdp_per_capita), population_density = max(population_density), 
                                               median_age = max(median_age), aged_70_older = max(aged_70_older), people_fully_vaccinated_per_hundred =
                                                 max(people_fully_vaccinated_per_hundred)) %>%
  mutate(deaths_per_million = total_deaths / population) %>% filter(deaths_per_million >= 0.0001 & iso_code != "OWID_WRL") %>%
  ggplot(aes(people_fully_vaccinated_per_hundred, deaths_per_million, fill = continent)) +
  geom_point(aes(color = continent)) +
  scale_x_continuous() +
  geom_label_repel(aes(label = iso_code), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') 
  
owid_covid_data %>% group_by(iso_code, continent)  %>%
  filter(!is.na(total_cases_per_million) & !is.na(people_fully_vaccinated_per_hundred) & !is.na(aged_70_older) &!is.na(total_deaths) & !is.na(total_deaths) & 
           !is.na(gdp_per_capita) & !is.na(hospital_beds_per_thousand)) %>% summarise(total_deaths = max(total_deaths), population = max(population), gdp_per_capita =
                                                                                        max(gdp_per_capita), population_density = max(population_density), 
                                                                                      median_age = max(median_age), aged_70_older = max(aged_70_older), 
                                                                                      total_vaccinations_per_hundred = max(total_vaccinations_per_hundred),
                                                                                      total_cases_per_million = max(total_cases_per_million)) %>%
  mutate(deaths_per_million = total_deaths / population) %>% filter(iso_code != "OWID_WRL" & deaths_per_million >= 0.0009) %>%
  ggplot(aes(deaths_per_million, total_cases_per_million, fill = continent)) +
  geom_point(aes(color = continent)) +
  scale_x_continuous() +
  geom_label_repel(aes(label = iso_code), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') +
   ggtitle("Covid-19: deaths per million VS cases per million - @mboto")


 ## Barplot Death Rate by Brazilian State
 Covid19_Brasil_22042020 %>% filter(place_type == "state") %>%
   mutate(state = reorder(state, deaths / (estimated_population_2019/10^6))) %>%
   ggplot(aes(state, deaths / (estimated_population_2019/10^6), fill = deaths / (estimated_population_2019/10^6))) +
   geom_bar(stat="identity") +
   xlab("State") +
   ylab("Deaths Rate per million") +
   ggtitle("Deaths Rate Per Million by Brazilian States - 21/04/2020  @Miguel A. Boto") + 
   coord_flip() +
   scale_fill_gradient(low = "green", high = "blue", na.value = NA)
 
 #Aesthetic brazilians state rate death per million vs population
 covid19_bras0407 %>% filter(place_type == "state" & is_last == "TRUE") %>%
   ggplot(aes(estimated_population_2019, deaths_per_million, label = Region)) +
   geom_point(aes(col=deaths), size = 3) + geom_text(nudge_x = 0.09) +
   scale_x_continuous(trans = "log10") +
   scale_y_continuous(trans = "log10") +
   xlab("Population (log scale)") +
   ylab("Deaths per Million (log scale)") +
   ggtitle("Brazilian States Deaths Per Million Rate by Population 04/07/2020 @Miguel A. Boto")
 
 #Aesthetic mapping; brazilians cities rate - death per million vs population
 covid19_bras2406 %>% top_n(50, deaths_per_million) %>% filter(place_type == "city" & is_last == "True") %>%
   ggplot(aes(estimated_population_2019, deaths_per_million, label = city)) +
   geom_point(aes(Col=="state"))
   scale_x_continuous(trans = "log10") +
   scale_y_continuous(trans = "log10") +
   xlab("Population (log scale)") +
   ylab("Deaths Rate (log scale)") +
   ggtitle("Covid-19 Deaths Per Million Rate by Population- Brazilian Cities 24/06/2020 @Miguel A. Boto")
   
 ## Barplot Death Rate - Brazilian Cities > 450k
 Covid19_Brasil_22042020 %>% top_n(350, death_rate) %>% filter(place_type == "city" & estimated_population_2019 > 450000) %>%
   mutate(city = reorder(city, death_rate)) %>%
   ggplot(aes(city, death_rate, fill = death_rate)) +
   geom_bar(stat="identity") +
   xlab("City") +
   ylab("Deaths per million Rate") +
   ggtitle("Covid-19 Deaths Per Million Rate- Brazilian Cities > 450k - 21/04/2020  @Miguel ?. Boto") + 
   coord_flip() +
   scale_fill_gradient(low = "green", high = "blue", na.value = NA)
 
 #Aesthetic mapping; brazilians cities rate - death per million vs population
 covid19_bras2406 %>% filter(is_last == "True" & place_type == "city") %>%
   top_n(40, estimated_population_2019) %>% 
   ggplot(aes(estimated_population_2019, deaths_per_million, label = city)) +
   geom_point(aes(col=deaths_per_million), size = 3) + geom_text(nudge_x = 0.09) +
   scale_x_continuous(trans = "log10") +
   scale_y_continuous(trans = "log10") +
   xlab("Population (log scale)") +
   ylab("Deaths Rate (log scale)") +
   ggtitle("Covid-19 Deaths Per Million Rate by Population- Brazilian Cities 24/06/2020 @Miguel A. Boto")
 
 ## Barplot Death Rate - Brazilian Cities > 450k
covid19_bras0407 %>% filter(place_type == "city" & is_last == "True") %>%
  top_n(40, estimated_population_2019) %>%
   mutate(city = reorder(city, deaths_per_million)) %>%
   ggplot(aes(city, deaths_per_million), fill = deaths_per_million) +
   geom_bar(stat="identity") +
   xlab("City") +
   ylab("Deaths Rate per million") +
   ggtitle("Deaths Rate Per Million by Brazilian Cities - 04/07/2020  @Miguel A. Boto") + 
   coord_flip() +
   scale_fill_gradient(low = "green", high = "blue", na.value = NA)

owid_covid_data %>%  
  filter(date == "2021-06-01" & !is.na(continent) & total_deaths_per_million >= 75) %>%
  top_n(40, total_deaths_per_million) %>%
  mutate(location = reorder(location, total_deaths_per_million)) %>%
  ggplot(aes(location, total_deaths_per_million), fill = total_deaths_per_million) +
  geom_bar(stat="identity") +
  xlab("Location") +
  ylab("Deaths per million") +
  coord_flip() +
  scale_fill_gradient(low = "green", high = "blue", na.value = NA) +
  ggtitle("Deaths per million by country - 2021-03-24  @Miguel A. Boto") 

owid_covid_data %>% filter(date == "2021-06-01" & total_deaths_per_million >= 500 ) %>%
  ggplot(aes(total_deaths_per_million, total_cases_per_million, color = continent)) +
  geom_point(size = 1) +
  geom_label_repel(aes(label = location), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') +
  ggtitle("Deaths & cases per million rate by country - 2021-03-24  @Miguel A. Boto")

owid_covid_data  %>%
  ggplot(aes(total_tests_per_thousand, total_vaccinations_per_hundred, color = continent)) +
  geom_point(size = 1) +
  geom_label_repel(aes(label = location), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') +
  ggtitle("Test & vaccinations rate by country - 2021-03-24  @Miguel A. Boto")

owid_covid_data %>% filter(date == "2021-03-24" & total_deaths_per_million >= 250 & population_density <= 750 & continent == "Europe") %>%
  ggplot(aes(total_deaths_per_million, population_density, color = population_density)) +
  geom_point(size = 1) +
  geom_label_repel(aes(label = location), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') +
  ggtitle("Deaths pe million per population density  by country - 2021-03-24  @Miguel A. Boto")
 
owid_covid_data %>% filter(date == "2021-03-24" & !is.na(continent)) %>%
  ggplot(aes(total_deaths_per_million, total_vaccinations_per_hundred, color = continent)) +
  geom_point() +
  
owid_covid_data %>% filter(!is.na(continent) & total_deaths >= 2000) %>%
  mutate(iso_code= reorder(iso_code, total_deaths_per_million)) %>%
  ggplot(aes(iso_code, total_deaths_per_million, fill = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Boxplot Daily Deaths Rate by Country - 26/06/20 @Miguel A. Boto")  +
  ylab("Deaths Rate") + xlab("Country")

summary(owid_covid_data)

owid_covid_data %>% filter(!is.na(continent) & total_deaths >= 500) %>%
  mutate(location= reorder(location, total_deaths_per_million)) %>%
  ggplot(aes(date, location, fill = total_deaths_per_million)) +
  geom_tile(color = "grey50") +
  scale_x_discrete(expand=c(0,0)) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position="bottom",
        text = element_text(size = 8)) +
  ggtitle("Heatmap Deaths Rate Daily Evolution by country - 26/06/20 @Miguel A. Boto")  +
  ylab("Country") + xlab("Daily rate death")




covid19_br_2404 %>% filter(estimated_population_2019 < 800000, estimated_population_2019 > 100000, death_rate > 25) %>%
  ggplot(aes(estimated_population_2019, death_rate, label = city)) +
  geom_point(aes(col=state), size = 4) + geom_text(nudge_x = 0.04) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  xlab("Population (log scale)") +
  ylab("Deaths Rate (log scale)") +
  ggtitle("Covid-19 Deaths Per Million Rate by Population - Brazilian Cities > 850k
          24/04/2020 @Miguel ?. Boto")


bras26 <- Total_Covid_19_Brazil_Deaths_27042020 %>% filter(is_last == "True" & place_type == "city") %>%
  mutate(rate = deaths / (estimated_population_2019/10^6)) %>%
  select(state, city, deaths, estimated_population_2019, rate)

bras26 %>% filter(estimated_population_2019 >= 700000) %>%
  ggplot(aes(estimated_population_2019, rate, label = city)) +
  geom_point(aes(col=state), size = 4) + geom_text(nudge_x = 0.04) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  xlab("Population (log scale)") +
  ylab("Deaths Rate (log scale)") +
  ggtitle("Covid-19 Deaths Per Million Rate by Population - Brazilian Cities 26/04/2020 @Miguel ?. Boto")

bras28 <- covid19_brasil_280420 %>% filter(is_last == "True" & place_type == "city") %>%
  mutate(rate = deaths / (estimated_population_2019/10^6)) %>%
  select(state, city, deaths, estimated_population_2019, rate)

bras28_st <- covid19_brasil_280420 %>% filter(is_last == "True" & place_type == "state") %>%
  mutate(rate = deaths / (estimated_population_2019/10^6)) %>%
  select(state, city, deaths, estimated_population_2019, rate, Region, GDP)

owid_covid_data %>% group_by(location) %>% 
  filter(date == "2020-06-25" & !is.na(continent) & !is.na(hospital_beds_per_thousand) & total_deaths_per_million >= 75) %>%
  mutate(hospital_beds_per_thousand = reorder(hospital_beds_per_thousand)) %>%
  ggplot(aes(hospital_beds_per_thousand, total_deaths_per_million, label = iso_code)) +
  geom_point(aes(col=continent), size = 4) + 
  geom_text(nudge_x = 0.19) +
  xlab("hosp. beds per million") +
  ylab("Deaths Rate ") +
  ggtitle("Covid-19 Deaths Per Million Rate by GDP in U$D - 27/06/2020 @Miguel ?. Boto")


bras28 %>% filter(estimated_population_2019 >= 700000) %>%
  ggplot(aes(estimated_population_2019/10^6, rate, label = city)) +
  geom_point(aes(col=state), size = 4) + geom_text(nudge_x = 0.04) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  xlab("Population (log scale)") +
  ylab("Deaths Rate (log scale)") +
  ggtitle("Covid-19 Deaths Per Million Rate by Population - Brazilian Cities 28/04/2020 @Miguel ?. Boto")


bras28_st %>% 
  ggplot(aes(GDP, rate, label = state)) +
  geom_point(aes(col=Region), size = 4) + geom_text(nudge_x = 0.04) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("GDP in U$D (log scale)") +
  ylab("Deaths Rate (log scale)") +
  ggtitle("Covid-19 Deaths Per Million Rate by GDP in U$D - Brazilian states 28/04/2020 @Miguel ?. Boto")

bras29 <- covid19_brasil_290420 %>% filter(is_last == "True" & place_type == "city") %>%
  mutate(rate = deaths / (estimated_population_2019/10^6)) %>%
  select(state, city, deaths, estimated_population_2019, rate)

bras29_st <- covid19_brasil_290420 %>% filter(is_last == "True" & place_type == "state") %>%
  mutate(rate = deaths / (estimated_population_2019/10^6)) %>%
  select(state, city, deaths, estimated_population_2019, rate, Region, GDP)

bras29 %>% filter(estimated_population_2019 >= 700000) %>%
  ggplot(aes(estimated_population_2019/10^6, rate, label = city)) +
  geom_point(aes(col=state), size = 4) + geom_text(nudge_x = 0.04) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  xlab("Population (log scale)") +
  ylab("Deaths Rate (log scale)") +
  ggtitle("Covid-19 Deaths Per Million Rate by Population - Brazilian Cities > 700k 29/04/2020 @Miguel ?. Boto")

bras29_st %>% 
  ggplot(aes(estimated_population_2019, rate, label = state)) +
  geom_point(aes(col=Region), size = 4) + geom_text(nudge_x = 0.04) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("Population / 10^6 (log scale)") +
  ylab("Deaths Rate (log scale)") +
  ggtitle("Covid-19 Deaths Per Million Rate by Population - Brazilian States 29/04/2020 @Miguel ?. Boto")



bras29_st %>%  mutate(state = reorder(state, rate)) %>%
ggplot(aes(state, rate, fill = rate)) +
geom_bar(stat = "identity") +
coord_flip() +
scale_fill_gradient(low = "green", high = "red", na.value = NA)

bras29 %>%  filter(estimated_population_2019 >= 600000) %>%
  mutate(city = reorder(city, rate)) %>%
  ggplot(aes(city, rate, fill = rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "green", high = "red", na.value = NA)

covid19_brasil_290420 %>% filter(is_last == "True" & place_type == "city") %>% summarize(sum(deaths))


#boxplot country rate deaths per million
Covid19_total_paises %>% top_n(40, Deaths) %>% mutate(Country = reorder(Country, `Deaths per million`)) %>%
  ggplot(aes(Country, `Deaths per million`, fill = `Deaths per million`)) +
  geom_bar(stat="identity") +
  xlab("Country") +
  ylab("Deaths per million") +
  ggtitle("Deaths Per Million Rate by Country 30/04/2020 @Miguel ?. Boto") + 
  coord_flip() +
  scale_fill_gradient(low = "green", high = "blue", na.value = NA)


#aesthetic mapping; country rate death per million vs GPD
Covid19_total_paises %>% top_n(65, Deaths) %>% ggplot(aes(GPD, `Deaths per million`, label = Country)) +
  geom_point(aes(col=continent), size = 3) + geom_text(nudge_x = 0.09) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("Gross Profit Dollars (log scale)") +
  ylab("Deaths per Million (log scale)") +
  ggtitle("Country Deaths Per Million Rate by GPD 30/04/2020 @Miguel Á. Boto") +
  geom_vline(xintercept=27000, col = "blue")

#aesthetic mapping; country rate death per million vs GPD
Covid19_total_paises %>% top_n(65, Deaths) %>% ggplot(aes(Population/10^6, `Deaths per million`, label = Country)) +
  geom_point(aes(col=continent), size = 3) + geom_text(nudge_x = 0.09) +
  scale_x_continuous(trans = "log10") +
  xlab("Population/10^6 (log scale)") +
  scale_y_continuous(trans = "log10") +
  ylab("Deaths per Million (log scale)") +
  ggtitle("Country Deaths Per Million Rate by Population 30/04/2020 @Miguel Á. Boto") 
  
  bras0205_city <- covid19_brasil_020520 %>% filter(is_last == "True" & place_type == "city") %>%
  mutate(rate = deaths / (estimated_population_2019/10^6)) %>%
  select(state, city, deaths, estimated_population_2019, rate)

bras0205_st <- covid19_brasil_020520 %>% filter(is_last == "True" & place_type == "state") %>%
  mutate(rate = deaths / (estimated_population_2019/10^6)) %>%
  select(state, city, deaths, estimated_population_2019, rate, Region, GDP)

covid19_bras2406 %>% filter(is_last == "True" & place_type == "city" & deaths_by_population >= 300) %>%
  ggplot(aes(estimated_population_2019/10^6, deaths_by_population, label = city)) +
  geom_point(aes(col=state), size = 4) + 
  geom_text(nudge_x = 0.04) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  xlab("Population (log scale)") +
  ylab("Deaths Rate (log scale)") +
  ggtitle("Covid-19 Deaths Per Million Rate by Population - Brazilian Cities > 700k 24/06/2020 @Miguel A. Boto")

covid19_bras2406 %>%  filter(place_type == "state" & is_last == "True") %>%
  ggplot(aes(estimated_population_2019, deaths_per_million, label = state)) +
  geom_point(aes(col=deaths_per_million), size = 4) + geom_text(nudge_x = 0.04) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("Population / 10^6 (log scale)") +
  ylab("Deaths Rate (log scale)") +
  ggtitle("Covid-19 Deaths Per Million Rate by Population - Brazilian States 24/06/2020 @Miguel A. Boto")



covid19_bras2406 %>%  filter(place_type == "state" & is_last == "True") %>%
  mutate(state = reorder(state, deaths_per_million)) %>%
  ggplot(aes(state, deaths_per_million, fill = deaths_per_million)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "green", high = "red", na.value = NA)

bras0205_city %>%  filter(estimated_population_2019 >= 600000) %>%
  mutate(city = reorder(city, rate)) %>%
  ggplot(aes(city, rate, fill = rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "green", high = "red", na.value = NA)

#boxplot country rate deaths per million
Covid19_total_paises %>% top_n(40, Deaths) %>% mutate(Country = reorder(Country, `Deaths per million`)) %>%
  ggplot(aes(Country, `Deaths per million`, fill = `Deaths per million`)) +
  geom_bar(stat="identity") +
  xlab("Country") +
  ylab("Deaths per million") +
  ggtitle("Deaths Per Million Rate by Country 02/05/2020 @Miguel ?. Boto") + 
  coord_flip() +
  scale_fill_gradient(low = "green", high = "blue", na.value = NA)


#aesthetic mapping; country rate death per million vs GPD
Covid19_total_paises %>% top_n(65, Deaths) %>% ggplot(aes(GPD, `Deaths per million`, label = Country)) +
  geom_point(aes(col=continent), size = 3) + geom_text(nudge_x = 0.09) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("Gross Profit Dollars (log scale)") +
  ylab("Deaths per Million (log scale)") +
  ggtitle("Country Deaths Per Million Rate by GPD 02/05/2020 @Miguel Á. Boto") +
  geom_vline(xintercept=27000, col = "blue")

#aesthetic mapping; country rate death per million vs GPD
Covid19_total_paises %>% top_n(65, Deaths) %>% ggplot(aes(Population/10^6, `Deaths per million`, label = Country)) +
  geom_point(aes(col=continent), size = 3) + geom_text(nudge_x = 0.09) +
  scale_x_continuous(trans = "log10") +
  xlab("Population/10^6 (log scale)") +
  scale_y_continuous(trans = "log10") +
  ylab("Deaths per Million (log scale)") +
  ggtitle("Country Deaths Per Million Rate by Population 02/05/2020 @Miguel Á. Boto") 

url <- "https://www.linkedin.com/company/11456974/admin/analytics/followers/"
dat <- read_csv(url)
download.file(url, "linkedin followres.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

library(diplyr)

names(dat)[names(dat) == "state"] <- "CO_SIGLA_ESTADO"

head(dat)

dat2 <- names(dat)[2]<-"CO_SIGLA_ESTADO"

head(dat2)

dat %>%  filter(is_last == "TRUE" & place_type == "city" & estimated_population_2019 >= 500000) %>%
  mutate(rate = deaths/(estimated_population_2019/10^6)) %>%
  mutate(city = reorder(city, rate)) %>%
  ggplot(aes(city, rate, fill = rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "green", high = "red", na.value = NA) +
  ggtitle("Deaths Rate by city Rank > 500k - 21/05/20 @Miguel Á. Boto") 

dat %>%
  filter(place_type == "state" & is_last == "TRUE") %>%
  mutate(rate = deaths/(estimated_population_2019/10^6)) %>%
  ggplot(aes(rate)) +
  geom_density()

dat %>%
  filter(place_type == "state" & is_last == "TRUE") %>%
  mutate(rate = deaths/(estimated_population_2019/10^6)) %>%
  ggplot(aes(sample = rate)) +
  geom_qq()

states <- c("SP", "PE", "AM", "MA", "SC", "PB", "ES", "BA", "MG", "GO", "CE", "DF", "PA", 'RJ', "PR", "PI", "RR","RN", "RS", "RO", "MT", "TO", "MS")

cities <- c("Manaus", "São Luís", "São Paulo", "Rio de Janeiro", "Recife", "Belo Horizonte", "Goiás", "Brasília", "Fortaleza", "Porto Alegre", "Curitiba", "Salvador")

dat %>% filter(place_type == "state" & CO_SIGLA_ESTADO == "MG") %>% 
  mutate(rate = deaths/(estimated_population_2019/10^6)) %>%
  ggplot(aes(date, rate, col = CO_SIGLA_ESTADO)) +
  geom_line() +
  xlab("Data") +
  ylab("Taxa de óbitos por população") +
  ggtitle("Evolução Taxa de óbitos por População e Estado - @Miguel Á. Boto - 18/05/20")

dat %>% filter(place_type == "city" & !is.na(deaths) & deaths >= 10 & estimated_population_2019 >= 900000) %>% 
  mutate(rate = deaths/(estimated_population_2019/10^6)) %>% 
  ggplot(aes(date, rate, col = city)) +
  geom_line() +
  xlab("Data") +
  ylab("Taxa de óbitos por população") +
  ggtitle("Evolução Taxa de óbitos por população e cidade > 900k - @Miguel Á. Boto - 18/05/20")

head(dat)

dat

# UNIÓN DE TABLA DIARIA CON BBDD DE CAMAS
  
 bd_cruzada <- inner_join(dat, demo_brasil, by = "CO_SIGLA_ESTADO")
 
 bd_cruzada <- bd_cruzada %>% mutate(rate = deaths/(estimated_population_2019/10^6))
 
 head(bd_cruzada)
 
 
days <- c(2020-04-15, 2020-05-15)
  
bd_cruzada %>%  filter(is_last == "TRUE", place_type == "city" & estimated_population_2019 >=50000) %>% 
  ggplot(aes(taxa_leitos_estado, rate, color = Region)) +
  geom_point()

bd_cruzada %>% filter(place_type == "city" & estimated_population_2019 >= 10000 & is_last == "TRUE") %>% 
  ggplot(aes(deaths, rate, col = Region)) +
  geom_point() + 
  facet_grid(Region~date)

p <- bd_cruzada %>%
  filter(is_last == "TRUE") %>%
  mutate(state = reorder(state, rate)) %>%
  ggplot(aes(state, rate, fill = Region)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Boxplot Taxa de óbitos por Estado - @Miguel Á. Boto - 18/05/20")

p + geom_point(alpha = 0.5)

bd_cruzada %>% filter(is_last == "TRUE") %>%
  ggplot(aes(Region, rate)) +
  geom_jitter(width = 0.1, alpha = 0.2)

bd_cruzada %>% filter(is_last == "TRUE" & place_type == "city") %>%
  ggplot(aes(rate, ..density..)) +
  geom_histogram(binwidth = 1, color="black") +
  facet_grid(Region~.)

library(RColorBrewer)

class(bd_cruzada$rate)

dat3 <- bd_cruzada %>%
  filter(rate == the_rate) 




bd_cruzada %>% mutate(rate = deaths/(estimated_population_2019/10^6)) %>%
  filter(place_type == "state") %>% ggplot(aes(date, state, fill = rate)) +
  geom_tile(color = "grey50") +
  scale_x_discrete(expand=c(0,0)) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept=2020-05-01, col = "blue") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position="bottom",
        text = element_text(size = 8)) +
  ggtitle("Deaths Rate Daily Evolution by State - 18/05/20 @Miguel Á. Boto")  +
   ylab("") + xlab("")

bd_cruzada %>% filter(date >= 2020-20-04) %>% mutate(rate = deaths/(estimated_population_2019/10^6)) %>%
  mutate(city = reorder(city, rate)) %>%
  filter(place_type == "city" & estimated_population_2019 >= 500000) %>% ggplot(aes(date, city, fill = rate)) +
  geom_tile(color = "grey50") +
  scale_x_discrete(expand=c(0,0)) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept=2020-05-01, col = "blue") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position="bottom",
        text = element_text(size = 8)) +
  ggtitle("Deaths Rate Daily Evolution by City > 500k- 21/05/20 @Miguel Á. Boto")  +
  ylab("") + xlab("")

                                                          
bd_cruzada %>% filter(place_type == "state" & is_last == "TRUE") %>%
  ggplot(aes(estimated_population_2019/10^6, rate, label = CO_SIGLA_ESTADO)) +
  geom_point(aes(col=Region), size = 3) + 
  geom_text(nudge_x = 0.01) +
  scale_x_continuous(trans = "log10") +
  xlab("População (log scale)") +
  scale_y_continuous(trans = "log10") +
  ylab("Taxa de Óbitos por milhão (log scale)") +
  ggtitle("Taxa de óbitos por milhão -  21/05/2020 @Miguel Á. Boto") 

bd_cruzada %>% filter(place_type == "state" & is_last == "TRUE") %>%
  ggplot(aes(GDP_per_capita, rate, label = CO_SIGLA_ESTADO)) +
  geom_point(aes(col=Region), size = 3) + 
  geom_text(nudge_x = 0.01) +
  scale_x_continuous(trans = "log10") +
  xlab("Taxa leitos por população (log scale)") +
  scale_y_continuous(trans = "log10") +
  ylab("Taxa de Óbitos por milhão (log scale)") +
  ggtitle("Taxa de óbitos por U$D GDP per capita-  18/05/2020 @Miguel Á. Boto") 

bd_cruzada %>% filter(place_type == "state" & is_last == "TRUE") %>%
  ggplot(aes(GDP_per_capita, taxa_leitos_estado, label = CO_SIGLA_ESTADO)) +
  geom_point(aes(col=Region), size = 3) + 
  geom_text(nudge_x = 0.01) +
  scale_x_continuous(trans = "log10") +
  xlab("Taxa leitos por população (log scale)") +
  scale_y_continuous(trans = "log10") +
  ylab("Taxa de Óbitos por milhão (log scale)") +
  ggtitle("Taxa de leitos vs GDP em U$D per capita-  18/05/2020 @Miguel Á. Boto") 

bd_cruzada %>% filter(place_type == "state" & is_last == "TRUE") %>%
  ggplot(aes(GDP_per_capita, rate, label = CO_SIGLA_ESTADO)) +
  geom_point(aes(col=Region), size = 3) + 
  geom_text(nudge_x = 0.05) +
  xlab("GDP U$D per capita") +
  ylab("Taxa de óbitos por milhão") +
  ggtitle("Taxa de óbitos vs GDP em U$D per capita-  18/05/2020 @Miguel Á. Boto") 

bd_cruzada %>% filter(place_type == "state" & is_last == "TRUE") %>%
  ggplot(aes(taxa_leitos_estado, rate, label = CO_SIGLA_ESTADO)) +
  geom_point(aes(col=Region), size = 3) + 
  geom_text(nudge_x = 0.01) +
  xlab("Taxa leitos por população") +
  ylab("Taxa de Óbitos por milhão ") +
  ggtitle("Taxa de óbitos por leitos per 100k habitantes -  21/05/2020 @Miguel Á. Boto") 

bd_cruzada %>% filter(place_type == "state" & is_last == "TRUE") %>%
  ggplot(aes(GDP_per_capita, taxa_leitos_estado, label = CO_SIGLA_ESTADO)) +
  geom_point(aes(col=Region), size = 3) + 
  geom_text(nudge_x = 0.01) +
  xlab("GDP in U$D per capita") +
  ylab("Taxa de leitos por população") +
  ggtitle("Taxa de leitos vs GDP em U$D per capita-  18/05/2020 @Miguel Á. Boto")


 
## Barplot tasa de muertes por provincia Espa?a
Covid19_total_provincia_esp %>% mutate(Provincia = reorder(Provincia, `Tasa Fallecidos / 100k habitantes`)) %>%
  ggplot(aes(Provincia, `Tasa Fallecidos / 100k habitantes`, fill = `Tasa Fallecidos / 100k habitantes`)) +
  geom_bar(stat="identity") +
  xlab("Provincia") +
  ylab("Tasa fallecidos 100k habitantes") +
  ggtitle("Tasa de fallecidos por 100k habitantes por provincia - España 12/04/2020 @Miguel ?. Boto") + 
  coord_flip() +
  scale_fill_gradient(low = "green", high = "blue", na.value = NA)


 
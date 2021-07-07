
knitr::opts_chunk$set(echo = TRUE)
library(psych)
library(tidyverse)
library(ggthemes)
library(dplyr)
library(extrafont)
library(RColorBrewer)
library(plotly)
library(tidymodels)
library(countrycode)
library(kableExtra)
library(ppsr)
library(patchwork)
library(gganimate)
library(mice)
library(stringr)
library(rworldmap)
library(data.table)
library(gifski)
library(htmltools)
library(naniar)
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyfullscreen)



intervals <- tibble(read_csv("data/fao_fra_datasets/fra_forest_development_data.csv"))


Forest_Area<- tibble(read_csv("data/fao_fra_datasets/fra_forest_area_data.csv"))

intervals_1<-intervals %>% mutate(`1d_deforestation`=case_when(year %in% c("1990-2000","2000-2010")~`1d_deforestation`*10,year %in% c("2010-2015","2015-2020")~`1d_deforestation`*5)) %>% mutate(`1d_expansion`=case_when(year %in% c("1990-2000","2000-2010")~`1d_expansion`*10,year %in% c("2010-2015","2015-2020")~`1d_expansion`*5)) %>% mutate(`1d_afforestation`=case_when(year %in% c("1990-2000","2000-2010")~`1d_afforestation`*10,year %in% c("2010-2015","2015-2020")~`1d_afforestation`*5)) %>% mutate(`1e_reforestation`=case_when(year %in% c("1990-2000","2000-2010")~`1e_reforestation`*10,year %in% c("2010-2015","2015-2020")~`1e_reforestation`*5)) %>%mutate(`1d_nat_exp`=case_when(year %in% c("1990-2000","2000-2010")~`1d_nat_exp`*10,year %in% c("2010-2015","2015-2020")~`1d_nat_exp`*5)) 

intervals_2<- intervals_1%>% select(regions,name,`1d_deforestation`,`1d_expansion`,`1d_afforestation`,`1e_reforestation`,`1d_nat_exp`)  %>% group_by(name,regions) %>% summarize(t_deforestation =sum(as.numeric(`1d_deforestation`),na.rm = TRUE),t_expansion=sum(as.numeric(`1d_expansion`),na.rm = TRUE),t_afforestation =sum(as.numeric(`1d_afforestation`),na.rm = TRUE),t_reforestation =sum(as.numeric(`1e_reforestation`),na.rm = TRUE),t_nat_exp =sum(as.numeric(`1d_nat_exp`),na.rm = TRUE))

def_30<-inner_join(intervals_2,Forest_Area,by="name") %>%mutate(t_deforestation=case_when(t_deforestation==0.00  ~ `1990`-`2020`,t_deforestation>0.00 ~ t_deforestation)) %>% mutate(t_deforestation=case_when(t_deforestation<0.00  ~ 0.00,t_deforestation>=0.00 ~ t_deforestation))%>% mutate(year= as.integer((30*`2020`)/t_deforestation))


def_30<-def_30 %>% mutate(percent_change_1=(t_deforestation/`1990`)*100) %>%  mutate(drivers=case_when(percent_change_1<30 ~ "not Drivers",percent_change_1>30  ~"Main Drivers")) %>% mutate(percent_change_1=format(round(percent_change_1,2),nsmall=2))

def_30$name[def_30$name=="Bolivia (Plurinational State of)"]<-"Bolivia"
def_30$name[def_30$name=="Brunei Darussalam"]<-"Brunei"
def_30$name[def_30$name=="Cabo Verde"]<-"Cape Verde"
def_30$name[def_30$name=="Congo"]<-"Democratic Republic of the Congo"
def_30$name[def_30$name=="Côte d'Ivoire"]<-"Ivory Coast"
def_30$name[def_30$name=="French Guyana"]<-"French Guiana"
def_30$name[def_30$name=="Iran (Islamic Republic of)"]<-"Iran"
def_30$name[def_30$name=="Lao People's Democratic Republic"]<-"Laos"
def_30$name[def_30$name=="Micronesia (Federated States of)"]<-"Micronesia"
def_30$name[def_30$name=="Republic of Korea"]<-"South Korea"
def_30$name[def_30$name=="Republic of Moldova"]<-"Moldova"
def_30$name[def_30$name=="Réunion"]<-"Reunion"
def_30$name[def_30$name=="Russian Federation"]<-"Russia"
def_30$name[def_30$name=="Saint Vincent and the Grenadines"]<-"Saint Vincent"
def_30$name[def_30$name=="Syrian Arab Republic"]<-"Syria"
def_30$name[def_30$name=="Trinidad and Tobago"]<-"Trinidad"
def_30$name[def_30$name=="United Kingdom of Great Britain and Northern Ireland"]<-"UK"
def_30$name[def_30$name=="United Republic of Tanzania"]<-"Tanzania"
def_30$name[def_30$name=="United States of America"]<-"USA"
def_30$name[def_30$name=="Venezuela (Bolivarian Republic of)"]<-"Venezuela"
def_30$name[def_30$name=="Viet Nam"]<-"Vietnam"

def_30$code <- countrycode(sourcevar = def_30$name,
                           origin = "country.name",
                           destination = "iso3c")


ghg<- tibble(read_csv("data/cait_emissions_data.csv")) 

gh1<-ghg%>% filter(Gas=="CO2" & Country!="World") %>% filter(Sector=="Total including LUCF")
colnames(gh1)[1]<-"name"
gh1$code <- countrycode(sourcevar = gh1$name,
                        origin = "country.name",
                        destination = "iso3c")

#function for time series prediction of the future values co2
predic_2021<-function(country){
    x<-gh1 %>% filter(name==country) %>% select(-`Data source`,-Sector,-Gas,-Unit,-code) %>% mutate(`1990`=as.double(`1990`)) %>% pivot_longer(cols = -name,names_to = "year",values_to = "emission") %>% arrange(year)
    fit<-arima(ts(x$emission,start = 1990,frequency = 1),order = c(1,2,0))
    predict(fit,n.ahead=5)$pred[3]
}

ghg_pre<-gh1 %>% rowwise() %>% mutate(new_area=as.numeric(predic_2021(name))/2.5)


world <- map_data("world") %>% group_by(region) %>% filter(row_number()==1)
colnames(world)[5]<-"name"
def_30_world<-inner_join(world,def_30,by="name")
trends<- inner_join(world,intervals_1,by="name") %>% inner_join(Forest_Area,by="name") %>% select(name,year,`1d_deforestation`,`1d_expansion`,`1d_afforestation`,`1e_reforestation`,`1d_nat_exp`)
trends$continent <- countrycode(sourcevar = trends$name,
                                origin = "country.name",
                                destination = "continent")
trends$year[trends$year=="2010-2015"]<-"2010-2020"
trends$year[trends$year=="2015-2020"]<-"2010-2020"

trends_def<-trends %>% group_by(year,continent) %>% summarize(t_deforestation =sum(as.numeric(`1d_deforestation`),na.rm = TRUE),t_expansion=sum(as.numeric(`1d_expansion`),na.rm = TRUE),t_afforestation =sum(as.numeric(`1d_afforestation`),na.rm = TRUE),t_reforestation =sum(as.numeric(`1e_reforestation`),na.rm = TRUE),t_nat_exp =sum(as.numeric(`1d_nat_exp`),na.rm = TRUE)) 


# Thorben Data

fra_data_tmp <- tibble(read_csv("data/fao_fra_datasets/fra_forest_disturbance_data.csv"))
temperature_data_tmp <- tibble(read_csv("data/fao_temperature_change_data.csv"))

fra_data <- fra_data_tmp %>%
    mutate(continent = countrycode(sourcevar = iso3, origin = "iso3c", destination = "continent"),
           across(everything(), ~ replace_na(.x, 0)),
           name = factor(name),
           continent = factor(continent)) %>% 
    select(-c(iso3, regions)) %>% 
    rename(Insects = `5a_insect`, 
           Diseases = `5a_diseases`, 
           Weather = `5a_weather`, 
           Others = `5a_other`,
           Fire = `5b_fire_forest`) %>% 
    #rename_with(str_to_title) %>% 
    relocate(name, continent, year, Insects, Diseases, Weather, Others, Fire)

temp_fra <- fra_data_tmp %>%
    mutate(continent = countrycode(sourcevar = iso3, origin = "iso3c", destination = "continent"),
           name = factor(name),
           continent = factor(continent)) %>% 
    select(name, continent, year, `5b_fire_forest`) %>% 
    pivot_wider(names_from = year, values_from =  `5b_fire_forest`) %>% 
    drop_na() %>% 
    pivot_longer(cols = -c(name, continent), names_to = "year", values_to = "Fire") %>% 
    mutate(year = as.double(year))

temperature_data <- temperature_data_tmp %>% 
    filter(Year %in% 2000:2017,
           Flag == "Fc") %>% 
    select(-c(contains("Code"), Flag, Unit))

# function for map
plot_yearly_forest_destruction_by_region <- function(region = 'Global') {
    fra_data %>% 
        filter(name == region | continent == region | region == 'Global') %>% 
        group_by(year) %>% 
        summarise(Insects = sum(Insects, na.rm = TRUE),
                  Diseases = sum(Diseases, na.rm = TRUE),
                  Weather = sum(Weather, na.rm = TRUE),
                  Others = sum(Others, na.rm = TRUE),
                  Fire = sum(Fire, na.rm = TRUE)) %>% 
        pivot_longer(cols = c(Insects, Diseases, Weather, Others, Fire), names_to = "cause", values_to = "area") %>%
        mutate(cause = factor(cause)) %>% 
        ggplot(aes(x=year, y=area, fill= cause %>% fct_reorder(area) %>% fct_relevel("Others", after = Inf))) +
        geom_area() +
        scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2017)) +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_manual(values = c(Weather = "#2b83ba", Others = "#ffffbf", Fire = "#d7191c",
                                     Diseases = "#fdae61", Insects = "#abdda4")) +
        labs(title = "Forest destruction per year and cause",
             subtitle = glue::glue("Region: {region} - Time period: 2000 - 2017"),
             x = "Year",
             y = "Forest Area [1000 ha]",
             fill = "Cause") +
        theme_minimal()
}

#function for cause plot 

plot_total_forest_destruction_by_region <- function(region = 'Global') {
    disturbance_data <- fra_data %>% 
        filter(name == region | region == 'Global') %>% 
        select(-`5b_fire_land`) %>% 
        mutate(Insects = sum(Insects, na.rm = TRUE)) %>% 
        mutate(Diseases = sum(Diseases, na.rm = TRUE)) %>% 
        mutate(Weather = sum(Weather, na.rm = TRUE)) %>% 
        mutate(Others = sum(Others, na.rm = TRUE)) %>% 
        mutate(Fire = sum(Fire, na.rm = TRUE)) %>% 
        pivot_longer(cols = c(Insects, Diseases, Weather, Others, Fire), names_to = "cause", values_to = "area") %>% 
        mutate(cause = factor(cause)) %>% 
        slice(1:5) %>% 
        select(cause, area)
    
    bar_plot <- ggplot(disturbance_data, aes(x = cause %>% fct_reorder(desc(area)) %>% fct_relevel("Others", after = Inf), y = area, fill = cause)) + 
        geom_col() +
        labs(title = "Total forest destruction by cause",
             subtitle = glue::glue("Region: {region} - Time period: 2000 - 2017"),
             x = "",
             y = "Forest Area [1000 ha]") +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_manual(values = c(Weather = "#2b83ba", Others = "#ffffbf", Fire = "#d7191c",
                                     Diseases = "#fdae61", Insects = "#abdda4")) +
        theme_minimal() +
        guides(fill = FALSE)
    
    pie_plot <- ggplot(disturbance_data, aes(x="", y = area, fill = cause %>% fct_reorder(desc(area)) %>% fct_relevel("Others", after = Inf))) + 
        geom_bar(stat="identity", width=1) +
        coord_polar("y") +
        scale_fill_manual(values = c(Weather = "#2b83ba", Others = "#ffffbf", Fire = "#d7191c",
                                     Diseases = "#fdae61", Insects = "#abdda4")) +  
        labs(x = "",
             y = "Forest Area [1000 ha]",
             fill = "Cause") +
        theme_void()
    
    bar_plot + pie_plot
}

# function for countires effected 

plot_highest_forest_destruction <- function(cont = 'Global', number = 15, cs = 'All') {
    num_text <- number
    number <- as.integer(number)
    if (cs != "All") {
        number <- number / 5
    }
    fra_data %>% 
        filter(continent == cont | cont == 'Global') %>% 
        select(name, c(Insects, Diseases, Weather, Others, Fire)) %>% 
        group_by(name) %>% 
        summarise(Insects = sum(Insects, na.rm = TRUE),
                  Diseases = sum(Diseases, na.rm = TRUE),
                  Weather = sum(Weather, na.rm = TRUE),
                  Others = sum(Others, na.rm = TRUE),
                  Fire = sum(Fire, na.rm = TRUE)) %>% 
        ungroup() %>% 
        pivot_longer(cols = c(Insects, Diseases, Weather, Others, Fire), names_to = "cause", values_to = "area") %>% 
        filter(cause == cs | cs == 'All') %>% 
        group_by(name) %>% 
        mutate(total = sum(area, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(desc(total)) %>% 
        slice(1:(5*number)) %>% 
        ggplot(aes(x=area, y=reorder(name, area), fill=reorder(cause, desc(area)) %>% fct_relevel("Others", after = Inf))) +
        geom_col() +
        scale_x_continuous(labels = scales::comma) +
        scale_fill_manual(values = c(Weather = "#2b83ba", Others = "#ffffbf", Fire = "#d7191c",
                                     Diseases = "#fdae61", Insects = "#abdda4")) + 
        labs(title = glue::glue("The {num_text} Countries with the highest forest destruction"),
             subtitle = glue::glue("Region: {cont} - Time period: 2000 - 2017"),
             x = "Forest Area [1000 ha]",
             y = NULL,
             fill = "Cause") +
        theme_minimal()
    
}


#wildfire ------------
global_yearly_tc <- temperature_data %>% 
    filter(Area == 'World', 
           Element == 'Temperature change', 
           Months == 'Meteorological year'
    ) %>% 
    select(Year, Value) %>% 
    rename(year = Year, temp_increase = Value)

global_yearly_tc

fra_tc_global <- inner_join(temp_fra, global_yearly_tc, by=c("year"))

local_yearly_tc <- temperature_data %>% 
    filter(Element == 'Temperature change', 
           Months == 'Meteorological year') %>% 
    select(Area, Year, Value) %>% 
    rename(name = Area, year = Year, temp_increase = Value)

fra_tc_local <- inner_join(temp_fra, local_yearly_tc, by=c("name", "year"))

#-------------------from here new edit data

forest_continents <- read_csv("data/fao_forest_datasets/fao_continents_data.csv", col_types = cols())

# change column names
colnames(forest_continents)[colnames(forest_continents) == "Value"] <- "ForestSize"
colnames(forest_continents)[colnames(forest_continents) == "Area"] <- "Continent"

newData <- forest_continents %>%
    drop_na() %>% 
    select(c("Continent","Year","ForestSize")) %>% 
    group_by(Continent, Year) %>% 
    arrange(Year) %>% 
    mutate(AvgforestSize = mean(ForestSize))
#----------------------------

fra <- tibble(read_csv("data/fao_fra_datasets/fra_further_topics_data.csv"))
tmp <- fra %>% 
    select(iso3, name, year, `1a_forestArea`) %>% 
    filter(year == 1990 | year == 2020) %>% 
    pivot_wider(names_from = year, values_from = `1a_forestArea`) %>% 
    mutate(diff = `2020` - `1990`)

l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
    showframe = FALSE,
    showcoastlines = FALSE
)

limit <- max(abs(tmp$diff)) * c(-1, 1)
#------------------------------------------
forest_countries <- read_csv("data/fao_forest_datasets/fao_forest_area_data.csv", col_types = cols())

colnames(forest_countries)[colnames(forest_countries) == "Value"] <- "ForestArea"
colnames(forest_countries)[colnames(forest_countries) == "Area"] <- "Country"
forest_countries <- forest_countries %>% subset(Country != "USSR")
forest_countries <- forest_countries %>% subset(Country != "China, mainland")

top10 <- forest_countries %>% 
    filter(Year == 2020) %>% 
    drop_na() %>% 
    arrange(desc(ForestArea)) %>%
    top_n(10) %>% 
    select(Country, ForestArea)

rest <- forest_countries %>% 
    filter(Year == 2020) %>% 
    drop_na() %>% 
    arrange(desc(ForestArea)) %>%
    slice(11:n()) %>% 
    summarise(Country = "Other Countries",
              ForestArea = sum(ForestArea))

top_data <- bind_rows(top10, rest) %>% mutate(Country = factor(Country))
#-----------------------------------
l <- list(color = toRGB("grey"), width = 0.5)

g2 <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    showocean=TRUE, oceancolor="LightBlue"
)
#------------------------------------
l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
    showframe = FALSE,
    showcoastlines = FALSE
)


#------------------------------
reforestation <- intervals %>% 
    select(name, year, `1d_afforestation`, `1e_reforestation`) %>% 
    mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
    mutate(`1e_reforestation` = case_when(
        year == c("1990-2000", "2000-2010") ~ `1e_reforestation` * 10,
        TRUE ~ `1e_reforestation` * 5
    ),
    `1d_afforestation` = case_when(
        year == c("1990-2000", "2000-2010") ~ `1d_afforestation` * 10,
        TRUE ~ `1d_afforestation` * 5
    )) %>% 
    mutate(sum = `1d_afforestation` + `1e_reforestation`) 

total_reforestation <- reforestation %>%  
    group_by(name) %>% 
    summarise(totalref = sum(sum, na.rm = TRUE)) %>% 
    ungroup()  %>%
    arrange(desc(totalref))


refplot <- total_reforestation  %>%
    head(20)

rest_ref <- total_reforestation %>% 
    slice(21:n()) %>% 
    summarise(name = "Other Countries",
              totalref = sum(totalref))

ref_data <- bind_rows(refplot, rest_ref) %>% mutate(name = factor(name))

area <- fra %>%
    group_by(name) %>%
    filter(year == 1990) %>%
    select(name, `1a_forestArea`)

increase <- total_reforestation %>%
    inner_join(area, by = "name") %>%
    mutate(reforestation_increase = (totalref/`1a_forestArea`) * 100) %>%
    arrange(desc(reforestation_increase))

increase$code <- countrycode(increase$name, origin = "country.name", destination = "iso3c")

#---------------------------

deforestation <- intervals %>% 
    select(name, year, `1d_deforestation`) %>% 
    mutate(`1d_deforestation` = case_when(
        year == c("1990-2000", "2000-2010") ~ `1d_deforestation` * 10,
        TRUE ~ `1d_deforestation` * 5
    )) %>% 
    group_by(name) %>% 
    summarise(totaldef = sum(`1d_deforestation`, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(desc(totaldef))



corref <- total_reforestation %>%
    inner_join(deforestation, by = "name") %>%
    select(name,totalref, totaldef)

#----------------------------
world <- map_data("world") %>% 
    group_by(region) %>% 
    filter(row_number()==1)
colnames(world)[5]<-"name"

def_30_world<-inner_join(world,def_30,by="name")
trends<- inner_join(world,intervals_1,by="name") %>% inner_join(Forest_Area,by="name") %>% select(name,year,`1d_deforestation`,`1d_expansion`,`1d_afforestation`,`1e_reforestation`,`1d_nat_exp`)
trends$continent <- countrycode(sourcevar = trends$name,
                                origin = "country.name",
                                destination = "continent")
trends$year[trends$year=="2010-2015"]<-"2010-2020"
trends$year[trends$year=="2015-2020"]<-"2010-2020"

trends$year[trends$year=="1990-2000"]<-0
trends$year[trends$year=="2000-2010"]<-1
trends$year[trends$year=="2010-2020"]<-2

trends_def<-trends %>% group_by(year,continent) %>% summarize(t_deforestation =sum(as.numeric(`1d_deforestation`),na.rm = TRUE),t_expansion=sum(as.numeric(`1d_expansion`),na.rm = TRUE),t_afforestation =sum(as.numeric(`1d_afforestation`),na.rm = TRUE),t_reforestation =sum(as.numeric(`1e_reforestation`),na.rm = TRUE),t_nat_exp =sum(as.numeric(`1d_nat_exp`),na.rm = TRUE)) %>% ungroup()

#----------------------------------
trends<- inner_join(world,reforestation,by="name") %>% 
    select(name,year,sum)
trends$continent <- countrycode(sourcevar = trends$name,
                                origin = "country.name",
                                destination = "continent")
trends$year[trends$year=="2010-2015"]<-"2010-2020"
trends$year[trends$year=="2015-2020"]<-"2010-2020"

trends$year[trends$year=="1990-2000"]<-0
trends$year[trends$year=="2000-2010"]<-1
trends$year[trends$year=="2010-2020"]<-2

trends_ref<-trends %>% 
    group_by(year,continent) %>% summarize(t_reforestation =sum(sum,na.rm = TRUE)) %>% ungroup()
#-------------------------------------------

AirPollution <- tibble(read_csv("data/oecd_air_pollution_data.csv"))
colnames(AirPollution)[colnames(AirPollution) == "Value"] <- "AirPollution"
AirPollution <- AirPollution %>% 
    select(c("Country", "Year","AirPollution")) %>% 
    group_by(Country,Year)

AirPollution_India <- AirPollution %>% 
    group_by(Year) %>% 
    filter(Country == "India") %>% 
    summarize(AirPollution = mean(AirPollution)) %>% 
    ungroup() 


pollution_global <- AirPollution %>% 
    group_by(Country,Year) %>% 
    drop_na() %>% 
    group_by(Year) %>% 
    summarise(Country = "Other Countries",
              AirPollution = mean(AirPollution))

AirPollution_India_plot <- AirPollution_India %>% 
    ggplot(aes(x = Year, y = AirPollution))+
    geom_point(color =  "#79198a", size = 3, alpha = 0.6)+
    geom_line(size = 1, alpha = 0.6)+
    geom_point(aes(x = pollution_global$Year,  y = pollution_global$AirPollution), color =  "#22228a", size = 3, alpha = 0.6)+
    geom_line(aes(x = pollution_global$Year,  y = pollution_global$AirPollution), size = 1, alpha = 0.6)+
    labs(title = "Air pollution trends in India",
         subtitle = "Time period: 1990-2019",
         x = "Year",
         y = expression(paste("Air pollution [", mu, "g/", m^3, "]")))+
    theme_minimal()+
    theme(axis.title = element_text(),text = element_text(family = "Arial"))+
    theme(plot.subtitle = element_text(size = rel(0.8)))
#------------------------------
AirPollution_India_1 <- AirPollution %>% 
    group_by(Year) %>% 
    filter(Country == "India") %>% 
    summarize(Country = "India", AirPollution = mean(AirPollution)) %>% 
    ungroup() 



pollution_rest <- AirPollution %>% 
    drop_na() %>% 
    filter(Country != "India") %>% 
    summarise(Country = "Other Countries",
              AirPollution = mean(AirPollution))

trend <- bind_rows(AirPollution_India_1, pollution_rest) %>% mutate(Country = factor(Country))

trend_plot <- plot_ly(trend, x = ~AirPollution, y = ~factor(Country)) %>%
    add_boxplot(color = ~Country) %>%
    layout(title = "Air pollution in India and the rest of the World (1990-2020)",
           yaxis = list(title = "") , xaxis = list(title = "Air pollution"))

mergedData <- merge(forest_countries, AirPollution, by=c("Country","Year")) %>%
    select(c('Country','Year','ForestArea','AirPollution')) %>% 
    drop_na() %>% 
    group_by(Year) %>% 
    summarize(TotalForestArea = sum(ForestArea), TotalAirPollution = sum(AirPollution)) %>% 
    ungroup() %>% 
    unique()

mergedData_plot<-mergedData %>% 
    ggplot(aes(x = TotalForestArea, y = TotalAirPollution)) +
    geom_point(size = 4, alpha=0.7, color = "#056b5f") +
    labs(x="Total Forest Area [1000 ha]", y = "Total Air Pollution", title="Correlation between Forest Area and Air Pollution (globally)") + 
    geom_smooth(method = 'lm')+
    ggrepel::geom_text_repel(
        aes(label = Year), 
        color = "black", size = 7/.pt)+
    scale_x_continuous(labels = scales::comma)+
    scale_y_continuous(labels = scales::comma)
#------------------------------------------
emissionData <- read_csv("data/cait_emissions_data.csv", col_types = cols())
colnames(emissionData)[colnames(emissionData) == "Country"] <- "country" 
colnames(emissionData)[colnames(emissionData) == "Data source"] <- "DataSource" 
emissionData$`1990` <- as.numeric(emissionData$`1990`)

emissionData <- emissionData %>%
    drop_na()%>%
    select(-DataSource, -Sector, -Unit)%>%
    pivot_longer(!c(country,Gas), names_to = "year", values_to = "emissionValue")%>%
    filter(country!='World' & country != "European Union (27)")

emissionData_plot<-emissionData%>%
    group_by(country)%>%
    summarise(avgEmission = mean(emissionValue))%>%
    arrange(desc(avgEmission))%>%
    top_n(20)%>%
    ggplot(aes(x=avgEmission, y=reorder(country, avgEmission))) + 
    geom_col(fill="red") + 
    labs(title='Top 20 emitters of GreenHouse gases (1990 - 2018)',
         x = 'Average Emissions [1000 tonnes]', y = NULL)+
    theme(legend.position = "none")+
    scale_x_continuous(labels = scales::comma)

mycols <- c("##009999", "#EFC000FF", "#868686FF", "#CD534CFF")
pieData <- emissionData%>%
    group_by(Gas)%>%
    summarise(Value = sum(emissionValue))%>%
    filter(Gas !="All GHG")%>%
    mutate(percentvalue = round(Value / sum(Value)*100), digits=0)%>%
    arrange(percentvalue)
pie <- ggplot(data = pieData, aes(x = "", y = percentvalue, fill = Gas)) +
    geom_col(color='black') +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(percentvalue, "%")), position = position_stack(vjust = 0.5))+
    theme(panel.background = element_blank(),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 18))+
    ggtitle("Proportion of GHG gases") 

dataAggregate <- aggregate(emissionValue ~ country, emissionData, mean)
colnames(dataAggregate)[colnames(dataAggregate) == "emissionValue"] <- "Average Emission Value [MtCO2e] per Country from 1990 to 2020"
worldmaps <- joinCountryData2Map(dataAggregate, nameJoinColumn = "country", joinCode = "NAME", verbose = FALSE)

colourPalette <- RColorBrewer::brewer.pal(9, 'Reds')

#----------------------------
data <- tibble(read.csv("data/fao_fra_datasets/fra_carbon_stock_data.csv"))
data$X1990 <- as.numeric(data$X1990)
data <- data[which(rowMeans(is.na(data))<0.5), ]
impute_data <- mice(data, m=5, maxit = 5, seed = 100)
clean_data <- complete(impute_data, 2)
clean_data <- group_by(clean_data, country)
clean_data <- mutate(clean_data, dataMean = mean(c(X1990,X2000,X2010,X2015,X2016,X2017,X2018,X2019,X2020)))

clean_data <- cbind(clean_data, replicate (22, clean_data$dataMean),simplify = FALSE)
clean_data <- setnames(clean_data, old = c("...12","...13","...14","...15","...16","...17","...18","...19","...20",
                                           "...21","...22","...23","...24","...25","...26","...27","...28","...29",
                                           "...30","...31","...32","...33"),
                       new = c("X1991","X1992","X1993","X1994","X1995","X1996","X1997","X1998","X1999","X2001","X2002",
                               "X2003","X2004","X2005",
                               "X2006","X2007","X2008","X2009","X2011","X2012","X2013","X2014"), skip_absent = TRUE)
clean_data <- select(clean_data,'country',c("X1990","X1991","X1992","X1993","X1994","X1995","X1996","X1997",
                                            "X1998","X1999","X2001","X2002","X2003","X2004","X2005","X2006",
                                            "X2007","X2008","X2009","X2011","X2012","X2013","X2014","X2015","X2016",
                                            "X2017","X2018","X2019","X2020"))
carbonStockData <- pivot_longer(clean_data, X1990:X2020, names_to = 'year', values_to = 'carbonStockValue')
carbonStockData <-separate(carbonStockData, col=year, into = c("y","year"), sep="X")
carbonStockData$y <- NULL


forestArea <- read_csv("data/fao_forest_datasets/fao_forest_area_data.csv", col_types = cols())
colnames(forestArea)[colnames(forestArea) == "Area"] <- "country"
colnames(forestArea)[colnames(forestArea) == "Value"] <- "Area"
colnames(forestArea)[colnames(forestArea) == "Year"] <- "year"
forestArea <- select(forestArea, c('country', 'year','Area'))

#merging carbonstock dataset with Forest Area dataset
carbonStockData <- merge(carbonStockData, forestArea, by=c("country","year"))
carbonStockData <- mutate(carbonStockData, carbonInTonnes = (carbonStockValue * Area))

#------------------------------------
mergedData_1 <- merge(carbonStockData, emissionData, by=c("country","year"))%>%
    select(c('year', 'country','carbonInTonnes','emissionValue'))%>%
    drop_na()%>%
    group_by(year)%>%
    summarize(carbonStockSum = mean(carbonInTonnes), emissionSum = mean(emissionValue))%>%
    unique()

mergedData_1_plot<-mergedData_1%>%
    ggplot(aes(year, carbonStockSum, group=1)) + geom_line() + 
    geom_point(aes(colour = "red")) + 
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
    scale_y_continuous(labels = scales::comma) + 
    labs(title="Carbon Stock trend from 1990 to 2018", x = "Year", y = "Carbon Stock [1000 tonnes]") +
    theme(legend.position = "none")

mergedData_2_plot<-mergedData_1%>%
    ggplot(aes(year, emissionSum, group=1)) + geom_line() + 
    geom_point(aes(colour = "red")) + 
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
    scale_y_continuous(labels = scales::comma) + 
    labs(title="Trend of emitted GHG from 1990 to 2018", x = "Year", y = "GHG emitted [1000 tonnes]") +
    theme(legend.position = "none")
mergedData_3_plot<-carbonStockData %>%
    drop_na()%>%
    group_by(country)%>%
    summarise(avgCarbon = mean(carbonInTonnes))%>%
    arrange(desc(avgCarbon))%>%
    top_n(20)%>%
    ggplot(aes(x=avgCarbon, y=reorder(country, avgCarbon))) + geom_col(fill='limegreen') + labs(title='Largest carbon stock in forests (1990 - 2020)', x = 'Average Carbon Stock [1000 tonnes]', y = NULL) + scale_color_viridis_c(option = "inferno") + theme(legend.position = "none")+scale_x_continuous(labels = scales::comma)

mergedData_4_plot<-mergedData_1%>%
    ggplot(aes(x=emissionSum, y=carbonStockSum)) + geom_point(color="red") +
    labs(x="GHG emissions [1000 tonnes]", y = "Sum of carbon stock in forests [1000 tonnes]", title="GHG emissions against carbon stock in forests, per year") +
    geom_smooth(method = 'lm', fill="darkolivegreen2") +
    scale_y_continuous(labels = scales::comma)
#--------------
defTop <- def_30 %>%
    arrange(desc(year)) %>%
    head(20)

defBot <- def_30 %>%
    drop_na() %>%
    filter(year <= 50) %>% 
    arrange(year)
#------------
p <- fra_tc_local %>% 
    filter(!is.na(Fire)) %>% 
    mutate(year = as.integer(year)) %>% 
    ggplot(aes(y=Fire, x=temp_increase, color = continent)) +
    geom_point(alpha = 0.5, show.legend = FALSE) +
    # geom_smooth(se = FALSE) +
    facet_wrap(~continent) +
    scale_y_continuous(labels = scales::comma, limits = c(0,100)) +
    theme_minimal() +
    labs(title = "Relation between temperature increase and\narea of wildfires per year and continent",
         subtitle = "Year: {frame_time}",
         x = "Temperature increase [C°]",
         y = "Wildfire area [1000 ha]",
         color = "Continent") +
    transition_time(year) +
    ease_aes('linear')
#---------------------------
fa_country_avg <- forest_countries %>% 
    group_by(Country) %>% 
    summarise(ForestArea = mean(ForestArea)) %>% 
    ungroup()

tmp_1 <- tibble(read_csv("data/aquastat.csv"))
aquastat <- tmp_1 %>% filter(Year == 2017) %>% select(Area, `Variable Name`, Value) %>% pivot_wider(names_from = `Variable Name`, values_from = Value) %>% select(Area, `Long-term average annual precipitation in volume`, `Total renewable water resources`)
aquastat

water_forest_data <- inner_join(fa_country_avg, aquastat, by = c("Country" = "Area"))

#-----------------------
data_1 <- read_csv("data/fao_fra_datasets/fra_carbon_stock_data.csv", col_types = cols())
forestArea_1 <- read_csv("data/fao_forest_datasets/fao_forest_area_data.csv", col_types = cols())
colnames(forestArea_1)[colnames(forestArea_1) == "Area"] <- "country"
colnames(forestArea_1)[colnames(forestArea_1) == "Value"] <- "Area"

data_1 <- data_1 %>%
    pivot_longer(c("1990","2000","2010","2015","2016","2017","2018","2019","2020"), names_to = 'Year', values_to = 'carbonStock')%>%
    merge(forestArea_1, by=c("country","Year"))%>%
    select(c("country","Year","Area","carbonStock"))%>%
    mice(m=5, maxit = 5, seed = 100, printFlag = FALSE)%>%
    complete(2)%>%
    group_by(Year)%>%summarise(totalArea = sum(Area), totalCarbonAbsorbed = sum(carbonStock))

#--------------------------

temp_tree <- temperature_data %>% 
    filter(Element == 'Temperature change', 
           Months == 'Meteorological year') %>% 
    select(Area, Year, Value) %>% 
    rename(name = Area, year = Year, temp_increase = Value)

# Interpretation of NA's as No Wild Fire, because of the structure of the entries for this column
fra <- fra_data %>% 
    mutate(wildfire = case_when(
        Fire == 0 ~ "No",
        TRUE ~ "Yes"
    )) %>% 
    mutate(wildfire = factor(wildfire)) 

fra_tc_data <- inner_join(fra, temp_tree, by=c("name", "year"))
fra_tc_data

set.seed(123)

# Specify data splits 
init_split <- initial_split(fra_tc_data, strata = wildfire)
wf_training <- training(init_split)
wf_testing <- testing(init_split)

# 5-fold cross validation
wf_cv <- vfold_cv(wf_training, v = 5)



# Decision Tree
dt_model_spec <- decision_tree() %>% 
    set_engine("rpart") %>% 
    set_mode("classification")


# Fit models on cv training data
wf <- workflow() %>%
    add_formula(wildfire ~ temp_increase) %>% 
    add_model(dt_model_spec)

dt_results_1 <- wf %>% 
    fit_resamples(wf_cv) %>% 
    collect_metrics()


wf <- wf %>% update_formula(wildfire ~ name + continent + temp_increase + boreal + temperate+ tropical+ subtropical + Insects + Diseases + Weather + Others + year
)

dt_results_2 <- wf %>% 
    fit_resamples(wf_cv) %>% 
    collect_metrics()

dt_results_2

dt_fit <- fit(dt_model_spec, wildfire ~ name + continent + temp_increase + boreal + temperate+ tropical+ subtropical + Insects + Diseases + Weather + Others + year, wf_training)

dt_fit$fit

predict(dt_fit, new_data = wf_testing) %>%
    mutate(truth = wf_testing$wildfire) %>%
    accuracy(estimate = .pred_class, truth = truth)

dt_fit <- fit(dt_model_spec, wildfire ~ temp_increase + boreal + temperate + tropical+ subtropical + Insects + Diseases + Weather + Others + year, wf_training)
rpart.plot::rpart.plot(dt_fit$fit,roundint = FALSE)

predict(dt_fit, new_data = wf_testing) %>%
    mutate(truth = wf_testing$wildfire) %>%
    accuracy(estimate = .pred_class, truth = truth)

motivation1<-"The existence of forests is essential for our life on Earth. By covering around 31 percent of the world’s total land area, forests provide a retreat and home to over 80 percent of land animals and countless partially even undiscovered plants. One can say that forests are the backbone of entire ecosystems. A significant part of the oxygen we breathe is provided by the trees, while they also absorb about 25 percent of greenhouse gases. Also economically we are dependent on forests as the livelihoods of about 1.6 billion people around the world are directly or indirectly connected to forests. Furthermore, forests provide 40 percent of today’s global renewable energy supply, as much as solar, hydroelectric and wind power combined. Despite these utilities, forestation across the world has faced several challenges ranging from wildfire, human-driven deforestation, poor management and poor conservation in general. However, a loss of whole forests would mean severe consequences to humanity and life on Earth."

motivation2<-"With this project we seek to answer important questions that address these challenges. We want to figure out the causes of destruction of forests, highlight their importance to our environment and predict trends around reforestation/deforestation. Moreover, we hope to show how we can tackle climate change by reforestation, in particular, how an increase in forest area will help to increase the buffer of sustainability. For the statistics so far, see our reference (UN 2019)."


Deforestation<-"In our analysis we are now attempting to answer important questions regarding deforestation, which is actually a part of forest destruction. However we want to highlight this issue in an own chapter as it is directly made by humans. Therefore we will have a look on the main drivers (by countries) of deforestation, showing trends over the continents and making a prediction of how many years it would take until all forests are lost by putting the deforestation and reforestation values over the last 30 years into relation."

drivers_deforestation_1<-"By hovering over the following map you can find the data for every country regarding the percentage of forest lost through deforestation in the last 30 years in relation to 1990, the value for deforestation in the last 30 years, the current forest area [1000 ha] and the number of years in which the forest area will be completely lost, given the deforestation for each country in the last 30 years."

drivers_deforestation_2<-"The color scale is showing the percentage of forest lost, which is making the main drivers of deforestation visible."

required_forest<-"The map below highlights the countries which have to increase the forest area the most to tackle their current CO2 emissions. As our dataset only provides C02 emission data from 1990-2018, we used the Arima model time series prediction to predict future CO2 emissions. By taking into account that one acre of forest can absorb about 2.5 tons of carbon annually, the predicted CO2 value is used to find the required amount of additional forest area."

Deforestation_trends<-"The chart below shows trends in the deforestation over the last thirty years over 5 continents. Unfortunately, our dataset only provides intervals in 5 respectively 10 year steps, which is why it is not possible to show a more precise trend line."

destruction<-"On a global scale wildfires are clearly the dominant cause of forest destruction over the years. However, this does not apply for every individual country. For example on next tab: Germany’s main cause of forest destruction are insects. One can also see that there is no obvious trend to find over this small time scale."

destruction_cause<-"Again we can see the huge impact of wildfires. On a global scale they make up more than 50% of forest destruction, destroying more than 1 Billion ha of forest. Insects make up almost 25% of forest destruction, destroying roughly 500 Million ha of forest. These two are clearly the main drivers of global forest destruction."

destruction_cause_1<-"In Germany insects and diseases are the main drivers of forest destruction, destroying approximately 2.5 Million ha of forest over this 18 years time period. However, wildfires are no significant problem at all in Germany."

countires_affected<-"As one would expect from our previous findings, the most affected countries mostly struggle with wildfires. The only exceptions to this are the USA, Canada, China, Sudan and Mexico. Brazil is the most affected country and has a huge wildfire problem. However, since Brazil is a tropical region and therefore very humid these wildfires are most likely caused by humans (ZEIT 2021)"

wildfires<-"Our first visualization doesn’t suggest any linear correlation, but it can be improved to make the interpretation clearer. Again, the temperature increase given in this dataset is in relation to a baseline temperature which corresponds to the period of 1951–1980."

final_analysis_1<-"Final Analysis The bottom line on what we’ve learned from our data is, that forests change incredibly fast. Even in our given time range from the last 30 years we gained many fascinating findings. In general we answered the questions with exploratory data analyses, regression models and correlation. To justify our results, we tried to have a view on our data from at least two sides. In these cases, we were thus able to reinforce a previous result with a further calculation or even visualization (e.g. continental and country-wise results)."

final_analysis_2<-"Starting with the forest area trends we detected that there are huge differences between countries and even continents when it comes to the development of forest area. While the Americas (in particular South America) and Africa faced a decrease Asian countries could slightly increase the forest area. Main reason for that was the extensive deforestation in Brazil with 19% while China recorded a way higher number in forest area 2020 than in 1990. The forest trend in Europe in the last 30 years is stable, it is even slightly upwards. A new perspective on the world is providing our map with the deforestation analysis. It shows the loss of forests in the past 30 years and furthermore the years until the forests are completely lost, if a country keeps on deforesting with the same pace. However, the numbers should be seen with the understanding that the amount deforestation is declining on almost every continent. For example countries in Americas deforested between 2000-2010 around 70 million ha, the figure declined between 2010-2020 to 31 million ha. When trying to put the deforestation results into relation with the reforestation we found a moderate correlation, showing that deforestation has actually an impact on reforestation. One could argue that at least some countries want to „make up“ for their human-driven deforestation. Interesting would be also to put the reforestation numbers into relation to other forest destruction causes, to show which countries are willing to tackle climate change. When it comes to natural forest destruction we figured out that wildfires are with 50% share clearly the dominant cause, however it varies between countries, e.g. in Germany is the main cause of forest destruction insects, in Brazil fires. We have to mention here, that our dataset does not distinguish between natural caused and human-driven fires. Based on recent articles and that Brazil is located in a tropical zone, we can assume that it is a human-driven deforestation. That is also one reason why our prediction of when and where wildfires are likely to occur is quite tough to answer. A more precise dataset, distinguishing between the causes of fire is missing. Finally we investigated in another large subject area, forests and the relation to other environmental issues. We were able to calculate the decrease in CO2 (0.0025 MtCO2) if forest area is increased by 1000 ha and furthermore we were able to determine how much forest area each country would have to plant, to be able to absorb their C02 emissions."

final_analysis_3<-"Since articles and papers deal very specifically with the individual topics around forests, we wanted to give an overview or a summary with our own data and calculations about the most important issues from the beginning of our project. We are aware that we could not cover everything and would have liked to answer many more questions that came to our mind before or during the project. However, we noticed that this would have been beyond the scope of our project. A look into the future is also intriguing. Besides to observe whether current positive or negative trends are holding on, with more years of collecting data, it would open completely new possibilities to answer further questions and make even more precise predictions."

global_forest_trends<-"Starting with the current status of our global forests, the trend line of the global forest area over the last 30 years gives an overview how the forest area of continents developed in the last 30 years. One can see a decrease in the Americas and Africa while Asia recorded a slight increase in forest area. The forest area in Europe as well in Oceania (containing Australia) seems to be quite stable."
global_forest_trends_1<-"The following map is visualizing the change in forest area from 1990 - 2020 for each country (by hovering over it). It underlines our previous results. While Brazil and therefore South America in general faces worldwide the highest decrease in forest area, China and other Asian countries report a higher value in 2020 compared to 1990."
global_forest_trends_2<-"The results are not really surprising as those countries have also the highest surface worldwide. However, if you take for example Russia’s forest area into relation with the forest area of the rest of the world, outside the top ten countries, it shows quite impressive results. Besides that the top 10 countries have in sum more forest area than the rest of the world together."

reforestation<-"This leads us to quite surprising results, e.g. that Algeria is the country with the highest reforestation increase given the total forest area in 1990. Nevertheless this doesn’t come out of nowhere, along with other North African countries, Algeria is pursuing several reforestation projects such as the great green wall or “barrage vert” (Göbel 2021). This results in the fact, that Algeria is one of the countries which has a higher forest cover in 2020 than in 1990."

ref_def_1<-"After we have now an overview on reforestation and deforestation we want to answer the question whether governments try to “make up” for the deforestation in the last 30 years."
ref_def_2<-"First of all we want to show the relation of total reforestation and deforestation in the last 30 years, to get a first impression."
ref_def_3<-"There are many outliers with either very high deforestation or reforestation figures. However, such outliers are not surprising when taking a look into recent news. For example Brazil, a country with one of the biggest rainforest areas, has been making negative headlines for years with it’s environment politics (ZEIT 2021)."
ref_def_4<-"We now zoom in to have a closer look on the data without the outliers by downsizing the scale."
ref_def_4a<-"The figure shows already a high and not linear distribution of our data. With the Shapiro-Wilk test we want to show the normality of our data."
ref_sper<-"##  Shapiro-Wilk normality test
## 
## data:  corref$totalref
## W = 0.21089, p-value < 0.00000000000000022"
ref_sper_1<-"##  Shapiro-Wilk normality test
## 
## data:  corref$totaldef
## W = 0.16293, p-value < 0.00000000000000022"
ref_def_4b<-"The values are below 0.05 for both, reforestation and deforestation, the data significantly deviate from a normal distribution. A result which is already highlighted by the graph."
ref_def_4c<-"As the data is therefore not linear, we should choose the Spearman method to calculate the correlation."
ref_def_4d<-"With a value of 0.54 it shows a strong positive correlation, which means, that deforestation has actually an impact on reforestation and a relationship exists.

Additionally, we calculated the power predictive score to calculate the impact of deforestation on reforestation."


ref_def_5<-"We are going to take a look on the trend line of the continents and their given total reforestation in the given time intervals of 10 years between 1990 and 2020."
ref_def_6<-"We are going to take a look on the trend line of the continents and their given total deforestation in the given time intervals of 10 years between 1990 and 2020."
ref_def_7<-"One can notice, that reforestation is increasing on many continents, while in the deforestation part we could detect a decrease of deforestation in some continents. It would be very interesting to do another correlation analysis in a few years. The trend seems to be clearly going in the direction of a higher correlation that deforestation has an impact on reforestation, at least for some continents and countries."
ghg_emission<-"To start with we visualized the top 20 emitters of GHG globally, within the time period 1990-2018. The result below shows China at the top, a country which (as we have already shown) has one of the highest forest area increases between 1990-2020."
carbon_stock<-"The next important variable is the carbon stock hold globally, which is here visualized in a trend line for the period 1990-2018. To put it already into relation, we show the visualization for the GHG trend right under it. One can already notice that the carbon stock has especially in the 1990s a sharp decrease, while the GHG trend is increasing strongly since 1999."
carbon_stock_1<-"Before we will calculate the correlation we want first to have a look on the countries with the largest carbon stock. Not surprisingly we find also here countries with the highest number of forest area."
carbon_stock_2<-"We decided to use the Kendall method to calculate the correlation, because the data is small and non-normally distributed. We find a very strong negative correlation with -0.95. This results in the interpretation, that a decreasing carbon stock leads to increasing GHG emissions. However, this relationship can not be ascertained using correlation because correlation does not necessarily imply causation."
Forest_lost<-"The two following bar plots are intended to illustrate the result once again. First, the top 20 countries which do not seem to have a problem with the current level of deforestation given their forest area and then the countries which could lose their forest area within the next 50 years due to too much deforestation."
Forest_lost_1<-"Countries that are actually located in the subtropical zone with a lot of rainforest, such as Nicaragua, Uganda or Ivory Coast should be highlighted here. Since we were also wondering why Brazil is missing in this list (given recent reports), we noticed later in our analysis, that the area of burned forests are extraordinary high. Assuming that the burned forest area belongs to the cause of deforestation and not wildfires, Brazil would lose it’s forests far earlier: in 20 instead of 133 years."
wildfires_1<-"Looking at the plot, there seems to be a correlation between these two variables. To quantify these results, we now calculate a correlation score. Since we have only 18 observations, we will use the Kendall correlation coefficient."
wildfires_2<-"The results show that there is a moderate positive correlation (0.4640523) between global temperature increase and wildfire area."
wildfires_3<-"Now we also take a look at the relation between global yearly temperature changes and the global count of countries with at least one wildfires in a given year."
wildfires_4<-"There seems to be no linear correlation visible for both land and forest fires and rising temperatures. Again we calculate the Kendall correlation coefficient."
wildfires_5<-"As we expected, there is no correlation between global yearly temperature changes and the global count of countries with at least one wildfires in a given year."
wildfires_6<-"Finally we go more into detail and show not only global values but values for each country and each year:"
wildfires_7<-"When analyzing the values for all the countries and also sorting them continent-wise, there seems to be no clear correlation. The points have no overall tendency to move linear and their movement in general looks chaotic. This is a quite interesting result: on a global averaged scale a moderate correlation between temperature increase and wildfires exists while when looking at all the individual values of the countries, this does not seem to be the case."
air_poll<-"The air pollution drastically increased after the year 2005, reaching its peak in 2014. After 2017 the air pollution decreased again and stagnated around a value of 83 μg/m3. Compared to the global average India has immense air pollution levels throughout the years. Globally the air pollution stays more or less the same or even slightly decreases."
air_poll_1<-"We notice that India’s air pollution range lies between 65 μg/m3 (least) and 95 μg/m3 (highest), while the range of average air pollution for other countries lies between 5 μg/m3 and 66 μg/m3. However, a lot of outliers in the global average can be found, showing that a lot of countries have air pollution ranging above the maximum average value."
air_poll_2<-"Furthermore we try to find a relationship between the forest area and air pollution. The assumption is confirmed by the correlation value that there is no relationship here."
water_1<-"Finally we want to take a look at two relationships between forest area and water. We decided to go for a global instead of a country-wise analysis to keep this chapter consistent. First we want to find out if there is a correlation between long-term average annual precipitation and the average forest area between 1990 - 2020."
water_2<-"Looking at the plot above, there seems to be a clear relationship between rainfall and forest area. To quantify these impressions, we again calculate a correlation score. In this case, both variables are not normally distributed, which we can see from the very low p-values of the Shapiro-Wilk tests. Therefore we go for the Spearman correlation coefficient."
water_3<-"The results fit to the first impression: there is a very strong positive correlation (0.9308611) between long-term average annual precipitation and long-term average forest area."
water_4<-"Next we want to look at the relationship to the total renewable water resources a country has available per year."
water_5<-"Again there seems to be a strong relationship. Both variables are not normally distributed, so we go for the Spearman correlation coefficient once more."
water_6<-"The results show that there is also a very strong positive correlation (0.8807696) between the total renewable water resources a country has available per year and the long-term average forest area of a country."
water_7<-"Although these results are not really surprising, they give a clear impression of how dependent our forests are on the amount of available water and rainfall. Since with climate change the amount of extreme weather phenomena increases, this could lead to further troubles for the global forest land."
Spearman_1<-"##  Shapiro-Wilk normality test
## 
## data:  water_forest_data$`Long-term average annual precipitation in volume`
## W = 0.37658, p-value < 0.00000000000000022"
Spearman_2<-"##  Shapiro-Wilk normality test
## 
## data:  water_forest_data$ForestArea
## W = 0.2588, p-value < 0.00000000000000022"
Spearman_3<-"##  Shapiro-Wilk normality test
## 
## data:  water_forest_data$`Total renewable water resources`
## W = 0.34162, p-value < 0.00000000000000022"
carbon_stock_3<-"That’s why we want to put the GHG emission in to relation with the forest area. The objective of our analysis is to investigate how much GHG will be absorbed if the forest area is increased."
carbon_stock_4<-"Using linear regression we come to the conclusion, that for a 1000 ha increase in total forest area, the volume of carbon absorbed will increase by 0.0025 MtCO2, given that all other variables being constant. Or differently expressed it would lead to a decrease of CO2 by 0.0025 Mt."
objective<-"To begin with we want to give a general overview of global forest development over the last 30 years. Afterwards we want to dig deeper into the topics of deforestation and reforestation by extracting the main responsible countries, showing trends and investigating in possible correlations, which is leading to a crucial prediction in how many years forests would be lost, if humankind continues to act as it has in the past. In the next chapter, our analysis leads us into the area of forest destruction by natural causes. After a comprehensive overview and focusing on countries most affected by forest destruction, we want to examine whether there is a correlation between rising temperatures and wildfires and additionally try to predict where and when wildfires are likely to occur. In our final chapter we put our forest data in relation to other environmental issues as air pollution, water availability, greenhouse gas emissions and the carbon storage of forests resulting in a prediction of how much forest area has to be further increased to tackle all greenhouse gas emissions."
objective_1<-"During the analysis, we considered some of our rather general questions from the proposal from different angles in order to provide answers that are more specific about different regions of the world. Furthermore we also tried to give some overview over other environmental issues related to our questions, for example which countries have the highest air pollution or emission values."

dt_text <- "We have two variables (forest area destroyed by insects and the percentage of temperate forest) which can help us to predict where and when wildfires are in general likely to occur. Both of these values don't need to be very large to result in a higher than 70% chance of a wildfire in a given country."


ui <- fluidPage(theme = shinytheme("yeti"), 
                
                tags$div(tags$img(height="600",width="100%", style="object-fit: cover;", src="forest2.jpg")),
                
                tabsetPanel(
                    
                    #-------------------------------------      
                    
                    tabPanel("Overview",tags$h1("Motivation"),tags$p(motivation1),tags$p(motivation2),
                             tags$iframe(src="https://www.youtube.com/embed/8X2MroABQ7U", allowfullscreen=NA),
                             tags$h1("Objectives"),tags$p(objective),tags$br(),tags$p(objective_1)),  
                    #---------------------------------------------
                    tabPanel("Global Forest Development",navlistPanel(
                        tabPanel("Global forest trends",tags$p(global_forest_trends, style="margin-top: 16px;"),plotlyOutput(outputId = "plot_global_forest_trends")),
                        tabPanel("Forest Area Change",tags$p(global_forest_trends_1, style="margin-top: 16px;
"),div(plotlyOutput(outputId = "plot_Forest_area_change"), align = "right")),
                        tabPanel("Largest forest area",tags$h1("Countries with the largest forest area"),tags$p("Before we continue with our deeper analysis, we also want to highlight which countries hold the most forest area on our Earth."),tags$p(global_forest_trends_2),plotlyOutput(outputId = "plot_Largest_forest_area"))
                        
                    )#navlistPanel
                    ),
                    #---------------------------------------
                    tabPanel("Reforestation",navlistPanel(tabPanel("Forest Increase",tags$p("By hovering over the following map, a tooltip of the reforestation increase for each country is shown. The greener the country, the higher is the increase.", style="margin-top: 16px;
"),tags$br(),plotlyOutput(outputId = "plot_Forest_Increase")),
                                                          tabPanel("Main drivers of reforestation", tags$p("These are the main drivers of reforestation:", style="margin-top: 16px;"), plotOutput(outputId = "plot_Driver_ref"),tags$p("As these results show mainly countries with a huge surface, we want to put the increase of reforestation from 1990-2020 in relation to the forest area in 1990."),plotOutput(outputId = "plot_Driver_ref_1"),tags$p(reforestation))
                                                          
                                                          
                    )#navlistPanel
                    ),
                    
                    #---------------------------------------
                    tabPanel("Deforestation",
                             navlistPanel(tabPanel("Forest prediction",tags$h1("Main drivers of deforestation and forest prediction"),tags$p(drivers_deforestation_1),tags$br(),tags$p(drivers_deforestation_2),plotlyOutput(outputId = "plot")),
                                          tabPanel("Forest for Co2 Emission",tags$h1("Required forests for Current Co2 emmission"),tags$p(required_forest),plotlyOutput(outputId = "plot1")),
                                          tabPanel("Years until forest is lost",tags$p(Forest_lost, style="margin-top: 16px;"),plotlyOutput(outputId = "plot_Forest_lost"),plotlyOutput(outputId = "plot_Forest_lost_1"),tags$p(Forest_lost_1))
                             )),
                    #-------------------------------------------
                    
                    tabPanel("Reforestation Vs Deforestation",
                             navlistPanel(tabPanel("Relation",tags$h1("Relation between reforestation and deforestation"),tags$p(ref_def_1),tags$br(),tags$p(ref_def_2),plotOutput(outputId = "plot_ref_def_1"),tags$p(ref_def_3),tags$br(),tags$p(ref_def_4),plotOutput(outputId = "plot_ref_def_2"),tags$p(ref_def_4a),tags$p(ref_def_4b),tags$p(ref_def_4c),tags$p("Spearman =  0.540645228525197"),tags$p(ref_def_4d),tags$p("However this value is with 0.14 not as high as expected after the correlation result.")),
                                          tabPanel("Trends by Continent",tabsetPanel(tabPanel("Reforestation",tags$p(ref_def_5, style="margin-top: 16px;"),plotOutput(outputId = "plot_ref_def_4")),
                                                                                     tabPanel("Deforestation",tags$p(ref_def_6, style="margin-top: 16px;"),plotOutput(outputId = "plot_ref_def_3"),tags$p(ref_def_7))
                                                                                     
                                          )#tabsetPanel
                                          )
                                          
                             )#navlistPanel  
                    ),
                    
                    #---------------------------------    
                    tabPanel("Forest Destruction", 
                             navlistPanel(tabPanel("Destruction yearly with cause ",tabsetPanel(tabPanel("Global visual",tags$h1("Main causes of forest destruction"),tags$p(destruction),plotOutput(outputId = "plot1_1")),
                                                                                                tabPanel("Regional visual",tags$p("Note: the peak in the plot about Germany was quite likely caused by the heat wave in 2003 (Gunkel 2013).", style="margin-top: 16px;"),selectInput(inputId = "Country_1",label = "Select country to plot",choices = unique(fra_data$name),selected = "Germany"),plotOutput(outputId = "plot_t1")) 
                             )),
                             
                             tabPanel("Destruction Causes",tabsetPanel(tabPanel("Global visual",tags$h1("Destroyed forest by cause"),tags$p(destruction_cause),plotOutput(outputId = "plot1_2")),
                                                                       tabPanel("Regional visual",tags$p(destruction_cause_1, style="margin-top: 16px;"),selectInput(inputId = "Country_2",label = "Select country to plot",choices = unique(fra_data$name),selected = "Germany"),plotOutput(outputId = "plot_t2")) 
                             )),
                             tabPanel("Countries Affected",tabsetPanel(tabPanel("Global visual",tags$h1("Most affected countries"),tags$p(countires_affected),plotOutput(outputId = "plot1_3")),
                                                                       tabPanel("Continent visual",tags$p("For Europe one can see that except for Russia, Europe’s most affected countries have no problem with wildfires.", style="margin-top: 16px;"),selectInput(inputId = "continent",label = "Select continent to plot",choices = c( "Americas", "Asia", "Africa", "Europe", "Oceania", "Global"),selected = "Europe"), 
                                                                                selectInput(inputId = "number",label = "Select number of countries to plot",choices = c(5, 10, 15), selected = 5), 
                                                                                selectInput(inputId = "cause",label = "Select cause to plot",choices = c("Insects", "Diseases", "Weather", "Others", "Fire", "All"), selected = "All"), plotOutput(outputId = "plot_t3"))))
                             
                             
                             )),
                    
                    #-----------------------------------      
                    
                    tabPanel("Wildfires", 
                             navlistPanel(
                                 tabPanel("Wildfires Vs Temperature",tags$h2("Relation between rising temperatures and wildfires"),tags$p(wildfires),plotOutput(outputId = "plot2_1"),tags$p("Next we compare global yearly temperature changes to global forest area destructed by wildfires."),plotOutput(outputId = "plot2_2"),tags$p(wildfires_1),tags$p("Kendall =  0.464052287581699"),tags$p(wildfires_2),tags$p(wildfires_3),plotOutput(outputId = "plot2_3"),tags$p(wildfires_4),tags$p("Kendall =  0.0523189950886021"),tags$p(wildfires_5),tags$p(wildfires_6),plotlyOutput(outputId = "plot2_4"),tags$p(wildfires_7)
                                          
                                          
                                 ),
                    tabPanel("Prediction",tags$h2("Prediction of where and when wildfires are likely to occur"), tags$h4("Decision Tree:"), plotOutput(outputId = "plot_dt1"), tags$p(dt_text))
                                 )
                    ),
                    #-------------------------------------
                    tabPanel("Environmental Issues",navlistPanel(
                        tabPanel("Forest area and Air pollution",tags$h2("Top 10 countries with most air pollution"),plotOutput(outputId = "plot_env_1"),tags$p("India has the highest air pollution in the last 30 years, that’s why we take a closer look on India’s air pollution figures compared to the global figures."),plotOutput(outputId = "plot_env_2"),tags$p(air_poll),plotlyOutput(outputId = "plot_env_3"),tags$p(air_poll_1),plotOutput(outputId = "plot_env_4"),tags$p(air_poll_2),tags$p("Kendall =  -0.032967032967033"),tags$p("The results above indicate that there is no correlation between Forest Area and Air Pollution.")
                        ),
                        tabPanel("GHG Emissions and Carbon in Forest",tags$h2("Relation between greenhouse gas emissions and carbon stored in forests"),tags$br(),tags$h4("Analysis and Visualizations of GHG Emissions"),tags$p(ghg_emission),plotOutput(outputId = "plot_ghg_emission_1"),tags$p("As seen below, we sought to know the proportionate constituents of the green house gases:"),plotOutput(outputId = "plot_ghg_emission_2"),tags$p("Furthermore we calculated the average emission per country from 1990 - 2020 which is visualized in the map below."),plotOutput(outputId = "plot_ghg_emission_3")),
                        tabPanel("Carbon Stock",tags$h2("Analysis and Visualization of carbon stock"),tags$p(carbon_stock),plotOutput(outputId = "plot_mergedData_1_plot"),plotOutput(outputId = "plot_mergedData_2_plot"),tags$p(carbon_stock_1),plotOutput(outputId = "plot_mergedData_3_plot"),tags$p("Correlation:We seek to answer the correlation question here, we start by comparing both variables per year."),plotOutput(outputId = "plot_mergedData_4_plot"),tags$p("Correlation result: Kendall =  -0.954415954415954"),tags$p(carbon_stock_2),tags$h2("Relation between absorbed gas and forest area increase"),tags$p(carbon_stock_3),plotOutput(outputId = "plot_carbon_stock_1"),tags$p(carbon_stock_4)
                        ),
                        
                        tabPanel("Water/Precipitation and Forest area",tags$h2("Relation between available water/precipitation and forest area"),tags$p(water_1),plotOutput(outputId = "plot_water_1"),tags$p(water_2),tags$p("Spearman =  0.930861096154095"),tags$p(water_3),tags$p(water_4),plotOutput(outputId = "plot_water_2"),tags$p(water_5),tags$p("Spearman =  0.880769586303481"),tags$p(water_6),tags$p(water_7))
                        
                        
                        
                        
                    )#navlistPanel
                    ),
                    
                    #-------------------------------------
                    
                    tabPanel("Final Analysis",tags$p(final_analysis_1, style="margin-top: 16px;"),tags$p(final_analysis_2),tags$p(final_analysis_3)),
                    #------------------------------------
                    tabPanel("Ressources",navlistPanel(
                        tabPanel("Source code & Process notebook", tags$a(tags$img(src="git.png"), href="https://github.com/ThorbenHe/DataSciR"), style="margin-top: 16px;"),
                        tabPanel("Data and References",tags$a("Climate Watch, CAIT data: 2020. “GHG Emissions.” Washington, DC: World Resources Institute. 2020.",href="https://www.climatewatchdata.org/ghg-emissions"),tags$br(),
                                 tags$a("FAO. 2020a. “Global Forest Resources Assessment 2020.” 2020.",href="https://fra-data.fao.org/WO/fra2020/home/"),tags$br(),
                                 tags$a("2020b. “Global Forest Resources Assessment 2020: Main Report.” Rome: FAO.",href="https://doi.org/10.4060/ca9825en"),tags$br(),
                                 tags$a("2021a. “FAOSTAT Temperature Change Dataset.” Rome Italy: FAO. 2021.",href="http://www.fao.org/faostat/en/#data/ET"),tags$br(),
                                 tags$a("2021b. “FAOSTATC Climate Change,emissions, Fires.” Rome Italy: FAO. 2021.",href="http://www.fao.org/faostat/en/#data/GF"),tags$br(),
                                 tags$p("Field, Christopher B, and Vicente R Barros. 2014. Climate Change 2014–Impacts, Adaptation and Vulnerability: Regional Aspects. Cambridge University Press."),
                                 tags$a("Göbel, Alexander. 2021. “Ein Grüner gürtel Gegen Die Sandige wüste.” Tagesschau.de.",href="https://web.archive.org/web/20100222172301/http://www.tagesschau.de:80/ausland/sahelzone100.html"),tags$br(),
                                 tags$a("Gunkel, Christoph. 2013. “Die Vergessene Jahrhundertkatastrophe.” Der Spiegel.", href="https://www.spiegel.de/geschichte/jahrhundertsommer-2003-eine-der-groessten-naturkatastrophen-europas-a-951214.html"),tags$br(),
                                 tags$p("Messier, Christian, Klaus J Puettmann, and K David Coates. 2013. Managing Forests as Complex Adaptive Systems: Building Resilience to the Challenge of Global Change. Routledge."),
                                 tags$a("OECD. 2021. “Air Quality and Health: Exposure to Pm2.5 Fine Particles - Countries and Regions.”",href="https://doi.org/10.1787/96171c76-en"),tags$br(),
                                 tags$a("ONLINE, ZEIT. 2021. “Abholzung Im Amazonasgebiet Steigt Auf Neuen höchststand.” ZEIT ONLINE.",href="https://www.zeit.de/wissen/umwelt/2021-06/brasilien-abholzung-amazonasgebiet-regenwald-mai-hoechststand"),tags$br(),
                                 tags$p("Ritchie, Hannah, and Max Roser. 2021. “Forests and Deforestation.” Our World in Data."),
                                 tags$a("UN, Economic Commission for Europe. 2019. “10 Facts to Fall in Love with Forests.” Unece.org.",href="https://unece.org/forestry/news/10-facts-fall-love-forests#:~:text=1")
                                 , style="margin-top: 16px;")
                    )#navlistPanel
                    )#Ressources
                    #------------------------------------  
                    
                ) # navbarPage
) # fluidPage


server <- function(input, output) {
    
    output$plot_dt1<-renderPlot(rpart.plot::rpart.plot(dt_fit$fit, roundint = FALSE))
    
    output$plot<-renderPlotly(plot_ly(def_30, type='choropleth', locations=~def_30$code, z=~def_30$percent_change_1, text=~paste(name,paste("Deforestation in the last 30 years:",t_deforestation ), paste("Current forest area:",`2020` ), paste("Years until forest area will be lost:", year) ,sep = "<br />"), marker = list(line = l), colors = "Purples")  %>% 
                                  colorbar(title = "Forest loss through deforestation [%]", ticksuffix = '%') %>% 
                                  layout(
                                      title = 'Deforestation analysis of the last 30 years',
                                      geo = g2
                                  ))
    
    output$plot1<-renderPlotly(plot_ly(ghg_pre, type='choropleth', locations=ghg_pre$code, z=ghg_pre$new_area, text=ghg_pre$name, colors="Purples", marker = list(line = l)) %>% colorbar(title = "Forest Area <br />Required [1000 ha]") %>% layout(
        title = 'New forest area required <br />to componsate present emission',
        geo = g
    ))
    
    
    output$plot2<-renderPlotly(plot_ly(trends_def,x=~year,y=~t_deforestation,color = trends_def$continent) %>% layout(
        title = 'Deforestation Trends over last three decades',xaxis = list(title = 'Decades'),yaxis = list(title = 'Total Deforesstation(1 Unit=1000 ha)')
    ))
    #------------------
    output$plot1_1<-renderPlot(plot_yearly_forest_destruction_by_region())
    
    data_t1<-reactive(input$Country_1)
    output$plot_t1<-renderPlot(plot_yearly_forest_destruction_by_region(data_t1()))
    #------------------
    output$plot1_2<-renderPlot(plot_total_forest_destruction_by_region())
    data_t2<-reactive(input$Country_2)
    output$plot_t2<-renderPlot(plot_total_forest_destruction_by_region(data_t2()))
    #--------------------
    output$plot1_3<-renderPlot(plot_highest_forest_destruction())
    
    data_t3<-reactive(input$continent)
    data_t4<-reactive(input$cause)
    data_t5<-reactive(input$number)
    output$plot_t3<-renderPlot(plot_highest_forest_destruction(cont = data_t3(), number = data_t5(), cs = data_t4()))
    
    #-------------------
    output$plot2_1<-renderPlot(fra_tc_global %>% 
                                   group_by(year) %>% 
                                   summarise(fire = sum(Fire, na.rm = TRUE), 
                                             temp = mean(temp_increase)) %>% 
                                   ggplot(aes(x=year, y=fire, fill = temp)) +
                                   geom_col() + 
                                   scale_y_continuous(labels = scales::comma) +
                                   scale_fill_viridis_c(option = "inferno") +
                                   labs(title = "Global area of wildfires per year",
                                        subtitle = "Time period: 2000 - 2017",
                                        x = "Year",
                                        y = "Wildfire area [1000 ha]",
                                        fill = "Temperature increase [C°]") +
                                   theme_minimal())
    #------------------ 
    output$plot2_2<-renderPlot(fra_tc_global %>% 
                                   group_by(year) %>% 
                                   summarise(fire = sum(Fire, na.rm = TRUE), 
                                             temp = temp_increase) %>% 
                                   ungroup() %>% 
                                   unique() %>% 
                                   ggplot(aes(x=temp, y=fire)) +
                                   geom_point() +
                                   geom_smooth(method = "lm") +
                                   ggrepel::geom_text_repel(
                                       aes(label = year), 
                                       color = "black", size = 7/.pt) +
                                   labs(
                                       title = "Relationship between global temperature increase and wildfire area",
                                       subtitle = "Time period: 2000 - 2017",
                                       x = "Temperature Increase [C°]",
                                       y = "Wildfire Area [1000 ha]" 
                                   ) +
                                   theme_minimal())
    
    #-------------------------
    output$plot2_3<-renderPlot(fra_tc_global %>% 
                                   mutate(Fire = case_when(
                                       is.na(Fire) | Fire == 0 ~ FALSE,
                                       TRUE ~ TRUE
                                   )) %>% 
                                   group_by(year) %>% 
                                   summarise(fire = sum(Fire, rm.na = TRUE), temp = temp_increase) %>% 
                                   ungroup() %>% 
                                   unique() %>% 
                                   ggplot(aes(x=temp, y=fire)) +
                                   geom_point() +
                                   ggrepel::geom_text_repel(
                                       aes(label = year), 
                                       color = "black", size = 7/.pt) +
                                   labs(
                                       title = "Relation between global temperature increase and countries with wildfires",
                                       subtitle = "Time period: 2000 - 2017",
                                       x = "Temperature increase [C°]",
                                       y = "Affected countries" 
                                   ) +
                                   theme_minimal())
    
    output$plot2_4<-renderPlotly(fra_tc_local %>% 
                                     filter(!is.na(Fire)) %>% 
                                     ggplot(aes(y=Fire, x=temp_increase, color = continent)) +
                                     geom_point(alpha = 0.5) +
                                     # geom_smooth(se = FALSE) +
                                     scale_y_continuous(labels = scales::comma) +
                                     labs(title = "Relation between temperature increase and\narea of wildfires per year and country",
                                          subtitle = "Time period: 2000 - 2017, each country is represented by 18 points",
                                          x = "Temperature increase [C°]",
                                          y = "Wildfire area [1000 ha]",
                                          color = "Continent") +
                                     theme_minimal())
    
    
    
    #------------------------new edit data 
    
    output$plot_global_forest_trends<-renderPlotly(
        newData  %>% 
            ggplot(aes(x = Year, y = ForestSize)) +
            geom_point(aes(color = Continent), size = 3, shape = 16, alpha = 0.7) +
            scale_y_continuous(limits = c(100000, 1800000), labels = scales::comma) +
            labs(
                x = "Years",
                y = "Forest area [1000 ha]",
                title = "Global forest area over the last 30 years"
            ) +
            theme_minimal()
    )
    
    output$plot_Forest_area_change<-renderPlotly(
        
        
        plot_ly(tmp, type='choropleth', locations=tmp$iso3, z=tmp$diff, text=tmp$name, colors = "PRGn", marker = list(line = l)) %>%
            colorbar(title = "Forest Area\n[1000 ha]", limits = limit) %>%
            layout(
                title = 'Change in forest area from 1990 - 2020',
                geo = g
            )
    )
    #-------------------------------
    output$plot_Largest_forest_area<-renderPlotly(
        top_data %>% 
            ggplot(aes(x = ForestArea, y = fct_rev(fct_inorder(Country)))) +
            geom_col(fill = "#1c9099") +
            theme(legend.position = "none") +
            labs(
                x = "Forest area [1000 ha]",
                y = NULL,
                title = "Top 10 countries with the largest forest area",
                subtitle = "For the year 2020"
            )+
            scale_x_continuous(labels = scales::comma) +
            theme_minimal()+
            theme(axis.title.x = element_text(color = "Black"), text = element_text(family="Arial")) 
    )
    #----------------
    output$plot_Forest_Increase<-renderPlotly( plot_ly(increase, type='choropleth', locations=increase$code, z=increase$reforestation_increase, text=increase$name, colors = "Greens", marker = list(line = l)) %>% 
                                                   colorbar(ticksuffix = '%') %>%
                                                   layout(
                                                       title = 'Increase in reforestation between 1990 and 2020 in %', 
                                                       geo = g
                                                   ))
    
    output$plot_Driver_ref<-renderPlot(ggplot(ref_data, aes(x = totalref, y = fct_rev(fct_inorder(name)))) +
                                           geom_col(fill = "Forestgreen") +
                                           scale_x_continuous(labels = scales::comma) +
                                           labs(
                                               x = "Increase between 1990-2020 [1000 ha]",
                                               y = NULL,
                                               title = "Top 20 countries of total reforestation between 1990 and 2020"
                                           ) )
    output$plot_Driver_ref_1<-renderPlot(increase %>%
                                             arrange(desc(reforestation_increase)) %>%
                                             head(20)%>%
                                             ggplot(mapping = aes(x = reorder(name, reforestation_increase), y = reforestation_increase)) +
                                             geom_col(fill = "Forestgreen") +
                                             labs(
                                                 y = "Increase in %",
                                                 x = NULL,
                                                 title = "Countries and their increase in reforestation from 1990-2020\nin relation to their total forest area in 1990"
                                             ) +
                                             coord_flip())
    #----------------------------
    output$plot_ref_def_1<-renderPlot(ggplot(data = corref, mapping = aes(x = totalref, y = totaldef)) +
                                          geom_point(alpha=0.5) +
                                          scale_x_continuous(labels = scales::comma) +
                                          scale_y_continuous(labels = scales::comma) +
                                          ggrepel::geom_text_repel(
                                              data = filter(corref), 
                                              aes(label = name), 
                                              color = "black", size = 7/.pt)+
                                          labs(x = "Total reforestation [1000 ha]", y = "Total deforestation [1000 ha]"))
    
    output$plot_ref_def_2<-renderPlot(ggplot(data = corref, mapping = aes(x = totalref, y = totaldef)) +
                                          geom_point(alpha=0.5) +
                                          scale_x_continuous(limits = c(0, 5000), labels = scales::comma) +
                                          scale_y_continuous(limits = c(0, 10000), labels = scales::comma) +
                                          ggrepel::geom_text_repel(
                                              data = filter(corref), 
                                              aes(label = name), 
                                              color = "black", size = 7/.pt)+
                                          labs(x = "Total reforestation [1000 ha]", y = "Total deforestation [1000 ha]")) 
    #----------------------------------
    
    output$plot_ref_def_3<-renderPlot(ggplot(trends_def, aes(x = as.integer(year), y = t_deforestation, color = continent)) +
                                          geom_point(aes(color = continent)) +
                                          geom_line() +
                                          scale_x_continuous(breaks = c(0,1,2), labels = c("1990-2000", "2000-2010", "2010-2020")) +
                                          scale_y_continuous(labels = scales::comma) +
                                          labs(
                                              title = 'Deforestation Trends on continents between 1990-2020',
                                              x = 'Year',
                                              y = 'Deforestation [1000 ha]',
                                              color = "Continent"
                                          ) +
                                          theme_minimal())
    
    output$plot_ref_def_4<-renderPlot(ggplot(trends_ref, aes(x = as.integer(year), y = t_reforestation, color = continent)) +
                                          geom_point(aes(color = continent)) +
                                          geom_line() +
                                          scale_x_continuous(breaks = c(0,1,2), labels = c("1990-2000", "2000-2010", "2010-2020")) +
                                          scale_y_continuous(labels = scales::comma) +
                                          labs(
                                              title = 'Reforestation Trends on continents between 1990-2020',
                                              x = 'Year',
                                              y = 'Reforestation [1000 ha]',
                                              color = "Continent"
                                          ) +
                                          theme_minimal())
    #-------------------------
    output$plot_env_1<-renderPlot(AirPollution %>% 
                                      drop_na() %>% 
                                      group_by(Country) %>% 
                                      summarize(avgAirPollution = mean(AirPollution)) %>% 
                                      as.data.frame() %>% 
                                      ungroup() %>% 
                                      arrange(desc(avgAirPollution)) %>%
                                      top_n(10) %>% 
                                      ggplot(aes(x = avgAirPollution, y = reorder(Country, avgAirPollution))) +
                                      geom_col(fill = "#DE8971", width = 0.5) +
                                      theme(legend.position = "none") +
                                      labs(
                                          x = expression(paste("Average air pollution [", mu, "g/", m^3, "]")),
                                          y = NULL,
                                          title = "Top 10 countries with the highest air pollution",
                                          subtitle = "Time period: 1990 - 2020"
                                      )+
                                      scale_x_continuous(labels = scales::comma) +
                                      theme_minimal()+
                                      theme(axis.title.x = element_text(color = "Black" ), text = element_text(family="Arial"))+
                                      geom_vline(aes(xintercept=mean(AirPollution$AirPollution)), 
                                                 color = "#204887", size=1)+
                                      geom_text(aes(x= 38,y= 8.5,label="Global average air pollution"),size=3))
    
    output$plot_env_2<-renderPlot(AirPollution_India_plot)
    output$plot_env_3<-renderPlotly(trend_plot)
    output$plot_env_4<-renderPlot(mergedData_plot)
    #--------------------------------------
    output$plot_ghg_emission_1<-renderPlot(emissionData_plot)
    output$plot_ghg_emission_2<-renderPlot(pie)
    output$plot_ghg_emission_3<-renderPlot(mapCountryData(worldmaps, 
                                                          nameColumnToPlot = "Average Emission Value [MtCO2e] per Country from 1990 to 2020",
                                                          catMethod = 'fixedWidth', 
                                                          colourPalette = colourPalette, 
                                                          numCats = 50))
    #-----------------------------
    output$plot_mergedData_1_plot<-renderPlot(mergedData_1_plot)
    output$plot_mergedData_2_plot<-renderPlot(mergedData_2_plot)  
    output$plot_mergedData_3_plot<-renderPlot(mergedData_3_plot) 
    output$plot_mergedData_4_plot<-renderPlot(mergedData_4_plot) 
    #------------------------------
    output$plot_Forest_lost<-renderPlotly(plot_ly(defTop,  type="bar", x=defTop$year, y = fct_rev(fct_inorder(defTop$name))) %>% layout(
        title = 'Years until forest area is lost based on deforestation',xaxis = list(title = 'Years')
    ))
    
    output$plot_Forest_lost_1<-renderPlotly(plot_ly(defBot, type="bar", x=defBot$year, y = fct_rev(fct_inorder(defBot$name)), color = "red") %>% layout(
        title = 'Years until forest area is lost based on deforestation',xaxis = list(title = 'Years')
    )) 
    #---------------   
    output$plot_water_1<-renderPlot(ggplot(water_forest_data %>% drop_na(), 
                                           aes(x = `Long-term average annual precipitation in volume`, y = ForestArea)) + 
                                        geom_point(alpha = 0.3) +
                                        geom_smooth(method = "lm") +
                                        ggrepel::geom_text_repel(
                                            aes(label = Country), 
                                            color = "black", size = 7/.pt) +
                                        scale_x_continuous(labels = scales::comma) +
                                        scale_y_continuous(labels = scales::comma) +
                                        labs(
                                            title = "Relation between precipitation and forest area",
                                            x = expression(paste("Long-term average annual precipitation [", 10^9, " ", m^3, "/year", "]")),
                                            y = "Forest area [1000 ha]"
                                        ))
    
    output$plot_water_2<-renderPlot(
        ggplot(water_forest_data, 
               aes(x = `Total renewable water resources`, y = ForestArea)) + 
            geom_point(alpha = 0.3) +
            geom_smooth(method = "lm") +
            ggrepel::geom_text_repel(
                aes(label = Country), 
                color = "black", size = 7/.pt) +
            scale_x_continuous(labels = scales::comma) +
            scale_y_continuous(labels = scales::comma) +
            labs(
                title = "Relation between renewable water resources and forest area",
                x = expression(paste("Total renewable water resources [", 10^9, " ", m^3, "/year", "]")),
                y = "Forest area [1000 ha]"
            )) 
    #----------------------
    output$plot_carbon_stock_1<-renderPlot(data_1 %>%
                                               ggplot(aes(y = totalArea, x = totalCarbonAbsorbed)) + geom_point(color="red1") + geom_smooth(method = "lm", size = 1, fill="deepskyblue2") + 
                                               labs(y = "Forest Area [1000 ha]", x = "Carbon Absorbed [MtCO2]", title = "Comparison between absorbed carbon and forest area") +
                                               scale_x_continuous(labels = scales::comma) +
                                               scale_y_continuous(labels = scales::comma))
}

shinyApp(ui, server, options = list(launch.browser = TRUE))








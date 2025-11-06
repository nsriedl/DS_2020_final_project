worldwide energy usage
================

## Research Topic (Title):

The Relationship Between Population Growth, Energy Demand, and Renewable
Transition in Developing Nations

## Team members:

Prudvik Seemakurthi, Nate Riedl

### Data:

**Source:** The dataset comes from Our World in Data (OWID) — a research

and publication platform based at the University of Oxford and the
Oxford Martin School. Link: <https://ourworldindata.org/energy>

**Description:** - This dataset provides a global, long-term view of
energy

production, consumption, and emissions for over 200 countries and
territories, spanning from the 1960s to 2023.  
- It integrates multiple aspects of the global energy system,
covering: - Total and per-capita energy consumption (in terawatt-hours,
TWh)  
- Energy production and electricity generation by source (coal, oil,
gas, renewables, nuclear, etc.)  
- Carbon dioxide (CO₂) emissions and carbon intensity of electricity  
- Fossil fuel and renewable shares in energy consumption  
- Economic and demographic indicators (GDP and population)

### Some of the important variables include:

| **Variable Name** | **Description** |
|----|----|
| `country` | Name of the country or region |
| `iso_code` | Three-letter ISO country code |
| `year` | Year of the observation |
| `population` | Total population of the country (from UN data) |
| `gdp` | Gross Domestic Product (constant international dollars, PPP-adjusted) |
| `primary_energy_consumption` | Total primary energy used (TWh) |
| `energy_per_capita` | Energy use per person (TWh or MWh per capita) |
| `fossil_fuel_consumption` | Total fossil fuel energy use (TWh) |
| `fossil_share_energy` | Share (%) of fossil fuels in total energy use |
| `renewables_consumption` | Renewable energy use (TWh) |
| `renewables_share_energy` | Share (%) of renewables in total energy use |
| `electricity_generation` | Total electricity generation (TWh) |
| `carbon_intensity_elec` | CO₂ emitted per unit of electricity generated (gCO₂/kWh) |
| `energy_per_gdp` | Energy use per unit of GDP (TWh per \$GDP) |

### Questions to Be Addressed (Fleshed Out Project Idea):

# Goal:

To analyze how population size and economic development influence energy
demand, emissions, and the transition to renewables. with a focus on
identifying developing nations achieving renewable growth despite low
GDP.

# Research Questions:

1.  Population & Energy: How does population size affect total energy
    demand and reliance on fossil fuels across countries?

2.  Population Growth & Fossil Dependence: Are countries with rapid
    population growth relying more on fossil fuels?

3.  Clean Energy & Economic Development: How does access to renewable
    (clean) energy correlate with GDP and energy per capita?

4.  Renewable Growth in Developing Nations: Which developing nations
    show the strongest renewable expansion despite low GDP?

5.  Energy Consumption & Time: How has the amount of fossil fuel vs
    renewable consumed changed over time?

6.  Electricity Generation: Which countries have had the highest
    electricity generation over the years and how does this compare to
    their fossil fuel and renewable energy consumption?

# Expected Outcomes:

Identify patterns showing whether high population correlates with higher
emissions or energy demand. Evaluate if developing nations are catching
up in renewable energy use. Visualize global disparities in renewable
adoption and fossil fuel dependence. Highlight countries that have made
significant clean energy progress despite economic challenges.

``` r
library(tidyverse)
```

    ## Warning: package 'ggplot2' was built under R version 4.5.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
df <- read_csv("owid-energy-data.csv")
```

    ## Rows: 23195 Columns: 130
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (2): country, iso_code
    ## dbl (128): year, population, gdp, biofuel_cons_change_pct, biofuel_cons_chan...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
-
dim(df)
```

    ## [1] -23195   -130

``` r
#colnames(df)
#unique(df$country)
head(df)
```

    ## # A tibble: 6 × 130
    ##   country        year iso_code population   gdp biofuel_cons_change_pct
    ##   <chr>         <dbl> <chr>         <dbl> <dbl>                   <dbl>
    ## 1 ASEAN (Ember)  2000 <NA>             NA    NA                      NA
    ## 2 ASEAN (Ember)  2001 <NA>             NA    NA                      NA
    ## 3 ASEAN (Ember)  2002 <NA>             NA    NA                      NA
    ## 4 ASEAN (Ember)  2003 <NA>             NA    NA                      NA
    ## 5 ASEAN (Ember)  2004 <NA>             NA    NA                      NA
    ## 6 ASEAN (Ember)  2005 <NA>             NA    NA                      NA
    ## # ℹ 124 more variables: biofuel_cons_change_twh <dbl>,
    ## #   biofuel_cons_per_capita <dbl>, biofuel_consumption <dbl>,
    ## #   biofuel_elec_per_capita <dbl>, biofuel_electricity <dbl>,
    ## #   biofuel_share_elec <dbl>, biofuel_share_energy <dbl>,
    ## #   carbon_intensity_elec <dbl>, coal_cons_change_pct <dbl>,
    ## #   coal_cons_change_twh <dbl>, coal_cons_per_capita <dbl>,
    ## #   coal_consumption <dbl>, coal_elec_per_capita <dbl>, …

# Basic cleaning:

1.  keeping relevant columns

``` r
energy_clean <- df %>%
  select(country, iso_code, year, population, gdp,
         fossil_fuel_consumption, renewables_consumption,
         fossil_share_energy, renewables_share_energy,
         energy_per_capita, energy_cons_change_twh, energy_cons_change_pct)
```

2.  This step filters the dataset to keep data from 1990 onwards,
    removes regions like continents and the world totals.

``` r
energy_clean <- energy_clean %>%
  filter(year >= 1990) %>%
  filter(!country %in% c("World", "Asia", "Europe", "Africa",
                         "North America", "South America", 
                         "Oceania", "European Union (27)")) %>%
  filter(!is.na(population))
```

3.This step groups the data by country and arranges it by year, then
calculates each country’s total energy demand, yearly population growth
rate, and the ratio of renewable to fossil fuel consumption.

``` r
energy_clean <- energy_clean %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    total_energy_demand = fossil_fuel_consumption + renewables_consumption,
    pop_growth_rate = (population - lag(population)) / lag(population) * 100,
    renew_fossil_ratio = renewables_consumption / fossil_fuel_consumption,
  )%>% ungroup()
length(unique(energy_clean$country))
```

    ## [1] 230

4.  This step filters out rows where the total energy demand is missing
    or equal to zero, keeping only valid and meaningful energy data for
    analysis.

``` r
energy_clean <- energy_clean %>%
  filter(!(is.na(total_energy_demand) | total_energy_demand == 0))
```

# Summary:

``` r
summary(energy_clean %>%
          select(population, total_energy_demand,
                 fossil_share_energy, renewables_share_energy,
                 energy_per_capita, gdp))
```

    ##    population        total_energy_demand fossil_share_energy
    ##  Min.   :2.548e+05   Min.   :   19.27    Min.   : 13.87     
    ##  1st Qu.:5.551e+06   1st Qu.:  209.80    1st Qu.: 78.39     
    ##  Median :2.030e+07   Median :  459.55    Median : 88.53     
    ##  Mean   :1.446e+08   Mean   : 2932.61    Mean   : 84.57     
    ##  3rd Qu.:6.153e+07   3rd Qu.: 1370.82    3rd Qu.: 97.03     
    ##  Max.   :3.122e+09   Max.   :73999.43    Max.   :100.00     
    ##                                                             
    ##  renewables_share_energy energy_per_capita       gdp           
    ##  Min.   : 0.000          Min.   :   626.2   Min.   :5.222e+09  
    ##  1st Qu.: 1.633          1st Qu.: 16997.6   1st Qu.:1.262e+11  
    ##  Median : 6.175          Median : 31961.7   Median :3.011e+11  
    ##  Mean   :11.287          Mean   : 42746.3   Mean   :9.732e+11  
    ##  3rd Qu.:16.065          3rd Qu.: 53906.4   3rd Qu.:8.098e+11  
    ##  Max.   :86.126          Max.   :318587.3   Max.   :2.697e+13  
    ##                                             NA's   :265

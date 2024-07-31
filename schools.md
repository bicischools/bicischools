# School data in the LMA
Joey Talbot

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
    Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.3.1; sf_use_s2() is TRUE

    Breaking News: tmap 3.x is retiring. Please test v4, e.g. with
    remotes::install_github('r-tmap/tmap')

Schools in Portugal are classified into Básica and Secundária. Básica
runs from age 9 to 15 years, and Secundária for age 15 to 18 years.
Básica schools are further divided into Ciclo 1 (age 6-10), Ciclo 2 (age
10-12), and Ciclo 3 (age 12-15). Multiple age groups may be catered for
within the same establishment. For example, some schools include Ciclo
1, Ciclo 2 and Ciclo 3, while other schools include Ciclo 2, Ciclo 3,
and Secundária.

For Bicischools we will focus on the Ciclo 1, Ciclo 2 and Ciclo 3 age
groups.

## School locations and number of students

We used school location data from [Carris
Metropolitana](https://raw.githubusercontent.com/carrismetropolitana/datasets/latest/facilities/schools/schools.csv),
plus national data on school type and enrolled student numbers by gender
and year group from [the General Directorate of Education and Science
Statistics](https://www.dgeec.medu.pt/api/ficheiros/65708d9fcc461a4d46cd9a13).

We sourced high resolution population data, including the population
aged 0-14 living within small zones, from [BGRI]().

``` r
schools_geo = readRDS("data/SCHOOLS_geo.Rds")
schools_year = readRDS("data/SCHOOLS_year.Rds")

bgri = read_sf("../internal/BGRI21_170/BGRI21_170.gpkg")
bgri_lisbon = bgri |> 
  filter(DTMN21 == 1106)

schools_both = inner_join(schools_year, schools_geo, by = c("DGEEC_id" = "CÓDIGO DGEEC ESCOLA"))
schools_both = st_as_sf(schools_both)

by_ciclo = schools_both |> 
  group_by(DGEEC_id, ESCOLA, MUNICIPIO, PUBLIC_PRIVATE, CICLO_ESTUDOS) |> 
  summarise(STUDENTS = sum(STUDENTS))
```

    `summarise()` has grouped output by 'DGEEC_id', 'ESCOLA', 'MUNICIPIO',
    'PUBLIC_PRIVATE'. You can override using the `.groups` argument.

``` r
c1 = by_ciclo |> 
  filter(CICLO_ESTUDOS == "1.º Ciclo")
c2 = by_ciclo |> 
  filter(CICLO_ESTUDOS == "2.º Ciclo")
c3 = by_ciclo |> 
  filter(CICLO_ESTUDOS == "3.º Ciclo")

# saveRDS(c1, "data/c1.Rds")
# saveRDS(c2, "data/c2.Rds")
# saveRDS(c3, "data/c3.Rds")

c1_public = by_ciclo |> 
  filter(CICLO_ESTUDOS == "1.º Ciclo",
         PUBLIC_PRIVATE == "Público")
c2_public = by_ciclo |> 
  filter(CICLO_ESTUDOS == "2.º Ciclo",
         PUBLIC_PRIVATE == "Público")
c3_public = by_ciclo |> 
  filter(CICLO_ESTUDOS == "3.º Ciclo",
         PUBLIC_PRIVATE == "Público")

# saveRDS(c1_public, "data/c1_public.Rds")
# saveRDS(c2_public, "data/c2_public.Rds")
# saveRDS(c3_public, "data/c3_public.Rds")
```

For Lisbon municipality, this shows the number of students at Ciclo 1
schools.

``` r
lisbon_c1 = by_ciclo |> 
  filter(MUNICIPIO == "Lisboa",
         CICLO_ESTUDOS == "1.º Ciclo")
lisbon_c2 = by_ciclo |> 
  filter(MUNICIPIO == "Lisboa",
         CICLO_ESTUDOS == "2.º Ciclo")
lisbon_c3 = by_ciclo |> 
  filter(MUNICIPIO == "Lisboa",
         CICLO_ESTUDOS == "3.º Ciclo")

tm_shape(bgri_lisbon) + tm_polygons("N_INDIVIDUOS_0_14") +
  tm_shape(lisbon_c1) + tm_dots(size = "STUDENTS")
```

![Ciclo 1 schools in Lisbon municipality, with number of
students](schools_files/figure-commonmark/unnamed-chunk-2-1.png)

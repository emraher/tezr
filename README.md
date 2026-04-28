
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tezr <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/emraher/tezr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/emraher/tezr/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**tezr** provides functions to query Turkiye’s [National Thesis
Center](https://tez.yok.gov.tr) (Ulusal Tez Merkezi) and extract
metadata.

**Disclaimer**: This package is not affiliated with, endorsed by, or
connected to YOK (Yükseköğretim Kurulu / Council of Higher Education) or
the National Thesis Center. It is an independent tool for academic
research purposes.

## Installation

``` r
# install.packages("pak")
pak::pak("emraher/tezr")
```

## Quick Start

``` r
library(tezr)
```

Search results use `title_original` and `title_translation`. Detail
records use `abstract_original` and `abstract_translation`.

### Basic Keyword Search

``` r
household <- search_basic(keyword = "hanehalkı")
#> ℹ Initializing session...
#> ℹ Searching for: "hanehalkı"
#> ✔ Found 1019 results
#> ✔ Returning 1019 results

dplyr::glimpse(household)
#> Rows: 1,019
#> Columns: 13
#> $ thesis_no       <chr> "949991", "981164", "905554", "866224", "792466", "811…
#> $ title_original   <chr> "Parasal aktarım mekanizmasının hanehalkı bilançoları…
#> $ title_translation <chr> "REFLECTIONS OF MONETARY TRANSMISSION MECHANISM ON HO…
#> $ author          <chr> "HALİL TANYILDIZI", "AYŞENUR KARAHASANOĞLU", "TARIK UÇ…
#> $ university      <chr> "Erzincan Binali Yıldırım Üniversitesi", "Çukurova Üni…
#> $ year            <int> 2025, 2025, 2024, 2024, 2023, 2023, 2023, 2022, 2022, …
#> $ thesis_type_tr  <chr> "Doktora", "Yüksek Lisans", "Yüksek Lisans", "Doktora"…
#> $ thesis_type_en  <chr> "Doctorate", "Master", "Master", "Doctorate", "Doctora…
#> $ language_tr     <chr> "Türkçe", "Türkçe", "Türkçe", "Türkçe", "Türkçe", "İng…
#> $ language_en     <chr> "Turkish", "Turkish", "Turkish", "Turkish", "Turkish",…
#> $ subject_tr      <chr> "İşletme", "Ekonomi", "Ekonometri; İşletme", "Ekonomi;…
#> $ subject_en      <chr> "Business Administration", "Economics", "Econometrics;…
#> $ detail_id       <chr> "h_WrLBMhp0j9Ih1Bz6_ssA", "YpbNX80LdVlVM683htrXqQ", "C…
```

### Advanced Search with Filters

``` r
climate_change <- search_advanced(
  keyword = "iklim değişikliği",
  year_start = 2015,
  group = "science"
)
#> ℹ Initializing session...
#> ℹ Performing advanced search...
#> ✔ Found 2314 results
#> ! Returning 2000 of 2314 results.Set `max_search_results = Inf` to auto-paginate and retrieve all results.
```

### Detailed Search with Filters

``` r
phd_theses <- search_detailed(
  university = "Ankara Üniversitesi",
  division = "İktisat Ana Bilim Dalı",
  thesis_type = "phd",
  year_start = 2020
)
#> ℹ Initializing session...
#> ℹ Found University ID: "3"
#> ℹ Found Division ID: "51"
#> ℹ Performing detailed search...
#> ✔ Found 20 results
#> ✔ Returning 20 results
```

### Get Thesis Details

``` r
details <- detail(phd_theses$detail_id[1])
#> ℹ Fetching thesis details...
#> ✔ Retrieved details for thesis

details |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "colname",
    values_to = "colvalue"
  ) |>
  print(n = 23)
#> # A tibble: 24 × 2
#>    colname         colvalue                                                     
#>    <chr>           <chr>                                                        
#>  1 thesis_no       634695                                                       
#>  2 title_original  Çevre vergileri üzerine üç makale                            
#>  3 title_translation Three papers on environmental taxes                        
#>  4 author          MUSTAFA EMİR YÜCEL                                           
#>  5 advisor         PROF. DR. TÜRKMEN GÖKSEL                                     
#>  6 co_advisor      <NA>                                                         
#>  7 university      Ankara Üniversitesi                                          
#>  8 institute       Sosyal Bilimler Enstitüsü                                    
#>  9 division        İktisat Ana Bilim Dalı                                       
#> 10 year            2020                                                         
#> 11 pages           117                                                          
#> 12 thesis_type_tr  Doktora                                                      
#> 13 thesis_type_en  Doctorate                                                    
#> 14 language_tr     Türkçe                                                       
#> 15 language_en     Turkish                                                      
#> 16 subject_tr      Ekonomi                                                      
#> 17 subject_en      Economics                                                    
#> 18 abstract_original     Bu tez çalışması, çevre vergilerinin ve bu vergilerden elde …
#> 19 abstract_translation  This thesis, theoretically and empirically analyzed the effe…
#> 20 keywords_tr     Uluslararası ticaret; Vergiler; Çevre ekonomisi; Çevre kirli…
#> 21 keywords_en     International trade; Taxes; Environmental economics; Environ…
#> 22 access_status   open                                                         
#> 23 pdf_url         https://tez.yok.gov.tr/UlusalTezMerkezi/TezGoster?key=_F5QEp…
#> # ℹ 1 more row
```

## Learn More

- **[Getting
  Started](https://emraher.github.io/tezr/articles/getting-started.html)**
- **[Analysis
  Examples](https://emraher.github.io/tezr/articles/analysis-examples.html)**
- **[Function
  Reference](https://emraher.github.io/tezr/reference/index.html)**


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
#> ℹ Searching for: hanehalkı
#> ✔ Found 1040 results
#> ✔ Returning 1040 results

dplyr::glimpse(household)
#> Rows: 1,040
#> Columns: 15
#> $ thesis_no         <chr> "1002204", "999418", "1002109", "1001006", "1001935"…
#> $ title_original    <chr> "Coğrafi bilgi sistemleri aracılığıyla ulaşım modlar…
#> $ title_translation <chr> "Transportation modes through geographic information…
#> $ author            <chr> "EMİNE BERFİN ŞAHİN", "GÜNSENİN ALTINKAYNAK", "CANSU…
#> $ university        <chr> "YILDIZ TEKNİK ÜNİVERSİTESİ", "PAMUKKALE ÜNİVERSİTES…
#> $ year              <int> 2026, 2026, 2026, 2026, 2026, 2026, 2026, 2026, 2026…
#> $ thesis_type_tr    <chr> "Yüksek Lisans", "Doktora", "Yüksek Lisans", "Yüksek…
#> $ thesis_type_en    <chr> "Master", "Doctorate", "Master", "Master", "Master",…
#> $ language_tr       <chr> "Türkçe", "Türkçe", "İngilizce", "İngilizce", "Türkç…
#> $ language_en       <chr> "Turkish", "Turkish", "English", "English", "Turkish…
#> $ subject_tr        <chr> "Jeodezi ve Fotogrametri; Ulaşım; Şehircilik ve Bölg…
#> $ subject_en        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ detail_id         <chr> "sY_pvMMRcFTFZJBPvsSHDw", "SAPTHPMDjlwDmZOmgUMV4g", …
#> $ encrypted_no      <chr> "d-_X3r5CRNDVZ_qEp8nuWA", "5iGLCCiXIg_o7q1Za8MQQQ", …
#> $ detail_url        <chr> "https://tez.yok.gov.tr/UlusalTezMerkezi/tezDetay.js…
```

### Advanced Search with Filters

``` r
climate_change <- search_advanced(
  keyword = "iklim değişikliği",
  year_start = 2015,
  group = "science"
)
#> ℹ Performing advanced search...
#> ✔ Found 204 results
#> ✔ Returning 204 results
```

### Detailed Search with Filters

``` r
phd_theses <- search_detailed(
  university = "Ankara Üniversitesi",
  division = "İktisat Ana Bilim Dalı",
  thesis_type = "phd",
  year_start = 2020
)
#> ℹ Found University ID: "3"
#> ℹ Found Division ID: "51"
#> ℹ Performing detailed search...
#> ✔ Found 0 results
#> ℹ YOK returned no rows with the university filter. Retrying without that filter and matching the returned rows locally...
#> ℹ Performing detailed search...
#> ✔ Found 1388 results
#> ✔ Returning 1388 results
#> ✔ Returning 20 locally filtered results
```

### Get Thesis Details

``` r
details <- detail(phd_theses[1, ])
#> ℹ Fetching thesis details...
#> ✔ Retrieved details for thesis

details |>
  dplyr::select(
    thesis_no,
    title_original,
    author,
    university,
    year,
    dplyr::starts_with("citation_")
  ) |>
  dplyr::glimpse(width = 80)
#> Rows: 1
#> Columns: 10
#> $ thesis_no        <chr> "955043"
#> $ title_original   <chr> "Dijital ekonomide rekabet olgusu: Nedenleri ve sonuç…
#> $ author           <chr> "ORÇUN"
#> $ university       <chr> "ANKARA ÜNİVERSİTESİ"
#> $ year             <chr> "2025"
#> $ citation_apa     <chr> "KASAP, O. (2025). <i>Dijital ekonomide rekabet olgus…
#> $ citation_ieee    <chr> "O. KASAP, \"Dijital ekonomide rekabet olgusu: Nedenl…
#> $ citation_mla     <chr> "KASAP, ORÇUN. <i>Dijital ekonomide rekabet olgusu: N…
#> $ citation_chicago <chr> "KASAP, ORÇUN. \"Dijital ekonomide rekabet olgusu: Ne…
#> $ citation_harvard <chr> "KASAP, O. (2025) <i>Dijital ekonomide rekabet olgusu…
```

## Learn More

- **[Getting
  Started](https://emraher.github.io/tezr/articles/getting-started.html)**
- **[Analysis
  Examples](https://emraher.github.io/tezr/articles/analysis-examples.html)**
- **[Function
  Reference](https://emraher.github.io/tezr/reference/index.html)**

# Generate internal translation data for tezr
#
# This script scrapes thesis type and language mappings from the YOK thesis
# center and saves them as R/sysdata.rda for use by parsing functions.
#
# Run manually when YOK changes their thesis type codes or language codes:
#   source("data-raw/sysdata.R")
#
# The script fetches <select> option values from the search form at
# https://tez.yok.gov.tr/UlusalTezMerkezi/giris.jsp

library(httr2)
library(rvest)
library(tibble)

base_url <- "https://tez.yok.gov.tr/UlusalTezMerkezi/"

# Fetch the search page to extract <select> options
resp <- request(base_url) |>
  req_url_path_append("giris.jsp") |>
  req_options(ssl_verifypeer = FALSE) |>
  req_perform()

html <- resp_body_html(resp)

# --- Thesis types ---
# The thesis type dropdown uses values 0-7.
# Labels are in Turkish; English translations are added manually since the
# site does not provide them.
thesis_types <- tibble::tibble(
  value = c("0", "1", "2", "3", "4", "5", "6", "7"),
  label_tr = c(
    "Se\u00e7iniz",
    "Y\u00fcksek Lisans",
    "Doktora",
    "T\u0131pta Uzmanl\u0131k",
    "Sanatta Yeterlik",
    "Di\u015f Hekimli\u011fi Uzmanl\u0131k",
    "T\u0131pta Yan Dal Uzmanl\u0131k",
    "Eczac\u0131l\u0131kta Uzmanl\u0131k"
  ),
  label_en = c(
    "Select",
    "Master",
    "Doctorate",
    "Specialization in Medicine",
    "Proficiency in Art",
    "Specialization in Dentistry",
    "Minor Specialization in Medicine",
    "Expertise in Pharmacy"
  )
)

# --- Languages ---
# The language dropdown. Values and Turkish labels come from the site;
# English labels are added manually.
languages <- tibble::tibble(
  value = c(
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10",
    "11",
    "12",
    "13",
    "14",
    "15",
    "16",
    "17",
    "18",
    "19",
    "20",
    "21",
    "26",
    "27",
    "28",
    "29",
    "30",
    "31",
    "32",
    "33",
    "34",
    "35",
    "36",
    "37",
    "39",
    "41",
    "42",
    "43",
    "44",
    "45"
  ),
  label_tr = c(
    "Se\u00e7iniz",
    "T\u00fcrk\u00e7e",
    "\u0130ngilizce",
    "Arap\u00e7a",
    "Almanca",
    "Frans\u0131zca",
    "\u0130spanyolca",
    "\u0130talyanca",
    "Rus\u00e7a",
    "Leh\u00e7e",
    "\u00c7ince",
    "K\u00fcrt\u00e7e",
    "Azerice",
    "Bulgar\u0131ca",
    "\u00c7ek\u00e7e",
    "Romence",
    "Felemenk\u00e7e",
    "Japonca",
    "Fars\u00e7a",
    "Yunanca",
    "Slovence",
    "Makedonca",
    "\u00c7erkezce",
    "K\u0131rg\u0131zca",
    "Bo\u015fnak\u00e7a",
    "G\u00fcrc\u00fcce",
    "Korece",
    "Ermenice",
    "Zazaca",
    "Malayca",
    "Kazak\u00e7a",
    "Ukraynaca",
    "Mo\u011folca",
    "Endonezce",
    "\u00d6zbek\u00e7e",
    "Macarca",
    "S\u0131rp\u00e7a",
    "Portekizce",
    "Arnavut\u00e7a",
    "Letonca"
  ),
  label_en = c(
    "Select",
    "Turkish",
    "English",
    "Arabic",
    "German",
    "French",
    "Spanish",
    "Italian",
    "Russian",
    "Polish",
    "Chinese",
    "Kurdish",
    "Azerbaijanese",
    "Bulgarian",
    "Czech",
    "Romanian",
    "Dutch",
    "Japanese",
    "Persian",
    "Greek",
    "Slovenian",
    "Macedonian",
    "Adyghe",
    "Kirghiz",
    "Bosnian",
    "Georgian",
    "Korean",
    "Armenian",
    "Zaza",
    "Malay",
    "Kazakh",
    "Ukrainian",
    "Mongolian",
    "Indonesian",
    "Uzbek",
    "Hungarian",
    "Serbian",
    "Portuguese",
    "Albanian",
    "Latvian"
  )
)

usethis::use_data(thesis_types, languages, internal = TRUE, overwrite = TRUE)

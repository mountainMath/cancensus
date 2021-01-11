## code to prepare `CODES_TABLE` dataset goes here

geography <- c("CD", "CD", "CD", "CD", "CD", "CD", "CD", "CD", "CD", "CD", "CD",
               "CD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD",
               "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD",
               "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD",
               "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD",
               "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD", "CSD",
               "CSD", "CSD", "CSD", "CSD")
Encoding(geography) <- "UTF-8"
status_code <- c("CDR", "CT", "CTY", "DIS", "DM", "MRC", "RD", "REG", "RM", "TÉ", "TER",
                 "UC", "C", "CC", "CG", "CN", "COM", "CT", "CU", "CV", "CY", "DM", "HAM",
                 "ID", "IGD", "IM", "IRI", "LGD", "LOT", "M", "MD", "MÉ", "MU", "NH", "NL",
                 "NO", "NV", "P", "PE", "RCR", "RDA", "RGM", "RM", "RV", "S-É", "SA", "SC",
                 "SÉ", "SET", "SG", "SM", "SNO", "SV", "T", "TC", "TI", "TK", "TL", "TP",
                 "TV", "V", "VC", "VK", "VL", "VN")
Encoding(status_code) <- "UTF-8"
status <- c("Census division / Division de recensement", "County / Comté", "County",
            "District", "District municipality", "Municipalité régionale de comté",
            "Regional district", "Region", "Regional municipality", "Territoire équivalent",
            "Territory / Territoire", "United counties", "City / Cité", "Chartered community",
            "Community government", "Crown colony / Colonie de la couronne", "Community",
            "Canton (municipalité de)", "Cantons unis (municipalité de)", "City / Ville",
            "City", "District municipality", "Hamlet", "Improvement district",
            "Indian government district", "Island municipality",
            "Indian reserve / Réserve indienne", "Local government district",
            "Township and royalty", "Municipality / Municipalité", "Municipal district",
            "Municipalité", "Municipality", "Northern hamlet", "Nisga'a land",
            "Unorganized / Non organisé", "Northern village",
            "Parish / Paroisse (municipalité de)", "Paroisse (municipalité de)",
            "Rural community / Communauté rurale", "Regional district electoral area",
            "Regional municipality", "Rural municipality", "Resort village",
            "Indian settlement / Établissement indien", "Special area",
            "Subdivision of county municipality / Subdivision municipalité de comté",
            "Settlement / Établissement", "Settlement",
            "Self-government / Autonomie gouvernementale", "Specialized municipality",
            "Subdivision of unorganized / Subdivision non organisée", "Summer village",
            "Town", "Terres réservées aux Cris", "Terre inuite",
            "Terres réservées aux Naskapis", "Teslin land", "Township", "Town / Ville",
            "Ville", "Village cri", "Village naskapi", "Village", "Village nordique")
Encoding(status) <- "UTF-8"
CODES_TABLE <- dplyr::tibble(geography = geography, status_code = status_code, status = status)

usethis::use_data(CODES_TABLE, overwrite = TRUE)

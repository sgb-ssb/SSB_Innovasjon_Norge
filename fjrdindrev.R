

#### Pakker ####
library(haven)
library(readxl)
library(tidyverse)
library(quanteda)
library(tidytext)
library(stm)
library(rsample)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggfortify)
library(igraph)

options(scipen = 10000, encoding = "UTF-8")

#### DATA ####

# Last inn data med oversatte søknadstekster i excel-format.

soknader <- opus_data_sorter_oversatt_ferdig %>%
  mutate(Budsjettkommentar = ifelse(is.na(Budsjettkommentar), " ", Budsjettkommentar),
         Finansiering = ifelse(is.na(Finansiering), " ", Finansiering),
         nyhetsverdi1 = ifelse(is.na(nyhetsverdi1), " ", nyhetsverdi1),
         problemoglosning1 = ifelse(is.na(problemoglosning1), " ", problemoglosning1),
         problemoglosning2 = ifelse(is.na(problemoglosning2), " ", problemoglosning2),
         problemoglosning4 = ifelse(is.na(problemoglosning4), " ", problemoglosning4)) %>%
  mutate(text = str_c(Budsjettkommentar, Finansiering, nyhetsverdi1,
                      problemoglosning1, problemoglosning2, problemoglosning4)) %>%
  select(-c(Budsjettkommentar, Finansiering, nyhetsverdi1,
            problemoglosning1, problemoglosning2, problemoglosning4, `...1`)) %>%
  rename(org_nr = Organisasjonsnummer) %>%
  filter(ApplicationTypeName == "Markedsavklaringstilskudd") %>%
  mutate(aar = str_extract(Saknummer, "[0-9]{1,4}"))

corpus_soknader <- corpus(soknader) # Lager et corpus av søknadene for videre prosessering.


#### TOPIC MODELL ####

load("./soknader_g.rda")
load("./soknader_stm.rda")

# Disse temaene handler om den fjerde industrielle revolusjon (se egen tolkning):
plot(soknader_stm, topics = c(5, 34, 38, 57, 65, 73, 79, 81, 85), type = "summary", n = 10)

soknad_tibble <- as_tibble(list(text = corpus_soknader$documents$texts,
                                id = corpus_soknader$documents$SiebelID,
                                innovasjonsniva = corpus_soknader$documents$innovasjonsniva,
                                omsoktbelop = corpus_soknader$documents$omsoktbelop,
                                innvilgetbelop = corpus_soknader$documents$innvilgetbelop))

soknader_ladninger_alle <- as_tibble(soknader_stm$theta) %>%
  mutate(id = soknader_g$meta$SiebelID,
         saksnummer = soknader_g$meta$Saknummer,
         utfall = soknader_g$meta$Utfall,
         org_nr = soknader_g$meta$org_nr,
         aar = soknader_g$meta$aar,
         innovasjonstype = soknader_g$meta$Innovasjontype,
         prosjektorientering = soknader_g$meta$Prosjektorientering,
         vekstpotensiale = soknader_g$meta$Vekstpotensiale,
         kjennetegn = soknader_g$meta$Kjennetegn,
         virkemiddelbetegnelse = soknader_g$meta$Virkemiddelbetegnelse,
         prosjektnavn = soknader_g$meta$Prosjektnavn) %>%
  gather(colnames(.[,1:100]), key = "tema", value = "ladning") %>%
  mutate(org_nr = as.character(org_nr)) %>%
  left_join(soknad_tibble, by = "id")

soknader_ladninger <- soknader_ladninger_alle %>%
  mutate(tema = ifelse(tema == "V5", "datascience",
                       ifelse(tema == "V34", "bildegjenkjenning",
                              ifelse(tema == "V38", "droner",
                                     ifelse(tema == "V57", "digitaleplattformer",
                                            ifelse(tema == "V65", "kunstigintelligens",
                                                   ifelse(tema == "V73", "data",
                                                          ifelse(tema == "V79", "biologisk",
                                                                 ifelse(tema == "V81", "delingsokonomi",
                                                                        ifelse(tema == "V85", "robotisering",
                                                                               tema)))))))))) %>%
  filter(tema %in% c("datascience", # V5
                     "bildegjenkjenning", # V34
                     "droner", # V38
                     "digitaleplattformer", # V57
                     "kunstigintelligens", # V65
                     "data", # V73
                     "biologisk", # V79
                     "delingsokonomi", # V81
                     "robotisering" # V85
                     )) 

soknader_ladninger2 <- soknader_ladninger %>%
  spread(tema, ladning)

soknader_ladninger %>%
  group_by(tema) %>%
  summarise(median = median(ladning, na.rm = TRUE),
            gjennomsnitt = mean(ladning, na.rm = TRUE)) %>%
  ungroup()

hoyladning <- soknader_ladninger %>%
  group_by(tema) %>%
  filter(ladning >= mean(ladning)) %>%
  ungroup()

hoyladning <- hoyladning[,c("org_nr", "id", "aar", "tema", "ladning", "text", "saksnummer", "utfall", "omsoktbelop", "innvilgetbelop",
                            "kjennetegn", "prosjektnavn", "prosjektorientering",
                            "innovasjonstype", "innovasjonsniva", "virkemiddelbetegnelse", "vekstpotensiale")]


#### ANDEL ####

soknader_ladninger_alle %>%
  group_by(tema) %>%
  summarise(mean(ladning)) # Finner gjennomsnitt for alle tema

soknader_ladninger_alle %>%
  group_by(tema) %>%
  filter(ladning >= mean(ladning)) %>%
  summarise(n = n()) %>% 
  ggplot(aes(fct_reorder(tema, n), n)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_light()

soknader_ladninger_alle %>%
  group_by(tema) %>%
  filter(ladning >= mean(ladning)) %>%
  summarise(n = n()) %>% 
  ungroup() %>%
  mutate(sum = sum(n)) %>%
  spread(tema, n) %>%
  mutate(fjrdind = (V5 + V34 + V38 + V57 + V65 + V73 + V79 + V81 + V85)/sum*100) %>%
  pull(fjrdind)


#### FAKTORER ####

princompdata <- soknader_ladninger2 %>%
  select(id, datascience, data, robotisering, kunstigintelligens, bildegjenkjenning, droner,
         biologisk, delingsokonomi, digitaleplattformer) %>%
  column_to_rownames(var = "id")

pca_res <- prcomp(princompdata, scale = TRUE)

summary(pca_res)
varimax(pca_res$rotation)

autoplot(pca_res, data = princompdata, 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 6, loadings.label.colour = c("darkgreen"),
         loadings.colour = "darkgreen", alpha = 0.2) + theme_light()

# biologisk
# delingsokonomi og digitaleplattformer
# datascience og kunstigintelligens
# bildegjenkjenning og droner
# robotisering
# personvern

soknader_ladninger3 <- soknader_ladninger2 %>%
  mutate(kunstigintelligens = (kunstigintelligens+datascience)/2, # maskinlaring, ai
         digitaleplattformer = (digitaleplattformer+delingsokonomi)/2, # dele, samle
         fysiskegjenstander = (droner+bildegjenkjenning)/2) %>% # RFID, sensorer, droner
  select(-c(bildegjenkjenning, droner, delingsokonomi, datascience)) %>%
  gather(digitaleplattformer, kunstigintelligens, fysiskegjenstander, biologisk, data, robotisering,
         key = "tema", value = "ladning")

table(soknader_ladninger3$tema)


#### DENSITY ####

# Sammenlikner density for ikke-faktoriserte temaer
soknader_ladninger %>% # De fleste temaene lader veldig lavt
  ggplot(aes(ladning, fill = tema)) +
  geom_density(alpha = 0.3) + 
  ggtitle("Ladninger pa tema for alle soknader") +
  theme_light()

soknader_ladninger %>% # Velger vi ut temaer som lader over gjennomsnittet, er fordelingen smoothere
  group_by(tema) %>%
  filter(ladning >= mean(ladning)) %>%
  ungroup() %>%
  ggplot(aes(ladning, fill = tema)) +
  geom_density(alpha = 0.3) + 
  ggtitle("Ladninger pa tema for soknader som scorer over gjennomsnittet") +
  theme_light()

# Med density for faktoriserte temaer
soknader_ladninger3 %>% # Sjekker etter a ha innskrenket temaene
  rename("Tema" = "tema") %>%
  mutate(Tema = ifelse(Tema == "biologisk", "Biologisk",
                       ifelse(Tema == "data", "Data",
                              ifelse(Tema == "digitaleplattformer", "Digitale plattformer",
                                     ifelse(Tema == "kunstigintelligens", "Kunstig intelligens",
                                            ifelse(Tema == "robotisering", "Robotisering",
                                                   ifelse(Tema == "fysiskegjenstander", "Fysiske gjenstander", Tema))))))) %>%
  ggplot(aes(ladning, fill = Tema)) +
  geom_density(alpha = 0.4, show.legend = FALSE) + 
  labs(x = "Ladning", y = "Tetthet") +
  theme_light()

soknader_ladninger3 %>% 
  group_by(tema) %>%
  filter(ladning >= mean(ladning)) %>%
  ungroup() %>%
  rename("Tema" = "tema") %>%
  mutate(Tema = ifelse(Tema == "biologisk", "Biologisk",
                       ifelse(Tema == "data", "Data",
                              ifelse(Tema == "digitaleplattformer", "Digitale plattformer",
                                     ifelse(Tema == "kunstigintelligens", "Kunstig intelligens",
                                            ifelse(Tema == "robotisering", "Robotisering",
                                                   ifelse(Tema == "fysiskegjenstander", "Fysiske gjenstander", Tema))))))) %>%
  ggplot(aes(ladning, fill = Tema)) +
  geom_density(alpha = 0.4) + 
  labs(x = "Ladning", y = "Tetthet") +
  theme_light()


hoyladning <- soknader_ladninger3 %>%
  group_by(tema) %>%
  filter(ladning >= mean(ladning)) %>%
  ungroup()

hoyladning <- hoyladning[,c("org_nr", "id", "aar", "tema", "ladning", "text", "saksnummer", "utfall", "omsoktbelop", "innvilgetbelop",
                            "kjennetegn", "prosjektnavn", "prosjektorientering",
                            "innovasjonstype", "innovasjonsniva", "virkemiddelbetegnelse", "vekstpotensiale")]


# Henter sammendrag
alle <- summary(soknader_ladninger3$ladning) 
hoye <- summary(hoyladning$ladning) 
sammendrag <- rbind(alle, hoye) %>%
  round(7)

sammendrag

hoyladning %>%
  group_by(tema) %>%
  summarise(min = min(ladning),
            forstkvantil = quantile(ladning, 0.25),
            median = median(ladning),
            gjennomsnitt = mean(ladning),
            tredjekvantil = quantile(ladning, 0.75),
            max = max(ladning))

#### NETTVERK ####

dublett_tema <- hoyladning %>%
  group_by(org_nr) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  ungroup() %>%
  arrange(desc(n))

dublett_tema2 <- hoyladning %>%
  group_by(org_nr, tema) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  ungroup() %>%
  arrange(desc(n))

tema <- hoyladning %>% select(org_nr, tema)

dublett_tema3 <- anti_join(dublett_tema, dublett_tema2) %>%
  left_join(tema, by = "org_nr")

dublett_tema3 %>%
  na.omit() %>% # Teller opp hvor ofte orgnr faller innenfor hvert tema, med og uten NA
  group_by(tema) %>%
  summarise(n = n())

dublett_graph <- dublett_tema3 %>%
  na.omit() %>%
  mutate(org_nr = ifelse(is.na(org_nr), "missing", org_nr)) %>%
  unique() %>%
  select(tema, org_nr) %>%
  rename("Tema" = "tema") %>%
  mutate(Tema = ifelse(Tema == "biologisk", "Biologisk",
                       ifelse(Tema == "data", "Data",
                              ifelse(Tema == "digitaleplattformer", "Digitale plattformer",
                                     ifelse(Tema == "kunstigintelligens", "Kunstig intelligens",
                                            ifelse(Tema == "robotisering", "Robotisering",
                                                   ifelse(Tema == "fysiskegjenstander", "Fysiske gjenstander", Tema)))))))

graph  <- graph_from_data_frame(dublett_graph, directed = FALSE) # Making a graphed data frame that is undirected.
types  <- bipartite.mapping(graph)$type # Checking if the data is indeed bipartite (two-mode). Exctracting types.
matrix <- as_incidence_matrix(graph, types = types) # Making incidence matrix with the graphed data frame and types.
mode(matrix) <- "numeric" # Converting the matrix to numeric to avoid mode problems.

matrix_t <- tcrossprod(matrix)
diag(matrix_t) <- 0

g <- graph_from_adjacency_matrix(matrix_t, weighted = TRUE, mode = "undirected") 
g2 <- graph_from_adjacency_matrix(matrix_t, mode = "undirected") 

deg2 <- degree(g2, mode = "all")

simple <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
fr <- layout_with_fr(g)

# E(g)$weight

plot(simple,
     vertex.size = deg2*0.2, 
     vertex.color=rgb(0.1,0.7,0.8,0.5),
     edge.width = E(g)$weight*0.1, 
     layout = fr)
legend(x = -1.7, y = 1.4,
       c("Storrelse pa boble = Hvor mange foretak som handler om tema.",
         "Tykkelse pa strek = Hvor mange foretak som handler om begge temaer."), 
       pch = 20,
       col = "#777777",
       pt.cex = 2, cex = 0.8, bty = "n", ncol = 1)

#### MERGER DATA ####

# Data må søkes om tilgang til fra SSB.

vof20191 <- vof20191 %>% 
  select(-orgnrdat) %>% 
  rename(kommune = fkommune) 

regnskap <- regnskap %>% 
  select(-fnr_dat)
regnskap <- regnskap %>% 
  rename(org_nr = ORG_NR)
regnskap <- regnskap %>%
  rename(org_form = ORG_FORM)
regnskap <- regnskap %>%
  rename(navn = NAVN)

soknader_ladninger4 <- soknader_ladninger3 %>%
  spread(tema, ladning)

datamatch1 <- rbind(vof20191, regnskap) # Legger på 2019 i data (situasjonsuttak - det korresponderer ikke helt det heller)

datamatch2 <- left_join(soknader_ladninger4, datamatch1, by = c("org_nr", "aar")) # Legger på topic-ladninger fra skattefunnsøknadene, setter først pga. de er primære enheter

datamatch3 <- datamatch2 %>%
  mutate(nace = str_extract(nace1_sn07, "[0-9]{2}"))


#### DESKRIPTIV ANALYSE ####

datafaktor <- datamatch3 %>%
  mutate(fysiskegjenstander = ifelse(fysiskegjenstander >= mean(fysiskegjenstander), 1, 0),
         digitaleplattformer = ifelse(digitaleplattformer >= mean(digitaleplattformer), 1, 0),
         biologisk = ifelse(biologisk >= mean(biologisk), 1, 0),
         robotisering = ifelse(robotisering >= mean(robotisering), 1, 0),
         data = ifelse(data >= mean(data), 1, 0),
         kunstigintelligens = ifelse(kunstigintelligens >= mean(kunstigintelligens), 1, 0)) %>%
  select(utfall, fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         nace, syss, org_form, fylke, alder) %>%
  mutate(utfall = ifelse(utfall == "Innvilget", "Innvilget",
                         ifelse(is.na(utfall), NA,
                                "Avslatt"))) %>%
  mutate_at(c("fysiskegjenstander", "digitaleplattformer", "biologisk", "robotisering", "data", "kunstigintelligens"), as.factor) %>%
  mutate(utfall = factor(utfall, levels = c("Innvilget", "Avslatt")),
         fylke2 = ifelse(fylke == "01", "Ostfold",
                         ifelse(fylke == "02", "Akershus",
                                ifelse(fylke == "03", "Oslo",
                                       ifelse(fylke == "04", "Hedemark",
                                              ifelse(fylke == "05", "Oppland",
                                                     ifelse(fylke == "06", "Buskerud",
                                                            ifelse(fylke == "07", "Oppland",
                                                                   ifelse(fylke == "08", "Telemark",
                                                                          ifelse(fylke == "09", "Aust Agder",
                                                                                 ifelse(fylke == "10", "Vest Agder",
                                                                                        ifelse(fylke == "11", "Rogaland",
                                                                                               ifelse(fylke == "12", "Hordaland",
                                                                                                      ifelse(fylke == "14", "Sogn og Fjordane",
                                                                                                             ifelse(fylke == "15", "More og Romsdal",
                                                                                                                    ifelse(fylke == "16", "Sor Trondelag",
                                                                                                                           ifelse(fylke == "17", "Nord Trondelag",
                                                                                                                                  ifelse(fylke == "18", "Nordland",
                                                                                                                                         ifelse(fylke == "19", "Troms",
                                                                                                                                                ifelse(fylke == "20", "Finnmark",
                                                                                                                                                       ifelse(fylke == "50", "Trondelag",
                                                                                                                                                              NA)))))))))))))))))))),
         
         fylke2 = as.factor(fylke2),
         org_form = as.factor(org_form),
         syss = as.numeric(syss),
         alder = as.numeric(syss),
         nace2 = ifelse(nace %in% c("01", "02", "03"), "Jordbruk",
                        ifelse(nace %in% c("05", "06", "07", "08", "09"), "Bergverksdrift og utvinning",
                               ifelse(nace %in% c("10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                                  "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31",
                                                  "32", "33"), "Industri",
                                      ifelse(nace %in% c("35"), "Elektrisitets-, gass-, damp- og varmtvannsforsyning",
                                             ifelse(nace %in% c("36", "37", "38", "39"), "Vannforsyning, avlops- og renovasjonsvirksomhet",
                                                    ifelse(nace %in% c("41", "42", "43"), "Bygge og anleggsvirksomhet",
                                                           ifelse(nace %in% c("45", "46", "47"), "Varehandel, reparasjon av motorvogner",
                                                                  ifelse(nace %in% c("49", "50", "51", "52", "53"), "Transport og lagring",
                                                                         ifelse(nace %in% c("55", "56"), "Overnatting og servicevirksomhet",
                                                                                ifelse(nace %in% c("58", "59", "60", "61", "62", "63"), "Informasjon og kommunikasjon",
                                                                                       ifelse(nace %in% c("64", "65", "66"), "Finansierings- og forsikringsvirksomhet",
                                                                                              ifelse(nace %in% c("68"), "Omsetning og drift av fast eiendom",
                                                                                                     ifelse(nace %in% c("69", "70", "71", "72", "73", "74", "75"), "Faglig, vitenskapelig og teknisk tjenesteyting",
                                                                                                            ifelse(nace %in% c("77", "78", "79", "80", "81", "82"), "Forretningsmessig tjenesteyting",
                                                                                                                   ifelse(nace %in% c("84"), "Offentlig administrasjon og forsvar",
                                                                                                                          ifelse(nace %in% c("85"), "Undervisning",
                                                                                                                                 ifelse(nace %in% c("86", "87", "88"), "Helse og omsorgstjenester",
                                                                                                                                        ifelse(nace %in% c("90", "91", "92", "93"), "Kulturell virksomhet, underholdning og fritidsaktiviteter",
                                                                                                                                               ifelse(nace %in% c("94", "95", "96"), "Annen tjenesteyting",
                                                                                                                                                      ifelse(nace %in% c("97"), "Lønnet arbeid i private husholdninger",
                                                                                                                                                             ifelse(nace %in% c("99"), "Internasjonale organisasjoner og organer",
                                                                                                                                                                    nace))))))))))))))))))))),
         nace2 = as.factor(nace2))

datafaktor %>%
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(utfall, Tema, Ladning) %>%
  summarise(n = n()) %>%
  spread(Ladning, n) %>%
  ungroup() %>%
  group_by(utfall) %>%
  summarise(ikkefjrdind = sum(`0`, na.rm = TRUE),
            fjrdind = sum(`1`, na.rm = TRUE)) %>%
  ungroup() %>%
  summarise(sum = sum(fjrdind)) # Antall soknader som handler om den fjerde industrielle revolusjon: 1321

# FYLKE

# Prosentandel av søknader som handler om den fjerde industrielle revolusjon per fylke
datafaktor %>%
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(fylke2, Tema, Ladning) %>%
  summarise(n = n()) %>%
  spread(Ladning, n) %>%
  ungroup() %>%
  group_by(fylke2) %>%
  summarise(ikkefjrdind = sum(`0`, na.rm = TRUE),
            fjrdind = sum(`1`, na.rm = TRUE)) %>%
  mutate(pros = fjrdind/(ikkefjrdind+fjrdind)*100)

(291/1321)*100 # Andel soknader om den fjerde industrielle revolusjon som kommer fra Oslo: 22 %
(129/1321)*100 # Andel soknader om den fjerde industrielle revolusjon som kommer fra Rogaland: 9,8 %
(116/1321)*100 # Andel soknader om den fjerde industrielle revolusjon som kommer fra Akershus: 8,8 %
(111/1321)*100 # Andel soknader om den fjerde industrielle revolusjon som kommer fra Hordaland: 8,4 %


# Prosentandel av søknader som handler om temaer tilknyttet den fjerde industrielle revolusjon per fylke
datafaktor %>%
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(fylke2, Tema, Ladning) %>%
  summarise(n = n()) %>%
  spread(Ladning, n) %>%
  summarise(pros = `1`/(`0`+`1`)*100) %>%
  ungroup() %>%
  filter(fylke2 == "Oslo")

fylkedata <- datafaktor %>%
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(fylke2, Tema, Ladning) %>%
  summarise(n = n()) %>%
  filter(Ladning == "1") %>%
  ungroup() 

fylkedata %>%
  na.omit() %>%
  mutate(Tema = ifelse(Tema == "biologisk", "Biologisk",
                       ifelse(Tema == "data", "Data",
                              ifelse(Tema == "digitaleplattformer", "Digitale plattformer",
                                     ifelse(Tema == "fysiskegjenstander", "Fysiske gjenstander",
                                            ifelse(Tema == "kunstigintelligens", "Kunstig intelligens",
                                                   ifelse(Tema == "robotisering", "Robotisering", Tema))))))) %>%
  ggplot(aes(fct_reorder(fylke2, n), n, fill = Tema)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(x = "", y = "Antall soknader som handler om den fjerde industrielle revolusjon",
       caption = ("Utelukker 249 missingverdier der vi ikke vet fylke.")) +
  scale_fill_manual(breaks = c("Biologisk", "Data", "Digitale plattformer", "Fysiske gjenstander", "Kunstig intelligens", "Robotisering"),
                    values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")) +
  theme_light()


# NACE

# Prosentandel av søknader som handler om den fjerde industrielle revolusjon per nace
datafaktor %>%
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(nace2, Tema, Ladning) %>%
  summarise(n = n()) %>%
  spread(Ladning, n) %>%
  ungroup() %>%
  group_by(nace2) %>%
  summarise(ikkefjrdind = sum(`0`, na.rm = TRUE),
            fjrdind = sum(`1`, na.rm = TRUE)) %>%
  mutate(pros = fjrdind/(ikkefjrdind+fjrdind)*100)

(506/1321)*100 # Andel soknader om den fjerde industrielle revolusjon i nacekode informasjon og kommunikasjon: 38,3 %
(329/1321)*100 # Andel soknader om den fjerde industrielle revolusjon i nacekode faglig, vitenskapelig og teknisk tjenesteyting: 24,9 %

nacedata <- datafaktor %>%
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(nace2, Tema, Ladning) %>%
  #mutate(nace2 = ifelse(nace2 == "00", NA, nace2)) %>%
  summarise(n = n()) %>%
  filter(Ladning == "1") %>%
  ungroup() 

nacedata %>%
  na.omit() %>%
  mutate(Tema = ifelse(Tema == "biologisk", "Biologisk",
                       ifelse(Tema == "data", "Data",
                              ifelse(Tema == "digitaleplattformer", "Digitale plattformer",
                                     ifelse(Tema == "fysiskegjenstander", "Fysiske gjenstander",
                                            ifelse(Tema == "kunstigintelligens", "Kunstig intelligens",
                                                   ifelse(Tema == "robotisering", "Robotisering", Tema))))))) %>%
  ggplot(aes(fct_reorder(nace2, n), n, fill = Tema)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(x = "", y = "Antall soknader som handler om den fjerde industrielle revolusjon",
       caption = "Utelukker 250 missingverdier der vi ikke vet naeringskode.") +
  scale_fill_manual(breaks = c("Biologisk", "Data", "Digitale plattformer", "Fysiske gjenstander", "Kunstig intelligens", "Robotisering"),
                    values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")) +
  theme_light()



# ORG_FORM

# Prosentandel av søknader som handler om den fjerde industrielle revolusjon per orgform
datafaktor %>%
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(org_form, Tema, Ladning) %>%
  summarise(n = n()) %>%
  spread(Ladning, n) %>%
  ungroup() %>%
  group_by(org_form) %>%
  summarise(ikkefjrdind = sum(`0`, na.rm = TRUE),
            fjrdind = sum(`1`, na.rm = TRUE)) %>%
  mutate(pros = fjrdind/(ikkefjrdind+fjrdind)*100)

(841/1321)*100 # Andel soknader om den fjerde industrielle revolusjon i orgform AS: 64 %
(212/1321)*100 # Andel soknader om den fjerde industrielle revolusjon i orgform ENK: 16 %%


orgformdata <- datafaktor %>%
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(org_form, Tema, Ladning) %>%
  summarise(n = n()) %>%
  filter(Ladning == "1") %>%
  ungroup() 

orgformdata %>%
  na.omit() %>%
  mutate(Tema = ifelse(Tema == "biologisk", "Biologisk",
                       ifelse(Tema == "data", "Data",
                              ifelse(Tema == "digitaleplattformer", "Digitale plattformer",
                                     ifelse(Tema == "fysiskegjenstander", "Fysiske gjenstander",
                                            ifelse(Tema == "kunstigintelligens", "Kunstig intelligens",
                                                   ifelse(Tema == "robotisering", "Robotisering", Tema))))))) %>%
  ggplot(aes(fct_reorder(org_form, n), n, fill = Tema)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(x = "", y = "Antall soknader som handler om den fjerde industrielle revolusjon",
       caption = "Utelukker 249 missingverdier der vi ikke vet organisasjonsform.") +
  scale_fill_manual(breaks = c("Biologisk", "Data", "Digitale plattformer", "Fysiske gjenstander", "Kunstig intelligens", "Robotisering"),
                    values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")) +
  theme_light()


# ALDER

# Prosentandel av søknader som handler om den fjerde industrielle revolusjon per alder
datafaktor %>%
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(alder, Tema, Ladning) %>%
  summarise(n = n()) %>%
  spread(Ladning, n) %>%
  ungroup() %>%
  group_by(alder) %>%
  summarise(ikkefjrdind = sum(`0`, na.rm = TRUE),
            fjrdind = sum(`1`, na.rm = TRUE)) %>%
  mutate(pros = fjrdind/(ikkefjrdind+fjrdind)*100)

(284/1321)*100 # Andel soknader om den fjerde industrielle revolusjon som er 0 ar: 21,4 %
(72/1321)*100 # Andel soknader om den fjerde industrielle revolusjon som er 1 ar: 5,5 %
(29/1321)*100 # Andel soknader om den fjerde industrielle revolusjon som er 2 ar: 2,2 % 
(14/1321)*100 # Andel soknader om den fjerde industrielle revolusjon som er 3 ar: 1,1 %

alderdata <- datafaktor %>%
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(alder, Tema, Ladning) %>%
  summarise(n = n()) %>%
  filter(Ladning == "1") %>%
  ungroup() 

alderdata %>%
  na.omit() %>%
  mutate(alder = as.factor(alder)) %>%
  mutate(Tema = ifelse(Tema == "biologisk", "Biologisk",
                       ifelse(Tema == "data", "Data",
                              ifelse(Tema == "digitaleplattformer", "Digitale plattformer",
                                     ifelse(Tema == "fysiskegjenstander", "Fysiske gjenstander",
                                            ifelse(Tema == "kunstigintelligens", "Kunstig intelligens",
                                                   ifelse(Tema == "robotisering", "Robotisering", Tema))))))) %>%
  ggplot(aes(alder, n, fill = Tema)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(x = "", y = "Antall soknader som handler om den fjerde industrielle revolusjon",
       caption = "Utelukker 903 missingverdier der vi ikke vet alder pa foretaket") +
  scale_fill_manual(breaks = c("Biologisk", "Data", "Digitale plattformer", "Fysiske gjenstander", "Kunstig intelligens", "Robotisering"),
                    values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")) +
  theme_light()


# UTFALL

# Prosentandel av søknader som handler om den fjerde industrielle revolusjon per utfall (med og uten NA)
datafaktor %>%
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(utfall, Tema, Ladning) %>%
  summarise(n = n()) %>%
  spread(Ladning, n) %>%
  ungroup() %>%
  group_by(utfall) %>%
  summarise(ikkefjrdind = sum(`0`, na.rm = TRUE),
            fjrdind = sum(`1`, na.rm = TRUE)) %>%
  mutate(pros = fjrdind/(ikkefjrdind+fjrdind)*100)

## MED NA
(655/1321)*100 # Andel soknader om den fjerde industrielle revolusjon som ble innvilget: 49,6 %
(543/1321)*100 # Andel soknader om den fjerde industrielle revolusjon som ble avslatt: 41,1 %
(123/1321)*100 # Andel soknader om den fjerde industrielle revolusjon der vi ikke vet utfall: 9,3 %

## UTEN NA
(655/(655+543))*100 # Andel soknader om den fjerde industrielle revolusjon som ble innvilget: 54,7 %
(543/(655+543))*100 # Andel soknader om den fjerde industrielle revolusjon som ble avslatt: 45,3 %


# Prosentandel av søknader som ble innvilget/ikke innvilget og 
# handler om/handler ikke om den fjerde industrielle revolusjon per utfall uten NA
datafaktor %>%
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(utfall, Tema, Ladning) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(utfall == "Innvilget") %>%
  spread(Ladning, n) %>%
  mutate(ikkefjrdind = sum(`0`, na.rm = TRUE),
         fjrdind = sum(`1`, na.rm = TRUE)) %>%
  mutate(pros_fjrdind = (`1`/fjrdind)*100,
         pros_ikkefjrdind = (`0`/ikkefjrdind)*100)

# Andel innvilget per tema
datafaktor %>%
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(utfall, Tema, Ladning) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  na.omit() %>%
  spread(utfall, n) %>%
  filter(Ladning == 1) %>%
  #mutate(pros = (Innvilget/(Innvilget+Avslatt))*100) %>% 
  #arrange(desc(pros)) %>%
  mutate(pros = (`1`/`0`)*100)

# Andel som innvilges i helhet
datafaktor %>%
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(Tema, Ladning, utfall) %>%
  summarise(n = n()) %>%
  mutate(Ladning = ifelse(Ladning == 0, "Ikke fjrd.indr.rev", # Handler ikke om tema
                          "Fjrd.indr.rev")) %>%
  na.omit() %>%
  spread(utfall, n) %>%
  ungroup() %>%
  summarise(sum_innvilget = sum(Innvilget, na.rm = TRUE),
            sum_avslatt = sum(Avslatt, na.rm = TRUE)) %>%
  mutate(pros = sum_innvilget/(sum_innvilget+sum_avslatt))

utfalldata <- datafaktor %>% 
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(Tema, Ladning, utfall) %>%
  summarise(n = n()) %>%
  mutate(Ladning = ifelse(Ladning == 0, "Ikke fjrd.indr.rev", # Handler ikke om tema
                          "Fjrd.indr.rev")) %>% # Handler om tema
  ungroup() %>%
  mutate(Tema = ifelse(Ladning == "Ikke fjrd.indr.rev", "Ingen", Tema))

utfalldata %>%
  na.omit() %>%
  mutate(Tema = ifelse(Tema == "biologisk", "Biologisk",
                       ifelse(Tema == "data", "Data",
                              ifelse(Tema == "digitaleplattformer", "Digitale plattformer",
                                     ifelse(Tema == "fysiskegjenstander", "Fysiske gjenstander",
                                            ifelse(Tema == "kunstigintelligens", "Kunstig intelligens",
                                                   ifelse(Tema == "robotisering", "Robotisering", Tema))))))) %>%
  ggplot(aes(Ladning, n, fill = Tema)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(cols = vars(utfall)) +
  labs(x = "", y = "Antall soknader",
       caption = "Utelukker 1092 missingverdier der vi ikke vet utfall av soknaden. 123 av disse handlet om fjrd.indr.rev.") + 
  scale_fill_manual(breaks = c("Biologisk", "Data", "Digitale plattformer", "Fysiske gjenstander", "Kunstig intelligens", "Robotisering", "Ingen"),
                    values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "lightgray")) +
  theme_light()

datafaktor %>% 
  gather(fysiskegjenstander, digitaleplattformer, biologisk, robotisering, data, kunstigintelligens,
         key = "Tema", value = "Ladning") %>%
  group_by(Tema, Ladning, utfall) %>%
  summarise(n = n()) %>%
  mutate(Ladning = ifelse(Ladning == 0, "Ikke fjrd.indr.rev", # Handler ikke om tema
                          "Fjrd.indr.rev")) %>% # Handler om tema
  ungroup() %>%
  na.omit() %>%
  spread(Ladning, n) %>%
  filter(utfall == "Innvilget") %>%
  mutate(pros = (Fjrd.indr.rev/(Fjrd.indr.rev+`Ikke fjrd.indr.rev`))*100) %>%
  arrange(desc(pros))

50/(50+1256+121+1185+216+1090+97+1209+135+1171+36+1270)*100 # Innvilgede søkander, biologisk: 48,5 %
1185/(1185+1308)*100 # Innvilgede søknader, data: 47,5 %
1090/(1090+1072)*100 # Innvilgede søknader, digitale plattformer: 50,4 %
1209/(1209+1304)*100 # Innvilgede søknader, fysiske gjenstander: 48,1 %
1171/(1171+1300)*100 # Innvilgede søknader, kunstig intelligens: 47,4 %
1270/(1270+1347)*100 # Innvilgede søknader, robotisering: 48,5 %


#### MODELLER ####

## REGRESJONSMODELL ##

mod1 <- glm(as.factor(utfall) ~ data + biologisk + digitaleplattformer + fysiskegjenstander + kunstigintelligens + robotisering +
              nace2 + 
              org_form + 
              fylke2,
            na.action = "na.exclude",
            family = binomial("logit"),
            data = datafaktor)

stargazer::stargazer(mod1,
                     type = "text", omit = c("nace2", "fylke2", "org_form"),
                     covariate.labels = c("Data", "Biologisk", "Digitale plattformer",
                                          "Fysiske gjenstander", "Kunstig intelligens", "Robotisering"),
                     dep.var.caption = "Utfall av soknad", dep.var.labels = "Innvilget",
                     notes = "Kontrollvariabler: Naeringskode, fyle, organisasjonsform.")

datafaktor <- datafaktor %>% 
  mutate(predicted_glm = predict(mod1, type = "response"))

datafaktor %>%
  gather(data, biologisk, digitaleplattformer, fysiskegjenstander, kunstigintelligens, robotisering,
         key = "Tema", value = "Ladning") %>%
  mutate(Ladning = ifelse(Ladning == 0, "Handler ikke om tema",
                          ifelse(Ladning == 1, "Handler om tema",
                                 Ladning))) %>%
  rename(" " = "Ladning") %>%
  mutate(Tema = ifelse(Tema == "robotisering", "Robotisering",
                       ifelse(Tema == "kunstigintelligens", "Kunstig intelligens",
                              ifelse(Tema == "fysiskegjenstander", "Fysiske gjenstander",
                                     ifelse(Tema == "digitaleplattformer", "Digitale plattformer",
                                            ifelse(Tema == "data", "Data", 
                                                   ifelse(Tema == "biologisk", "Biologisk", Tema))))))) %>%
  ggplot(aes(predicted_glm, Tema, color = ` `)) + 
  geom_boxplot(alpha = 0.8) +
  scale_color_manual(breaks = c("Handler ikke om tema", "Handler om tema"),
                    values = c("#000000", "#E69F00")) +
  labs(x = "Sjanse for a fa innvilget soknad", y = "") +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "gray"),
        panel.grid.major = element_line(color = "lightgray"))


## RANDOM FOREST ##

set.seed(393)

min <- min(table(datafaktor$utfall)) # Minimum frekvens, dvs. antall ganger noen fikk invilget søknaden, 1306.

mod2 <- randomForest(factor(utfall) ~ data + biologisk + digitaleplattformer + fysiskegjenstander + kunstigintelligens + robotisering +
                       nace2 + 
                       org_form + 
                       fylke2, 
                     data = datafaktor,
                     na.action = na.roughfix,
                     type = "classification",
                     mtry = 6,
                     ntree = 1000,
                     sampsize = c(min,min),
                     importance = TRUE)

datafaktor <- datafaktor %>% 
  mutate(predicted_randomforest = predict(mod2))

caret::confusionMatrix(datafaktor$predicted_randomforest, datafaktor$utfall, positive = "Innvilget")

importance(mod2) %>%
  as_tibble() %>%
  mutate(Tema = c("Data", "Biologisk", "Digitale plattformer", "Fysiske gjenstander", "Kunstig intelligens",
                  "Robotisering", "Naeringskode", "Organisasjonsform", "Fylke")) %>%
  #gather(Innvilget, Avslatt, key = "Utfall", value = "value") %>%
  ggplot(aes(fct_reorder(Tema, MeanDecreaseAccuracy), MeanDecreaseAccuracy, fill = Tema)) + 
  geom_bar(stat = "identity") + 
  labs(x = "", y = "Variabelviktighet") +
  coord_flip() + 
  scale_fill_manual(breaks = c("Biologisk", "Data", "Digitale plattformer", "Fysiske gjenstander", "Kunstig intelligens", "Robotisering",
                               "Naeringskode", "Organisasjonsform", "Fylke"),
                    values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                               "#999999", "#D55E00", "#CC79A7")) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "gray"),
        panel.grid.major = element_line(color = "lightgray"))
  #ggplot(aes(Tema, value, fill = Utfall)) + 
  #geom_bar(stat = "identity") + 
  #facet_grid(cols = vars(Utfall)) +
  #scale_fill_manual(breaks = c("Avslatt", "Innvilget"),
  #                  values = c("#000000", "#E69F00")) +
  #coord_flip() + 
  #labs(x = "", y = "Prediksjonskraft pa utfall") +
  #theme(legend.position = "top",
  #      panel.background = element_rect(fill = "white"),
  #      axis.line.x = element_line(color = "gray"),
  #      panel.grid.major = element_line(color = "lightgray"))

varImpPlot(mod2, type = 1,
           main = "Viktighet av hver variabel pa a fa innvilget soknad")
varImpPlot(mod2, type = 2,
           main = "Viktighet av hver variabel pa a fa innvilget soknad")

par(mfrow = c(1,1))

partialPlot(mod2, pred.data = as.data.frame(na.omit(datafaktor)), x.var = kunstigintelligens, which.class = "Innvilget")
partialPlot(mod2, pred.data = as.data.frame(na.omit(datafaktor)), x.var = data, which.class = "Innvilget")
partialPlot(mod2, pred.data = as.data.frame(na.omit(datafaktor)), x.var = digitaleplattformer, which.class = "Innvilget")
partialPlot(mod2, pred.data = as.data.frame(na.omit(datafaktor)), x.var = fysiskegjenstander, which.class = "Innvilget")
partialPlot(mod2, pred.data = as.data.frame(na.omit(datafaktor)), x.var = biologisk, which.class = "Innvilget")
partialPlot(mod2, pred.data = as.data.frame(na.omit(datafaktor)), x.var = robotisering, which.class = "Innvilget")

partialPlot(mod2, pred.data = as.data.frame(na.omit(datafaktor)), x.var = fylke2, which.class = "Innvilget")
partialPlot(mod2, pred.data = as.data.frame(na.omit(datafaktor)), x.var = nace2, which.class = "Innvilget")
partialPlot(mod2, pred.data = as.data.frame(na.omit(datafaktor)), x.var = org_form, which.class = "Innvilget")

fylke_rf_plot <- partialPlot(mod2, pred.data = as.data.frame(na.omit(datafaktor)), x.var = fylke2, which.class = "Innvilget")
fylke_rf_plot %>% as_tibble() %>% rename("fylke" = "x") %>%
  ggplot(aes(fylke, y)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(x = "", y = "Sjanse (likelihood) for a fa innvilget soknad") +
  theme_light()

nace_rf_plot <- partialPlot(mod2, pred.data = as.data.frame(na.omit(datafaktor)), x.var = nace2, which.class = "Innvilget")
nace_rf_plot %>% as_tibble() %>% rename("nace" = "x") %>%
  ggplot(aes(nace, y)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(x = "", y = "Sjanse (likelihood) for a fa innvilget soknad") +
  theme_light()



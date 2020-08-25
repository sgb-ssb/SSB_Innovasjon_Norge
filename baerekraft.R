
#### Pakker ####
library(haven)
library(readxl)
library(tidyverse)
library(quanteda)
library(tidytext)
library(stm)
library(igraph)
library(rsample)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggfortify)


#### DATA ####

# Last inn søknader fra excel-fil.

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

# Tolkning av output fra topic modell tyder på at tema 7, 10, 11, 21, 33, 44, 61 og 80 gir uttrykk for ulike bærekraftstemaer knyttet til miljø.

plot(soknader_stm, topics = c(7, 10, 11, 21, 33, 44, 61, 80), type = "summary", n = 10)

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
  mutate(tema = ifelse(tema == "V7", "fornybar", # Fornybar energi 
                       ifelse(tema == "V10", "samf", # Samfunnsorientert, utviklingsland
                              ifelse(tema == "V11", "deling", # Elektrisk mobilitet, delingsøkonomi
                                     ifelse(tema == "V21", "transport", # Transport, utslipp, energi
                                            ifelse(tema == "V33", "materialer", # Materialer
                                                   ifelse(tema == "V44", "plast", # Hav, plast i havet
                                                          ifelse(tema == "V61", "tekstil", # Klær, gjenbruk/bytte
                                                                 ifelse(tema == "V80", "vann", # Vann, forurensing
                                                                        tema))))))))) %>%
  filter(tema %in% c("fornybar", 
                     "samf", 
                     "deling", 
                     "transport", 
                     "materialer", 
                     "plast", 
                     "tekstil", 
                     "vann" 
                     ))
  

hoyladning <- soknader_ladninger %>%
  group_by(tema) %>%
  filter(ladning >= mean(ladning)) %>%
  ungroup()

hoyladning <- hoyladning[,c("org_nr", "id", "aar", "tema", "ladning", "text", "saksnummer", "utfall", "omsoktbelop", "innvilgetbelop",
                            "kjennetegn", "prosjektnavn", "prosjektorientering",
                            "innovasjonstype", "innovasjonsniva", "virkemiddelbetegnelse", "vekstpotensiale")]


#### SAMMENDRAG ####

soknader_ladninger %>%
  group_by(tema) %>%
  summarise(mean(ladning))

soknader_ladninger %>% # De fleste temaene lader veldig lavt
  ggplot(aes(ladning, fill = tema)) +
  geom_density(alpha = 0.3) + 
  ggtitle("Ladninger pa tema for alle soknader") +
  theme_light()

hoyladning %>% # Velger vi ut temaer som lader over gjennomsnittet, er fordelingen smoothere
  rename("Tema" = "tema") %>%
  mutate(Tema = ifelse(Tema == "fornybar", "Fornybar energi",
                       ifelse(Tema == "samf", "Samfunnsansvar og utvikling",
                              ifelse(Tema == "deling", "Bil, utslipp og energi",
                                     ifelse(Tema == "materialer", "Materialer",
                                            ifelse(Tema == "plast", "Marin forurensing",
                                                   ifelse(Tema == "tekstil", "Gjenbruk og bytte",
                                                          ifelse(Tema == "vann", "Rensing av vann og luft",
                                                                 Tema)))))))) %>%
  ggplot(aes(ladning, fill = Tema)) +
  geom_density(alpha = 0.3) + 
  labs(x = "Ladning", y = "Tetthet") +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "gray"),
        panel.grid.major = element_line(color = "lightgray"))

Alle <- summary(soknader_ladninger$ladning) 
Over_gjennomsnittet <- summary(hoyladning$ladning) 
sammendrag <- rbind(Alle, Over_gjennomsnittet) %>%
  round(7)

sammendrag

sammendrag2 <- hoyladning %>%
  rename("Tema" = "tema") %>%
  mutate(Tema = ifelse(Tema == "fornybar", "Fornybar energi",
                       ifelse(Tema == "samf", "Samfunnsansvar og utvikling",
                              ifelse(Tema == "deling", "Bil, utslipp og energi",
                                     ifelse(Tema == "transport", "Energi og utslipp",
                                            ifelse(Tema == "materialer", "Materialer",
                                                   ifelse(Tema == "plast", "Marin forurensing",
                                                          ifelse(Tema == "tekstil", "Gjenbruk og bytte",
                                                                 ifelse(Tema == "vann", "Rensing av vann og luft",
                                                                        Tema))))))))) %>%
  group_by(Tema) %>%
  summarise(Min = min(ladning),
            Forstkvantil = quantile(ladning, 0.25),
            Median = median(ladning),
            Gjennomsnitt = mean(ladning),
            Tredjekvantil = quantile(ladning, 0.75),
            Max = max(ladning))

soknader_ladninger_alle %>%
  group_by(tema) %>%
  filter(ladning >= mean(ladning)) %>%
  summarise(n = n()) %>% 
  ungroup() %>%
  mutate(sum = sum(n)) %>%
  spread(tema, n) %>%
  mutate(barekraft = (V7 + V10 + V11 + V21 + V33 + V44 + V61 + V80)/sum*100) %>%
  pull(barekraft)


#### KONTROLLVARIABLER ####

# Disse dataene må det søkes om tilgang til fra SSB.

## VOF ##

# Må bruke virksomhets- og foretaksregisteret for å få data fra 2019. Dette er et situasjonsuttak for januar.

vof20191 <- vof2019 %>%
  mutate(aar = "2019") %>%
  filter(org_form != "BEDR") %>%
  mutate(org_nr = ifelse(org_nr == "", NA, org_nr)) %>%
  drop_na(org_nr) %>%
  mutate(fylke = str_extract(fkommune, "[0-9]{1,2}")) %>%
  select(org_nr, navn, orgnrdat, nace1_sn07, syss, org_form, fylke, fkommune, oms, aar) %>%
  mutate(opprettet = (str_extract(orgnrdat, "[0-9]{1,4}"))) %>%
  mutate(alder = as.numeric(aar)-as.numeric(opprettet))

## REGNSKAP ##

# Her finner vi følgende variabler: næringskode, bedriftsstørrelse, bedriftsform, alder og lokasjon

regnskap_vof_20131 <- regnskap_vof_2013 %>%
  select(org_nr, NAVN, fnr_dat, nace1_sn07, syss, ORG_FORM, fylke, kommune, oms, aar) %>%
  mutate(opprettet = (str_extract(fnr_dat, "[0-9]{1,4}"))) %>%
  mutate(alder = as.numeric(aar)-as.numeric(opprettet))

regnskap_vof_20141 <- regnskap_vof_2014 %>%
  select(org_nr, NAVN, fnr_dat, nace1_sn07, syss, ORG_FORM, fylke, kommune, oms, aar) %>%
  mutate(opprettet = (str_extract(fnr_dat, "[0-9]{1,4}"))) %>%
  mutate(alder = as.numeric(aar)-as.numeric(opprettet))

regnskap_vof_20151 <- regnskap_vof_2015 %>%
  select(org_nr, NAVN, fnr_dat, nace1_sn07, syss, ORG_FORM, fylke, kommune, oms, aar) %>%
  mutate(opprettet = (str_extract(fnr_dat, "[0-9]{1,4}"))) %>%
  mutate(alder = as.numeric(aar)-as.numeric(opprettet))

regnskap_vof_20161 <- regnskap_vof_2016 %>%
  select(org_nr, NAVN, fnr_dat, nace1_sn07, syss, ORG_FORM, fylke, kommune, oms, aar) %>%
  mutate(opprettet = (str_extract(fnr_dat, "[0-9]{1,4}"))) %>%
  mutate(alder = as.numeric(aar)-as.numeric(opprettet))

regnskap_vof_20171 <- regnskap_vof_2017 %>%
  select(org_nr, NAVN, fnr_dat, nace1_sn07, syss, ORG_FORM, fylke, kommune, oms, aar) %>%
  mutate(opprettet = (str_extract(fnr_dat, "[0-9]{1,4}"))) %>%
  mutate(alder = as.numeric(aar)-as.numeric(opprettet))

regnskap_vof_20181 <- regnskap_vof_2018 %>%
  select(org_nr, NAVN, fnr_dat, nace1_sn07, syss, ORG_FORM, fylke, kommune, oms, aar) %>%
  mutate(opprettet = (str_extract(fnr_dat, "[0-9]{1,4}"))) %>%
  mutate(alder = as.numeric(aar)-as.numeric(opprettet))

regnskap <- rbind(regnskap_vof_20131, regnskap_vof_20141, regnskap_vof_20151,
                  regnskap_vof_20161, regnskap_vof_20171, regnskap_vof_20181)



#### FAKTORER ####

soknader_ladninger2 <- soknader_ladninger %>%
  spread(tema, ladning)

princompdata <- soknader_ladninger2 %>%
  select(id, fornybar, materialer, plast, samf, tekstil, transport, vann, deling) %>%
  rename("Fornybar energi" = "fornybar") %>%
  rename("Samfunnsansvar og utvikling" = "samf") %>%
  rename("Bil, utslipp og energi" = "deling") %>%
  rename("Energi og utslipp" = "transport") %>%
  rename("Materialer" = "materialer") %>%
  rename("Marin forurensing" = "plast") %>%
  rename("Gjenbruk og bytte" = "tekstil") %>%
  rename("Rensing av vann og luft" = "vann") %>%
  column_to_rownames(var = "id")

pca_res <- prcomp(princompdata, scale = TRUE)

summary(pca_res)
varimax(pca_res$rotation)

autoplot(pca_res, data = princompdata, 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 6, loadings.label.colour = c("darkgreen"),
         loadings.colour = "darkgreen", alpha = 0.2) + theme_light()

soknader_ladninger3 <- soknader_ladninger2 %>%
  mutate(fornybar = (transport+fornybar)/2) %>% 
  select(-transport) %>%
  gather(fornybar, materialer, deling, plast, samf, tekstil, vann,
         key = "tema", value = "ladning")

table(soknader_ladninger3$tema)

hoyladning <- soknader_ladninger3 %>%
  group_by(tema) %>%
  filter(ladning >= mean(ladning)) %>%
  ungroup()


#### MERGER DATA ####

vof20191 <- vof20191 %>% 
  select(-orgnrdat) %>% 
  rename(kommune = fkommune) 

regnskap <- regnskap %>% 
  select(-fnr_dat) 

regnskap <- regnskap %>%
  rename("org_nr" = "ORG_NR")

regnskap <- regnskap %>%
  rename("org_form" = "ORG_FORM")

regnskap <- regnskap %>%
  rename("navn" = "NAVN")

datamatch1 <- rbind(vof20191, regnskap) # Legger på 2019 i data (situasjonsuttak - det korresponderer ikke helt det heller)

datamatch2 <- left_join(soknader_ladninger3, datamatch1, by = c("org_nr", "aar")) # Legger på topic-ladninger fra skattefunnsøknadene, setter først pga. de er primære enheter

datamatch3 <- datamatch2 %>%
  mutate(nace = str_extract(nace1_sn07, "[0-9]{2}")) %>%
  spread(tema, ladning) 

datamatch4 <- datamatch2 %>%
  mutate(nace = str_extract(nace1_sn07, "[0-9]{2}")) 


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
  mutate(Tema = ifelse(Tema == "fornybar", "Fornybar energi",
                       ifelse(Tema == "samf", "Samfunnsansvar og utvikling",
                              ifelse(Tema == "deling", "Bil, utslipp og energi",
                                     ifelse(Tema == "transport", "Energi og utslipp",
                                            ifelse(Tema == "materialer", "Materialer",
                                                   ifelse(Tema == "plast", "Marin forurensing",
                                                          ifelse(Tema == "tekstil", "Gjenbruk og bytte",
                                                                 ifelse(Tema == "vann", "Rensing av vann og luft",
                                                                        Tema)))))))))

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
legend(x = -1.5, y = 1.4,
       c("Storrelse pa boble = Hvor mange foretak som handler om tema.",
         "Tykkelse pa strek = Hvor mange foretak som handler om begge temaer."), 
       pch = 20,
       col = "#777777",
       pt.cex = 2, cex = 0.8, bty = "n", ncol = 1)



#### DESKRIPTIV STATISTIKK ####

datafaktor <- datamatch3 %>%
  mutate(deling = ifelse(deling >= mean(deling), 1, 0),
         fornybar = ifelse(fornybar >= mean(fornybar), 1, 0),
         materialer = ifelse(materialer >= mean(materialer), 1, 0),
         plast = ifelse(plast >= mean(plast), 1, 0),
         samf = ifelse(samf >= mean(samf), 1, 0),
         tekstil = ifelse(tekstil >= mean(tekstil), 1, 0),
         vann = ifelse(vann >= mean(vann), 1, 0)) %>%
  select(utfall, fornybar, materialer, plast, samf, tekstil, vann, deling,
         nace, syss, org_form, fylke, alder, org_nr) %>%
  mutate(utfall = ifelse(utfall == "Innvilget", "Innvilget",
                         ifelse(is.na(utfall), NA,
                                "Avslatt"))) %>%
  mutate_at(c("fornybar", "materialer", "plast", "samf", "tekstil",
              "vann", "deling"), as.factor) %>%
  mutate(nace = ifelse(nace == "00", NA, nace)) %>%
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


# UTFALL

utfalldata <- datafaktor %>% 
  gather(fornybar, materialer, plast, samf, tekstil, vann, deling,
         key = "Tema", value = "Ladning") %>%
  group_by(Tema, Ladning, utfall) %>%
  summarise(n = n()) %>%
  mutate(Ladning = ifelse(Ladning == 0, "Ikke baerekraft", # Handler ikke om tema
                          "Baerekraft")) %>% # Handler om tema
  ungroup() %>%
  mutate(Tema = ifelse(Ladning == "Ikke baerekraft", "Ingen", Tema))

utfalldata %>%
  na.omit() %>%
  mutate(Tema = ifelse(Tema == "fornybar", "Fornybar energi",
                       ifelse(Tema == "samf", "Samfunnsansvar og utvikling",
                              ifelse(Tema == "deling", "Bil, utslipp og energi",
                                     ifelse(Tema == "materialer", "Materialer",
                                            ifelse(Tema == "plast", "Marin forurensing",
                                                   ifelse(Tema == "tekstil", "Gjenbruk og bytte",
                                                          ifelse(Tema == "vann", "Rensing av vann og luft",
                                                                 Tema)))))))) %>%
  mutate(Tema = fct_relevel(Tema, "Fornybar energi", "Samfunnsansvar og utvikling", "Bil, utslipp og energi",
                            "Materialer", "Marin forurensing", "Gjenbruk og bytte",
                            "Rensing av vann og luft", "Ingen")) %>%
  ggplot(aes(Ladning, n, fill = Tema)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(cols = vars(utfall)) +
  labs(x = "", y = "Antall soknader",
       caption = "Utelukker 1277 missingverdier der vi ikke vet utfall av soknaden. 65 av disse handlet om baerekraft.") + 
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "gray"),
        panel.grid.major = element_line(color = "lightgray"))


# FYLKE

fylkedata <- datafaktor %>%
  gather(fornybar, materialer, plast, samf, tekstil, vann, deling,
         key = "Tema", value = "Ladning") %>%
  group_by(fylke2, Tema, Ladning) %>%
  summarise(n = n()) %>%
  filter(Ladning == "1") %>%
  ungroup() 

fylkedata %>%
  na.omit() %>%
  mutate(Tema = ifelse(Tema == "fornybar", "Fornybar energi",
                       ifelse(Tema == "samf", "Samfunnsansvar og utvikling",
                              ifelse(Tema == "deling", "Bil, utslipp og energi",
                                     ifelse(Tema == "materialer", "Materialer",
                                            ifelse(Tema == "plast", "Marin forurensing",
                                                   ifelse(Tema == "tekstil", "Gjenbruk og bytte",
                                                          ifelse(Tema == "vann", "Rensing av vann og luft",
                                                                 Tema)))))))) %>%
  mutate(Tema = fct_relevel(Tema, "Fornybar energi", "Samfunnsansvar og utvikling", "Bil, utslipp og energi",
                            "Materialer", "Marin forurensing", "Gjenbruk og bytte",
                            "Rensing av vann og luft")) %>%
  ggplot(aes(fct_reorder(fylke2, n), n, fill = Tema)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(x = "", y = "Antall soknader som handler om baerekraft",
       caption = ("Utelukker 192 missingverdier der vi ikke vet fylke.")) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "gray"),
        panel.grid.major = element_line(color = "lightgray"))

fylkesdata2 <- datafaktor %>% 
  mutate(barkraft = ifelse(c(fornybar == 1 | materialer == 1 | plast== 1| samf == 1 | tekstil == 1| vann == 1| 
                               deling == 1), "barekfraft", "ikkebar")) %>%
  select(-c(fornybar, materialer, plast, samf, tekstil, vann, deling)) %>%
  #distinct(org_nr, .keep_all = TRUE) %>%
  group_by(barkraft, fylke2) %>%
  summarise(n = n()) %>%
  ungroup()


fylkesdata2 %>%
  group_by(Ladning, fylke2) %>%
  tally(n) %>%
  spread(Ladning, n) %>%
  mutate(pros = Baerekraft/(Baerekraft+`Ikke baerekraft`)*100) %>%
  na.omit() %>%
  ggplot(aes(fct_reorder(fylke2, pros), pros)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue") + 
  labs(x = "", y = "Prosentandel soknader fra fylke som handler om baerekraft",
       caption = "Utelukker 157 missingverdier der vi ikke vet fylke. 17 av disse handlet om baerekraft.") + 
  coord_flip() +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "gray"),
        panel.grid.major = element_line(color = "lightgray"))


fylkesdata2 %>%
  na.omit() %>%
  mutate(Tema = ifelse(Tema == "fornybar", "Fornybar energi",
                       ifelse(Tema == "samf", "Samfunnsansvar og utvikling",
                              ifelse(Tema == "deling", "Bil, utslipp og energi",
                                     ifelse(Tema == "materialer", "Materialer",
                                            ifelse(Tema == "plast", "Marin forurensing",
                                                   ifelse(Tema == "tekstil", "Gjenbruk og bytte",
                                                          ifelse(Tema == "vann", "Rensing av vann og luft",
                                                                 Tema)))))))) %>%
  mutate(Tema = fct_relevel(Tema, "Fornybar energi", "Samfunnsansvar og utvikling", "Bil, utslipp og energi",
                            "Materialer", "Marin forurensing", "Gjenbruk og bytte",
                            "Rensing av vann og luft", "Ingen")) %>%
  ggplot(aes(Ladning, n, fill = Tema)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(cols = vars(fylke2)) +
  labs(x = "", y = "Antall soknader",
       caption = "Utelukker 3961 missingverdier der vi ikke vet fylke. 191 av disse handlet om baerekraft.") + 
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "gray"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(angle = 90))


# NACE

nacedata <- datafaktor %>%
  gather(fornybar, materialer, plast, samf, tekstil, vann, deling,
         key = "Tema", value = "Ladning") %>%
  group_by(nace2, Tema, Ladning) %>%
  summarise(n = n()) %>%
  filter(Ladning == "1") %>%
  ungroup() 

nacedata %>%
  na.omit() %>%
  mutate(Tema = ifelse(Tema == "fornybar", "Fornybar energi",
                       ifelse(Tema == "samf", "Samfunnsansvar og utvikling",
                              ifelse(Tema == "deling", "Bil, utslipp og energi",
                                     ifelse(Tema == "materialer", "Materialer",
                                            ifelse(Tema == "plast", "Marin forurensing",
                                                   ifelse(Tema == "tekstil", "Gjenbruk og bytte",
                                                          ifelse(Tema == "vann", "Rensing av vann og luft",
                                                                 Tema)))))))) %>%
  mutate(Tema = fct_relevel(Tema, "Fornybar energi", "Samfunnsansvar og utvikling", "Bil, utslipp og energi",
                            "Materialer", "Marin forurensing", "Gjenbruk og bytte",
                            "Rensing av vann og luft")) %>%
  ggplot(aes(fct_reorder(nace2, n), n, fill = Tema)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(x = "", y = "Antall soknader som handler om baerekraft",
       caption = "Utelukker 199 missingverdier der vi ikke vet nacekode.") +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "gray"),
        panel.grid.major = element_line(color = "lightgray"))

fagligvittjen <- datamatch2 %>%
  spread(tema, ladning) %>%
  rename("nace" = "nace1_sn07") %>%
  mutate(deling = ifelse(deling >= mean(deling), 1, 0),
         fornybar = ifelse(fornybar >= mean(fornybar), 1, 0),
         materialer = ifelse(materialer >= mean(materialer), 1, 0),
         plast = ifelse(plast >= mean(plast), 1, 0),
         samf = ifelse(samf >= mean(samf), 1, 0),
         tekstil = ifelse(tekstil >= mean(tekstil), 1, 0),
         transport = ifelse(transport >= mean(transport), 1, 0),
         vann = ifelse(vann >= mean(vann), 1, 0)) %>%
  select(utfall, fornybar, materialer, plast, samf, tekstil, transport, vann, deling,
         nace, syss, org_form, fylke, alder, org_nr) %>%
  mutate(utfall = ifelse(utfall == "Innvilget", "Innvilget",
                         ifelse(is.na(utfall), NA,
                                "Avslatt"))) %>%
  mutate_at(c("fornybar", "materialer", "plast", "samf", "tekstil", "transport",
              "vann", "deling"), as.factor) %>%
  rename("Fornybar energi" = "fornybar") %>%
  rename("Samfunnsansvar og utvikling" = "samf") %>%
  rename("Bil, utslipp og energi" = "deling") %>%
  rename("Energi og utslipp" = "transport") %>%
  rename("Materialer" = "materialer") %>%
  rename("Marin forurensing" = "plast") %>%
  rename("Gjenbruk og bytte" = "tekstil") %>%
  rename("Rensing av vann og luft" = "vann") %>%
  mutate(utfall = factor(utfall, levels = c("Innvilget", "Avslatt"))) %>%
  filter(str_detect(nace, c("69.[0-9]+", "70.[0-9]+", "71.[0-9]+", "72.[0-9]+", 
                            "73.[0-9]+", "74.[0-9]+", "75.[0-9]+")))

nacedata2 <- datafaktor %>% 
  mutate(barkraft = ifelse(c(fornybar == 1 | materialer == 1 | plast== 1| samf == 1 | tekstil == 1| vann == 1| 
                               deling == 1), "barekfraft", "ikkebar")) %>%
  select(-c(fornybar, materialer, plast, samf, tekstil, vann, deling)) %>%
  #distinct(org_nr, .keep_all = TRUE) %>%
  group_by(barkraft, nace2) %>%
  summarise(n = n()) %>%
  ungroup()


nacedata2 %>%
  na.omit() %>%
  ggplot(aes(barkraft, n, fill = nace2)) +  
  geom_bar(stat = "identity", position = "dodge") + 
  #facet_grid(cols = vars(nace2)) +
  labs(x = "", y = "Antall soknader") +
  #     caption = "Utelukker 4179 missingverdier der vi ikke vet nacekode. 198 av disse handlet om baerekraft.") + 
  #scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "gray"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(angle = 90))


# ORG_FORM

orgformdata <- datafaktor %>%
  gather(fornybar, materialer, plast, samf, tekstil, transport, vann, deling,
         key = "Tema", value = "Ladning") %>%
  group_by(nace2, Tema, Ladning) %>%
  group_by(org_form, Tema, Ladning) %>%
  summarise(n = n()) %>%
  filter(Ladning == "1") %>%
  ungroup() 

orgformdata %>%
  na.omit() %>%
  mutate(Tema = ifelse(Tema == "fornybar", "Fornybar energi",
                       ifelse(Tema == "samf", "Samfunnsansvar og utvikling",
                              ifelse(Tema == "deling", "Bil, utslipp og energi",
                                     ifelse(Tema == "transport", "Energi og utslipp",
                                            ifelse(Tema == "materialer", "Materialer",
                                                   ifelse(Tema == "plast", "Marin forurensing",
                                                          ifelse(Tema == "tekstil", "Gjenbruk og bytte",
                                                                 ifelse(Tema == "vann", "Rensing av vann og luft",
                                                                        Tema))))))))) %>%
  mutate(Tema = fct_relevel(Tema, "Fornybar energi", "Samfunnsansvar og utvikling", "Bil, utslipp og energi",
                            "Energi og utslipp", "Materialer", "Marin forurensing", "Gjenbruk og bytte",
                            "Rensing av vann og luft", "Ingen")) %>%
  ggplot(aes(fct_reorder(org_form, n), n, fill = Tema)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(x = "", y = "Antall soknader som handler om baerekraft",
       caption = "Utelukker 208 missingverdier der vi ikke vet organisasjonsform.") +
  scale_fill_brewer(palette = "Set2") +
  theme_light()

##### MODELLER ####

## REGRESJONSMODELL ##

mod1 <- glm(as.factor(utfall) ~ fornybar + materialer + plast + samf + tekstil + vann + deling +
              nace2 + 
              org_form + 
              fylke2,
            na.action = "na.exclude",
            family = binomial("logit"),
            data = datafaktor)

stargazer::stargazer(mod1, type = "html", 
                     omit = c("nace2", "fylke2", "org_form"),
                     covariate.labels = c("Fornybar energi", "Materialer", "Marin forurensing",
                                          "Samfunnsansvar og utvikling", "Gjenbruk og bytte",
                                          "Rensing av vann og luft",
                                          "Bil, utslipp og energi", "Konstant"),
                     dep.var.caption = "Utfall av soknad", 
                     dep.var.labels = "Innvilget",
                     notes = "Kontrollvariabler: Naeringskode, fylke, organisasjonsform.")

datafaktor <- datafaktor %>% 
  mutate(predicted_glm = predict(mod1, type = "response"))

datafaktor %>%
  gather(fornybar, materialer, plast, samf, tekstil, vann, deling,
         key = "Tema", value = "Ladning") %>%
  mutate(Ladning = ifelse(Ladning == 0, "Handler ikke om tema",
                          ifelse(Ladning == 1, "Handler om tema",
                                 Ladning))) %>%
  rename(" " = "Ladning") %>%
  mutate(Tema = ifelse(Tema == "fornybar", "Fornybar energi",
                       ifelse(Tema == "samf", "Samfunnsansvar og utvikling",
                              ifelse(Tema == "deling", "Bil, utslipp og energi",
                                     ifelse(Tema == "materialer", "Materialer",
                                            ifelse(Tema == "plast", "Marin forurensing",
                                                   ifelse(Tema == "tekstil", "Gjenbruk og bytte",
                                                          ifelse(Tema == "vann", "Rensing av vann og luft",
                                                                 Tema)))))))) %>%
  mutate(farge = ifelse(Tema %in% c("Rensing av vann og luft", "Materialer", "Fornybar energi",
                                    "Bil, utslipp og energi"), "olive", "gray")) %>%
  ggplot(aes(predicted_glm, Tema, color = ` `)) + 
  geom_boxplot(alpha = 0.8) +
  scale_color_manual(breaks = c("Handler ikke om tema", "Handler om tema"),
                     values = c("darkgray", "darkgreen")) +
  labs(x = "Sjanse for a fa innvilget soknad", y = "") +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "gray"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.y = element_text(face = c("bold", # Bil, utslipp og energi
                                            "bold", # Fornybar energi
                                            "plain", # Gjenbruk og bytte
                                            "plain", # Marin forurensing
                                            "bold", # Materialer
                                            "bold", # Rensing av vann og luft
                                            "plain"), # Samfunnsansvar og utvikling
                                   color = c("darkgreen", # Bil, utslipp og energi
                                             "darkgreen", # Fornybar energi
                                             "darkgray", # Gjenbruk og bytte
                                             "darkgray", # Marin forurensing
                                             "darkgreen", # Materialer
                                             "darkgreen", # Rensing av vann og luft
                                             "darkgray"))) # Samfunnsansvar og utvikling

# Sett "Rensing av vann og luft", "Materialer", "Fornybar energi" og "Bil, utslipp og energi" i fet skrift. Disse variablene er signifikante.

datafaktor %>%
  gather(fornybar, materialer, plast, samf, tekstil, vann, deling,
         key = "Tema", value = "Ladning") %>%
  mutate(Ladning = ifelse(Ladning == 0, "Handler ikke om tema",
                          ifelse(Ladning == 1, "Handler om tema",
                                 Ladning))) %>%
  rename(" " = "Ladning") %>%
  mutate(Tema = ifelse(Tema == "fornybar", "Fornybar energi",
                       ifelse(Tema == "samf", "Samfunnsansvar og utvikling",
                              ifelse(Tema == "deling", "Bil, utslipp og energi",
                                     ifelse(Tema == "transport", "Energi og utslipp",
                                            ifelse(Tema == "materialer", "Materialer",
                                                   ifelse(Tema == "plast", "Marin forurensing",
                                                          ifelse(Tema == "tekstil", "Gjenbruk og bytte",
                                                                 ifelse(Tema == "vann", "Rensing av vann og luft",
                                                                        Tema))))))))) %>%
  filter(Tema %in% c("Fornybar energi", "Materialer", "Rensing av vann og luft", "Bil, utslipp og energi")) %>%
  ggplot(aes(predicted_glm, Tema, color = ` `)) + 
  geom_boxplot(alpha = 0.8) +
  scale_color_manual(breaks = c("Handler ikke om tema", "Handler om tema"),
                     values = c("darkgray", "darkgreen")) +
  labs(x = "Sjanse for a fa innvilget soknad", y = "") +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "gray"),
        panel.grid.major = element_line(color = "lightgray"))

## RANDOM FOREST ##

set.seed(393)

## Trenger teknisk sett ikke å splitte ved random forest, for den bruker OOB estimater uansett
#split <- initial_split(datanumerisk, prop = 0.8)
#train <- training(split)
#test <- testing(split)

mod3 <- randomForest(factor(utfall) ~ fornybar + materialer + plast + samf + tekstil + transport + vann + deling +
                       nace + 
                       syss + 
                       org_form + 
                       fylke + 
                       alder,
                     data = datanumerisk,
                     na.action = na.roughfix,
                     type = "classification",
                     mtry = 5,
                     ntree = 600,
                     #sampsize = c(200,600),
                     importance = TRUE)

importance(mod3)
varImpPlot(mod3, type = 2,
           main = "Viktighet av hver variabel pa a fa innvilget soknad")

min <- min(table(datafaktor$utfall)) # Minimum frekvens, dvs. antall ganger noen fikk invilget søknaden, 1306.

mod4 <- randomForest(factor(utfall) ~ fornybar + materialer + plast + samf + 
                       tekstil + transport + vann + deling +
                       nace2 + 
                       syss + 
                       org_form + 
                       fylke2 + 
                       alder,
                     data = datafaktor,
                     na.action = na.roughfix,
                     type = "classification",
                     mtry = 8,
                     ntree = 600,
                     sampsize = c(min,min),
                     importance = TRUE)

mod4$confusion

importance(mod4)
varImpPlot(mod4, type = 2,
           main = "Viktighet av hver variabel pa a fa innvilget soknad")

datafaktor <- datafaktor %>% 
  mutate(predicted = predict(mod4))

caret::confusionMatrix(datafaktor$predicted, datafaktor$utfall, positive = "Innvilget")


partialPlot(mod4, pred.data = as.data.frame(na.omit(datafaktor)), x.var = fornybar, which.class = "Innvilget")
partialPlot(mod4, pred.data = as.data.frame(na.omit(datafaktor)), x.var = materialer, which.class = "Innvilget")
partialPlot(mod4, pred.data = as.data.frame(na.omit(datafaktor)), x.var = plast, which.class = "Innvilget")
partialPlot(mod4, pred.data = as.data.frame(na.omit(datafaktor)), x.var = samf, which.class = "Innvilget")
partialPlot(mod4, pred.data = as.data.frame(na.omit(datafaktor)), x.var = tekstil, which.class = "Innvilget")
partialPlot(mod4, pred.data = as.data.frame(na.omit(datafaktor)), x.var = transport, which.class = "Innvilget")
partialPlot(mod4, pred.data = as.data.frame(na.omit(datafaktor)), x.var = vann, which.class = "Innvilget")
partialPlot(mod4, pred.data = as.data.frame(na.omit(datafaktor)), x.var = deling, which.class = "Innvilget")

partialPlot(mod4, pred.data = as.data.frame(na.omit(datafaktor)), x.var = alder, which.class = "Innvilget")
partialPlot(mod4, pred.data = as.data.frame(na.omit(datafaktor)), x.var = syss, which.class = "Innvilget")
partialPlot(mod4, pred.data = as.data.frame(na.omit(datafaktor)), x.var = org_form, which.class = "Innvilget")

fylke_rf_plot <- partialPlot(mod4, pred.data = as.data.frame(na.omit(datafaktor)), x.var = fylke2, which.class = "Innvilget")
fylke_rf_plot %>% as_tibble() %>% rename("fylke" = "x") %>%
  ggplot(aes(fylke, y)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(x = "", y = "Sjanse (likelihood) for a fa innvilget soknad") +
  theme_light()

nace_rf_plot <- partialPlot(mod4, pred.data = as.data.frame(na.omit(datafaktor)), x.var = nace2, which.class = "Innvilget")
nace_rf_plot %>% as_tibble() %>% rename("nace" = "x") %>%
  ggplot(aes(nace, y)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(x = "", y = "Sjanse (likelihood) for a fa innvilget soknad") +
  theme_light()




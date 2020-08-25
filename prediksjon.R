
#### Pakker ####

library(readxl)
library(tidyverse)
library(tidytext)
library(stm)
library(quanteda)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(rsample)
library(pROC)
library(e1071)
library(glmnet)


##### DATA #####

# Last inn ferdig oversatte søknadstekster.

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
                                                                        tema))))))))) #%>%
  #filter(tema %in% c("fornybar", 
  #                   "samf", 
  #                   "deling", 
  #                   "transport", 
  #                   "materialer", 
  #                   "plast", 
  #                   "tekstil", 
  #                   "vann" 
  #))


# Hent inn VoF data og data fra regnskapsregisteret (må søkes om, kan også utelates).

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

datamatch2 <- left_join(soknader_ladninger, datamatch1, by = c("org_nr", "aar")) # Legger på topic-ladninger fra skattefunnsøknadene, setter først pga. de er primære enheter

datamatch3 <- datamatch2 %>%
  mutate(nace = str_extract(nace1_sn07, "[0-9]{2}")) %>%
  spread(tema, ladning) 

# Data fungerer best når topic-variabler er numeriske (sannsynlighet for å havne innenfor tema), ikke faktor(0 og 1)
datafaktor <- datamatch3 %>%
  mutate(fornybar = as.factor(ifelse(fornybar >= mean(fornybar), 1, 0))) %>%
  mutate(utfall = ifelse(utfall == "Innvilget", "Innvilget",
                         ifelse(is.na(utfall), NA,
                                "Avslatt"))) %>%
        #deling = ifelse(deling >= mean(deling), 1, 0),
         #materialer = ifelse(materialer >= mean(materialer), 1, 0),
         #plast = ifelse(plast >= mean(plast), 1, 0),
         #samf = ifelse(samf >= mean(samf), 1, 0),
         #tekstil = ifelse(tekstil >= mean(tekstil), 1, 0),
         #transport = ifelse(transport >= mean(transport), 1, 0),
         #vann = ifelse(vann >= mean(vann), 1, 0)) %>%
  #mutate(utfall = ifelse(utfall == "Innvilget", "Innvilget",
  #                       ifelse(is.na(utfall), NA,
  #                              "Avslatt"))) %>%
  #mutate_at(c("fornybar", "materialer", "plast", "samf", "tekstil", "transport",
  #            "vann", "deling"), as.factor) %>%
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


##### DFM #####

soknader_ladninger2 <- soknader_ladninger %>%
  select(id, text)

#corpus <- corpus(soknader_ladninger2)

#dfm <- dfm(corpus, 
#           remove_numbers = TRUE,
#           remove_punct = TRUE, 
#           remove_symbols = TRUE,
#           remove_url = TRUE, 
#           verbose = TRUE,
#           remove = stopwords("no", "en", "swedish"))

dfm_words <- soknader_ladninger2 %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE) %>%
  bind_tf_idf(word, id, n) %>%
  mutate(word = fct_reorder(word, tf_idf))

dfm_words <- dfm_words %>%
  select(id, word, tf_idf) %>%
  spread(word, tf_idf)

dfm_words <- dfm_words %>%
  mutate(id = as.character(id)) %>%
  mutate(utfall = as.factor(utfall)) %>%
  mutate(aar = as.character(aar)) %>%
  select(-c("vekstpotensiale", "kjennetegn", "prosjektnavn", "text", 
            "utfall", "aar", "navn", "fylke", "kommune", "opprettet", "alder", "deling", "fornybar", "materialer", "plast", "samf", "tekstil", "transport", "vann"))

topic_ladning <- as_tibble(soknader_stm$theta) %>%
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
  mutate(org_nr = as.character(org_nr)) %>%
  left_join(soknad_tibble, by = "id")

word_data <- left_join(dfm_words, topic_ladning)

# OBS: Tar veldig lang tid å kjøre:
word_data2 <- word_data %>%
  mutate_at(c(116:length(.)), function(x) ifelse(is.na(x), 0, x))


##### MODELLER #####

# Modellen ser ut til å prediekre best med numeriske verdier.

# 1. Sjekk om vi kan koble på flere variabler
# 2. Se på andre modeller

set.seed(23319)

## Disse har for mange missingverdier til a kunne brukes:
#syss
#alder
#oms
#innovasjonstype 
#prosjektorientering 
#vekstpotensiale 
#kjennetegn 
#virkemiddelbetegnelse 
#omsoktbelop 
#innvilgetbelop

test <- word_data2 %>% 
  select(116:length(.)) %>%
  select(1:100, "id", "org_nr", "aar") %>% 
  na.omit()

split <- initial_split(test, prop = 0.8)
train <- training(split)
test <- testing(split)

min <- min(table(train$fornybar)) # Minimum frekvens.


#### Logistisk modell ####

glm <- glm(as.factor(fornybar) ~ ., 
            na.action = "na.exclude",
            family = binomial("logit"),
            data = train) # Warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred 

# Test mot treningsdata:
pred_glm <- predict(glm, type = "response")

caret::confusionMatrix(data = factor(as.numeric(pred_glm > 0.01)), # 80 % sjanse for a ikke handle om fornybar = ikke fornybar
                       reference = factor(train$fornybar), positive = "1")

# Test mot testdata:
pred_glm <- predict(glm, newdata = test, type = "response")

caret::confusionMatrix(data = factor(as.numeric(pred_glm > 0.1)),
                       reference = factor(test$fornybar), positive = "1")


#### Random forest ####

rf <- randomForest(factor(fornybar) ~ .,
                     data = train,
                     na.action = na.roughfix,
                     type = "classification",
                     mtry = 8,
                     ntree = 200,
                     sampsize = c(170,30),
                     importance = TRUE)

#plot(mod2) # Ca. 200 traer
#importance(mod)
varImpPlot(rf, main = "Viktighet av hver variabel")

# Test mot treningsdata:
pred_rf_train <- predict(rf)
caret::confusionMatrix(pred_rf_train, train$fornybar, positive = "1")

# Test mot testdata:
pred_rf <- predict(rf, newdata = test)
caret::confusionMatrix(pred_rf, test$fornybar, positive = "1")


#### Support vektor ####

svm <- svm(fornybar ~ .,
            data = train,
            kernel = "linear", # radial
            gamma = 1,
            cost = 10,
            decision.values = TRUE)

summary(svm)

plot(svm, V4 ~ V39, data = train)

# Test mot treningsdata:
pred_svm_train <- fitted(svm)
caret::confusionMatrix(pred_svm_train, train$fornybar, positive = "1")

# Test mot testdata:
pred_svm <- predict(svm, newdata = test)
caret::confusionMatrix(pred_svm, test$fornybar, positive = "1")


#### Ridge ####

ridge <- cv.glmnet(data.matrix(train[-5]), # Tar ut kolonne 5, dvs. kolonne med "fornybar"
                   as.factor(train$fornybar), # Predikerer om soknad handler om fornybar
                   family = "binomial", # Bruker binomial model.
                   alpha = 0, # 0 = Ridge modell (you use squared parameter sum)
                   nfolds = 5, # nfold (split i fem)
                   intercept = TRUE, # Intercept hvis du vil
                   type.measure = "class")

plot(ridge)

# Test mot treningsdata:
pred_ridge_train <- predict(ridge$glmnet.fit, 
                            s = ridge$lambda.min,
                            type = "response",
                            newx = data.matrix(train[-5]))

caret::confusionMatrix(data = factor(as.numeric(pred_ridge_train > 0.3)), # 30 % sjanse for a handle om fornybar = fornybar
                       reference = factor(train$fornybar))

# Test mot testdata:
pred_ridge <- predict(ridge$glmnet.fit, 
                      s = ridge$lambda.min, 
                      type = "response", 
                      newx = data.matrix(test[-5]))

caret::confusionMatrix(data = factor(as.numeric(pred_ridge > 0.3)), # 30 % sjanse for a handle om fornybar = fornybar
                       reference = factor(test$fornybar))


#### Lasso ####

lasso <- cv.glmnet(data.matrix(train[-5]), 
                   as.factor(train$fornybar),
                   family = "binomial", 
                   alpha = 1, # 1 = Lasso modell (you take the sum of the absolute betas)
                   nfolds = 5, 
                   intercept = FALSE, 
                   type.measure = "class")

# coef(lasso$glmnet.fit)

# Test mot treningsdata:
pred_lasso_train <- predict(lasso$glmnet.fit, 
                            s = lasso$lambda.min,
                            type = "response",
                            newx = data.matrix(train[-5]))

caret::confusionMatrix(data = factor(as.numeric(pred_lasso_train > 0.3)), # 30 % sjanse for a handle om fornybar = fornybar
                       reference = factor(train$fornybar))

# Test mot testdata:
pred_lasso <- predict(lasso$glmnet.fit, 
                      s = lasso$lambda.min, 
                      type = "response", 
                      newx = data.matrix(test[-5]))

caret::confusionMatrix(data = factor(as.numeric(pred_lasso > 0.3)), # 30 % sjanse for a handle om fornybar = fornybar
                       reference = factor(test$fornybar))


## ROC kurver ##
glm_test_roc <- roc(response = test$fornybar,
                predictor = predict(glm, test))
# Area under the curve: 0.9744

randomforest_test_roc <- roc(response = test$fornybar,
                         predictor = predict(rf, test, type = "prob")[,2])
# Area under the curve: 0.9678

svm_test_roc <- roc(response = test$fornybar,
                    predictor = as.vector(attributes(predict(svm, test, decision.values = TRUE))$decision.values))
# Area under the curve: 0.9743

ridge_test_roc <- roc(response = test$fornybar,
                      predictor = predict(ridge$glmnet.fit, 
                                          s = ridge$lambda.min, 
                                          type = "response", newx = data.matrix(test[-5])))
# Area under the curve: 0.8249

lasso_test_roc <- roc(response = test$fornybar,
                  predictor = predict(lasso$glmnet.fit, 
                                      s = lasso$lambda.min,
                                      type = "response", newx = data.matrix(test[-5])))
# Area under the curve: 0.9508

ggroc(list("logit" = glm_test_roc,
           "random forest" = randomforest_test_roc,
           "svm" = svm_test_roc,
           "ridge" = ridge_test_roc,
           "lasso" = lasso_test_roc), size = 1, alpha = 0.8) +
  scale_colour_discrete(name = "Modell",
                        breaks = c("logit", "random forest", "svm", "ridge", "lasso"),
                        labels = c("Logistisk", "Random forest", "Support vector machine", "Ridge", "Lasso")) +
  ggtitle("ROC-kurver for modeller") +
  theme_bw()



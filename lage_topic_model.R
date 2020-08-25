

#### Pakker ####
library(haven)
library(readxl)
library(tidyverse)
library(quanteda)
library(tidytext)
library(stm)


#### DATA ####

# Last inn excel-fil med søknadstekster. De bør være oversatt slik at alle er f. eks. norsk bokmål.

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


#### TOPIC MODEL #####

soknader_dfm <- dfm(corpus_soknader,
                    remove_numbers = TRUE, # Tar ut tall.
                    remove_punct = TRUE, # Tar ut punktsetting.
                    remove_symbols = TRUE, # Tar ut symboler.
                    remove = c(stopwords("norwegian"), 
                               stopwords("swedish"), 
                               stopwords("english"))) 

soknader_dfm_trim <- dfm_trim(soknader_dfm, # Ord bær være tilstede i minst 0.1 %, men ikke mer enn 99.9 %, av tekstene.
                              min_docfreq = 0.001, # Sjeldne ord (dukker bare opp i 0.1 % av tekstene).
                              max_docfreq = 0.999, # Hyppige ord (dukker opp i 99.9 % av tekstene).
                              docfreq_type = "prop") 

many_models <- data_frame(K = c(50, 60, 70, 80, 100, 110, 120, 130, 140)) %>% # Sjekker antall topics.
  mutate(topic_model = map(K, ~stm(soknader_dfm_trim, K = ., # Kalkulerer topic modell for alle mengdene topics. Vi helst bruke future_map og paralellprosessere.
                                   verbose = TRUE)))

# God ide å lagre many_models, siden det tar en stund å kjøre den.

# Sjekker hva som er optimalt antall topics:
heldout <- make.heldout(soknader_dfm_trim)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, soknader_dfm_trim),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, soknader_dfm_trim),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Modelldiagnostikk på antall emner",
       subtitle = "100 emner virker optimalt")

k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  unnest(cols = c(exclusivity, semantic_coherence)) %>%
  mutate(K = as.factor(K)) %>%
  filter(K == "100") %>%
  ggplot(aes(semantic_coherence, exclusivity)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(color = "darkgreen") +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Sammenlikner exclusivity og semantic coherence",
       subtitle = "Emner med høy eksklusivitet har lavere semantisk smahøringhet") +
  theme_light()

# Optimalt antall ser her ut til å være omtrent 100.

#topic_model <- k_result %>% 
#  filter(K == 100) %>% 
#  pull(topic_model) %>% 
#  .[[1]]

soknader_g <- convert(soknader_dfm_trim, to = "stm")

soknader_stm <- stm(documents = soknader_g$documents,
                    vocab = soknader_g$vocab,
                    data = soknader_g$meta,
                    seed = 4320, # Seed gir reproduserbarhet
                    K = 100, # Setter antall temaer til 100
                    max.em.its = 10000, 
                    verbose = TRUE,
                    init.type = "LDA") 



#### TOLKNING ####

topicmodeloutput <- labelTopics(soknader_stm, n = 100)

prob <- unlist(topicmodeloutput$prob) %>%
  as_tibble() %>%
  unite("words", V1:V100, sep = " ") %>%
  select(words)
  
frex <- unlist(topicmodeloutput$frex) %>%
  as_tibble() %>%
  unite("words", V1:V100, sep = " ") %>%
  select(words)

topicmodeloutput <- as_tibble(list(Topic = topicmodeloutput$topicnums,
                                   Probability = prob$words ,
                                   FREX = frex$words))

xlsx::write.xlsx(topicmodeloutput, file = "/ssb/bruker/sgb/Baerekraft med Innovasjon Norge/R-scripts/data/topicmodeloutput.xlsx")

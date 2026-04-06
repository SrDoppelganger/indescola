# Pacotes ---------------------------------------------------------------
library(tibble)
library(tidyr)
library(dplyr)
library(mirt)


# Ajuste global e ajuste por item --------------------------------------


m2_fit <- tryCatch(
  mirt::M2(mod_3pl),
  error = function(e) NULL
)


item_fit <- tryCatch(
  mirt::itemfit(mod_3pl),
  error = function(e) NULL
)

# Ajuste da Base --------------------------------------------------------
item_data <- read.csv2("G:/Documents/projetos/Indescola_REVAMP/microdados_censo_escolar_2023/dados/microdados_ed_basica_2023.csv",encoding = "latin1")

clean_data <- item_data %>%
  filter(TP_DEPENDENCIA %in% c(1,2,3), TP_SITUACAO_FUNCIONAMENTO == 1)

# flags condicionais
clean_data <- clean_data |>
  mutate(
    #talvez precise agregar para creches e para anos iniciais/finais
    tem_ed_infantil = if_else(IN_INF == 1, 1, 0, missing = 0),
    tem_fund_ou_medio = if_else(IN_FUND == 1 | IN_MED == 1, 1, 0, missing = 0)
  )

# declaração e agragação de itens para TRI
item_data_processed <- clean_data |>
  mutate(
    item_agua_potavel = if_else(IN_AGUA_POTAVEL == 1, 1, 0),
    item_energia = if_else(IN_ENERGIA_REDE_PUBLICA == 1 | IN_ENERGIA_GERADOR_FOSSIL == 1 | IN_ENERGIA_RENOVAVEL == 1, 1,0),
    item_esgoto = if_else(IN_ESGOTO_REDE_PUBLICA == 1 | IN_ESGOTO_FOSSA_SEPTICA == 1, 1, 0),
    item_cozinha = if_else(IN_COZINHA == 1, 1, 0),
    item_biblioteca = if_else(IN_BIBLIOTECA == 1 | IN_BIBLIOTECA_SALA_LEITURA == 1, 1, 0),
    item_internet = if_else(IN_INTERNET == 1, 1, 0),
    item_banda_larga = if_else(IN_BANDA_LARGA == 1, 1, 0),
    
    item_sanitario = if_else(IN_BANHEIRO == 1, 1, 0),
    item_quadra_esportes = if_else(IN_QUADRA_ESPORTES_COBERTA == 1 | IN_QUADRA_ESPORTES_DESCOBERTA == 1, 1, 0),
    
    #itens condicionais
    item_parque_infantil = case_when(
      tem_ed_infantil == 0 ~ NA_real_,
      IN_PARQUE_INFANTIL == 1 ~ 1,
      TRUE ~ 0
    ),
    
    item_banheiro_infantil = case_when(
      tem_ed_infantil == 0 ~ NA_real_,
      IN_BANHEIRO_EI == 1 ~ 1,
      TRUE ~ 0
    ),
    
    item_lab_ciencias = case_when(
      tem_fund_ou_medio == 0 ~ NA_real_,
      IN_LABORATORIO_CIENCIAS == 1 ~ 1,
      TRUE ~ 0
    ),
    
    item_lab_informatica = case_when(
      tem_fund_ou_medio == 0 ~ NA_real_,
      IN_LABORATORIO_INFORMATICA == 1 ~ 1,
      TRUE ~ 0
    )
  )

#Matriz de dados TRI
tri_data <- item_data_processed |>
  select(
    item_agua_potavel,
    item_energia,
    item_esgoto,
    item_cozinha,
    item_biblioteca,
    #esse item só tinha uma categoria de resposta, logo, foi removido
    -item_internet,
    item_banda_larga,
    item_sanitario,
    item_quadra_esportes,
    item_parque_infantil,
    item_lab_ciencias,
    item_lab_informatica
  )

mirt_data <- na.omit(tri_data)

#talvez precise mudar paste0 para F1 = 1 - 100
model_spec <- mirt.model(paste0("F1 = 1-", ncol(mirt_data)))

mod_3pl <- mirt(data = mirt_data, model=model_spec, itemtype = "3PL")

item_parameters <- coef(mod_3pl, IRTpars = TRUE, simplify = TRUE)
theta_tbl <- fscores(mod_3pl, full.scores = TRUE, full.scores.SE = TRUE)

# Base final ------------------------------------------------------------
colunas_modelo <- names(tri_data)


item_data_final <- item_data_processed |>
  drop_na(all_of(colunas_modelo))

escolas_scores <- item_data_final |>
  select(
    CO_ENTIDADE,
    NO_ENTIDADE,
    SG_UF,
    NO_MUNICIPIO,
    TP_DEPENDENCIA
  ) |>
  bind_cols(theta_tbl)

escolas_scores <- escolas_scores |>
  mutate(
    escore_padronizado = ((F1 - mean(F1)) / sd(F1) * 10 + 50)
  )

#classificação em níveis
escolas_scores <- escolas_scores |>
  mutate(
    nivel_infra = case_when(
      escore_padronizado < 40 ~ "Elementar",
      escore_padronizado >= 40 & escore_padronizado < 50 ~ "Básica",
      escore_padronizado >= 50 & escore_padronizado < 60 ~ "Adequada",
      escore_padronizado >= 60 ~ "Avançada",
      TRUE ~ "Sem Escala"
    )
  )


# Ajuste de Escopo -------------------------------------------------------
pernambuco_data <- escolas_scores %>%
  filter(SG_UF == "PE")

municipais_data <- pernambuco_data %>%
  filter(TP_DEPENDENCIA == 3)

estaduais_data <- pernambuco_data %>%
  filter(TP_DEPENDENCIA == 2)


# Exportação ------------------------------------------------------------
sample_data <- head(item_data)
write.csv2(sample_data, file="sample_data.csv", row.names = FALSE)
write.csv2(estaduais_data, file="indices_estaduais_preAjuste.csv", row.names = FALSE)

readr::write_csv(item_data, file.path(output_dir, "03_base_itens_2023.csv"))
readr::write_csv(item_parameters, file.path(output_dir, "04_parametros_itens_3pl_2023.csv"))
readr::write_csv(school_scores, file.path(output_dir, "05_escores_escolas_2023.csv"))
readr::write_csv(summary_by_level, file.path(output_dir, "06_resumo_niveis_2023.csv"))
readr::write_csv(summary_by_uf, file.path(output_dir, "07_resumo_por_uf_2023.csv"))


if (!is.null(item_fit)) {
  item_fit |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "item") |>
    readr::write_csv(file.path(output_dir, "08_item_fit_2023.csv"))
}


if (!is.null(m2_fit)) {
  readr::write_csv(as.data.frame(m2_fit), file.path(output_dir, "09_m2_fit_2023.csv"))
}


saveRDS(mod_3pl, file.path(output_dir, "10_modelo_mirt_3pl_2023.rds"))


message("Processo concluído. Arquivos salvos em: ", normalizePath(output_dir))

# Pacotes ---------------------------------------------------------------
library(tibble)
library(dplyr)


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

pernambuco_data <- item_data %>%
  filter(NO_UF == "Pernambuco",is.na(TP_CATEGORIA_ESCOLA_PRIVADA), TP_SITUACAO_FUNCIONAMENTO == 1,)

municipais_data <- pernambuco_data %>%
  filter(TP_DEPENDENCIA == 3)

estaduais_data <- pernambuco_data %>%
  filter(TP_DEPENDENCIA == 2)

view(municipais_data)
view(estaduais_data)

# Base final ------------------------------------------------------------

school_scores <- item_data |>
  dplyr::select(
    school_id,
    NO_ENTIDADE,
    NO_UF,
    NO_MUNICIPIO,
    dependency,
    location
  ) |>
  dplyr::bind_cols(theta_tbl)


summary_by_level <- school_scores |>
  dplyr::count(nivel_infraestrutura, name = "n") |>
  dplyr::mutate(pct = n / sum(n) * 100)


summary_by_uf <- school_scores |>
  dplyr::filter(!is.na(uf)) |>
  dplyr::group_by(uf, nivel_infraestrutura) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop_last") |>
  dplyr::mutate(pct = n / sum(n) * 100) |>
  dplyr::ungroup()


# Exportação ------------------------------------------------------------


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
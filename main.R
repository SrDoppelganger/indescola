# ==============================================================================
# Cálculo do Índice de Infraestrutura Escolar (Indescola 2023) - CORRIGIDO
# ==============================================================================

library(tibble)
library(tidyr)
library(dplyr)
library(mirt)
library(readr)

# --- 1. Configurações ---
input_file <- "recursos/microdados_censo_escolar_2023/dados/microdados_ed_basica_2023.csv"
output_dir <- "resultados"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- 2. Leitura e Recodificação Robustecida ---
raw_data <- read.csv2(input_file, encoding = "latin1")

item_data_processed <- raw_data %>%
  filter(TP_DEPENDENCIA %in% c(1, 2, 3), TP_SITUACAO_FUNCIONAMENTO == 1) %>%
  mutate(
   TEMAGUARECODE = if_else(IN_AGUA_POTAVEL == 1 | IN_AGUA_REDE_PUBLICA ==1 | IN_AGUA_POCO_ARTESIANO == 1, 1, 0, missing=0),
   IN_ENERGIA_REDE_PUBLICA = if_else(IN_ENERGIA_REDE_PUBLICA == 1, 1, 0, missing=0),
   TEMESGOTORECODE = if_else(IN_ESGOTO_REDE_PUBLICA == 1| IN_ESGOTO_FOSSA_SEPTICA == 1, 1, 0, missing=0),
   IN_LIXO_SERVICO_COLETA = if_else(IN_LIXO_SERVICO_COLETA == 1, 1, 0, missing=0),
   BIBLIOOUSALADELEITURA = if_else(IN_BIBLIOTECA == 1| IN_BIBLIOTECA_SALA_LEITURA == 1, 1, 0, missing=0),
   IN_PROF_BIBLIOTECARIO = if_else(IN_PROF_BIBLIOTECARIO == 1, 1, 0, missing=0),
   IN_QUADRA_ESPORTES = if_else(IN_QUADRA_ESPORTES == 1| IN_QUADRA_ESPORTES_COBERTA == 1 |IN_QUADRA_ESPORTES_DESCOBERTA == 1, 1, 0, missing=0),
   IN_REFEITORIO = if_else(IN_REFEITORIO == 1, 1, 0, missing=0),
   IN_SALA_PROFESSOR = if_else(IN_SALA_PROFESSOR == 1, 1, 0, missing=0),
   IN_SALA_ATENDIMENTO_ESPECIAL = if_else(IN_SALA_ATENDIMENTO_ESPECIAL == 1, 1, 0, missing=0),
   
   TEMACESSIBILIDADERECODE = if_else(
    IN_BANHEIRO_PNE == 1|
    IN_ACESSIBILIDADE_RAMPAS == 1|
    IN_ACESSIBILIDADE_ELEVADOR == 1|
    IN_ACESSIBILIDADE_CORRIMAO == 1|
    IN_ACESSIBILIDADE_PISOS_TATEIS == 1|
    IN_ACESSIBILIDADE_VAO_LIVRE == 1,
    1, 0, missing = 0
   ),
   
   prop_clima = if_else(QT_SALAS_UTILIZADAS > 0, QT_SALAS_UTILIZA_CLIMATIZADAS / QT_SALAS_UTILIZADAS, 0, missing=0),
   i_climgt30lt70 = if_else(prop_clima > 0.30 & prop_clima <= 0.70, 1, 0, missing=0),
   i_climgt70 = if_else(prop_clima > 0.70, 1, 0, missing=0),
   
   TEMEQUIPAMENTOSRECODE = if_else(
    IN_EQUIP_SOM == 1|
    IN_EQUIP_TV == 1|
    IN_EQUIP_MULTIMIDIA == 1,
    1, 0, missing = 0
   ),
   
   i_tvgt03 = if_else(QT_EQUIP_TV >3, 1, 0, missing = 0),
   
   COMPUTADORESPARAALUNOSRECODE = if_else(
     IN_DESKTOP_ALUNO == 1 | IN_COMP_PORTATIL_ALUNO == 1 | IN_TABLET_ALUNO == 1,
     1, 0, missing = 0
   ),
   
   IN_INTERNET_APRENDIZAGEM = if_else(IN_INTERNET_APRENDIZAGEM == 1, 1, 0, missing=0)
  )

# --- 3. Função de Cálculo TRI por Subgrupo (Versão Otimizada para Memória) ---
calcular_indice_etapa <- function(dados, itens) {
  dados_validos <- dados %>%
    filter(rowSums(!is.na(across(all_of(itens)))) > 0)
  
  if(nrow(dados_validos) < 50) return(NULL)
  
  mirt_data <- dados_validos %>% select(all_of(itens))
  
  model_spec <- mirt.model(paste0("F1 = 1-", ncol(mirt_data)))
  
  # Executa o modelo
  mod_3pl <- mirt(data = mirt_data, model = model_spec, itemtype = "3PL", verbose = FALSE)
  
  # Extrai os escores
  scores_tbl <- as_tibble(fscores(mod_3pl, full.scores = TRUE)) %>%
    mutate(escore_padronizado = ((F1 - mean(F1, na.rm=T)) / sd(F1, na.rm=T)) * 10 + 50)
  
  res <- bind_cols(dados_validos %>% select(CO_ENTIDADE), 
                   scores_tbl %>% select(escore_padronizado))
  
  # --- TRUQUE DE MESTRE: Limpeza de Memória Interna ---
  # Removemos os objetos pesados de dentro da função imediatamente
  rm(mod_3pl, mirt_data, model_spec, scores_tbl)
  
  # Forçamos o R a esvaziar a pilha de proteção do C++
  gc() 
  
  return(res)
}

# --- 4. Processamento das 5 Etapas ---
itens_definitivos <- c("TEMAGUARECODE", "IN_ENERGIA_REDE_PUBLICA", "TEMESGOTORECODE", "IN_LIXO_SERVICO_COLETA", "BIBLIOOUSALADELEITURA", "IN_PROF_BIBLIOTECARIO", "IN_QUADRA_ESPORTES", "IN_REFEITORIO",
                "IN_SALA_PROFESSOR", "IN_SALA_ATENDIMENTO_ESPECIAL", "TEMACESSIBILIDADERECODE", "i_climgt30lt70", "i_climgt70", "TEMEQUIPAMENTOSRECODE", "i_tvgt03", "COMPUTADORESPARAALUNOSRECODE", "IN_INTERNET_APRENDIZAGEM"
                )

ind_creche <- calcular_indice_etapa(item_data_processed %>% filter(IN_INF_CRE == 1), itens_definitivos) %>% rename(ind_creche = escore_padronizado)
ind_pre    <- calcular_indice_etapa(item_data_processed %>% filter(IN_INF_PRE == 1), itens_definitivos) %>% rename(ind_pre = escore_padronizado)
ind_efai   <- calcular_indice_etapa(item_data_processed %>% filter(IN_FUND_AI == 1), itens_definitivos) %>% rename(ind_efai = escore_padronizado)
ind_efaf   <- calcular_indice_etapa(item_data_processed %>% filter(IN_FUND_AF == 1), itens_definitivos) %>% rename(ind_efaf = escore_padronizado)
ind_em     <- calcular_indice_etapa(item_data_processed %>% filter(IN_MED == 1),     itens_definitivos) %>% rename(ind_em = escore_padronizado)

# --- 5. Agregação Final ---
escolas_final <- item_data_processed %>%
  select(CO_ENTIDADE, NO_ENTIDADE, SG_UF, NO_MUNICIPIO, TP_DEPENDENCIA, QT_MAT_INF_CRE, QT_MAT_INF_PRE, QT_MAT_FUND_AI, QT_MAT_FUND_AF, QT_MAT_MED) %>%
  left_join(ind_creche, by = "CO_ENTIDADE") %>%
  left_join(ind_pre, by = "CO_ENTIDADE") %>%
  left_join(ind_efai, by = "CO_ENTIDADE") %>%
  left_join(ind_efaf, by = "CO_ENTIDADE") %>%
  left_join(ind_em, by = "CO_ENTIDADE") %>%
  rowwise() %>%
  mutate(
    numerador = sum(c(ind_creche * QT_MAT_INF_CRE, ind_pre * QT_MAT_INF_PRE, 
                      ind_efai * QT_MAT_FUND_AI, ind_efaf * QT_MAT_FUND_AF, 
                      ind_em * QT_MAT_MED), na.rm = TRUE),
    denominador = sum(c(
      if_else(!is.na(ind_creche), QT_MAT_INF_CRE, 0),
      if_else(!is.na(ind_pre), QT_MAT_INF_PRE, 0),
      if_else(!is.na(ind_efai), QT_MAT_FUND_AI, 0),
      if_else(!is.na(ind_efaf), QT_MAT_FUND_AF, 0),
      if_else(!is.na(ind_em), QT_MAT_MED, 0)
    ), na.rm = TRUE),
    Ind_entidade_final = if_else(denominador > 0, numerador / denominador, NA_real_)
  )%>%
  ungroup() %>%
  mutate(nivel_infra = case_when(Ind_entidade_final < 40 ~ "Elementar", Ind_entidade_final < 50 ~ "Básica", Ind_entidade_final < 60 ~ "Adequada", Ind_entidade_final >= 60 ~ "Avançada", TRUE ~ "Sem Escala"))

write_csv2(escolas_final, file.path(output_dir, "Indescola23_replica.csv"))

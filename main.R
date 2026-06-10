# ==============================================================================
# Cálculo do Índice de Infraestrutura Escolar (Indescola 2023) - OFICIAL
# Metodologia: TRI 3PL por 5 Etapas de Ensino com Ponderação por Matrícula
# ==============================================================================


#SCRIPT ORIGINAL QUE DA 0.7617 DE CORRELAÇÃO
# Alocação agressiva de memória para evitar o erro de Protection Stack
Sys.setenv(R_PPSIZE = "500000")

library(tibble)
library(tidyr)
library(dplyr)
library(mirt)
library(readr)

# --- 1. Configurações de Caminhos ---
input_file <- "recursos/microdados_censo_escolar_2023/dados/microdados_ed_basica_2023.csv"
output_dir <- "resultados"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- 2. Leitura e Recodificação Consolidada (Tratamento Base + 2023) ---
message("Lendo e recodificando microdados do Censo Escolar...")
raw_data <- read.csv2(input_file, encoding = "latin1")

item_data_processed <- raw_data %>%
  filter(TP_DEPENDENCIA %in% c(1, 2, 3), TP_SITUACAO_FUNCIONAMENTO == 1) %>%
  mutate(
    # 1. Saneamento e Estrutura Básica
    TEMÁGUARECODE = if_else(IN_AGUA_POTAVEL == 1 | IN_AGUA_REDE_PUBLICA == 1 | IN_AGUA_POCO_ARTESIANO == 1, 1, 0, missing = 0),
    IN_ENERGIA_REDE_PUBLICA = if_else(IN_ENERGIA_REDE_PUBLICA == 1, 1, 0, missing = 0),
    TEMESGOTORECODE = if_else(IN_ESGOTO_REDE_PUBLICA == 1 | IN_ESGOTO_FOSSA_SEPTICA == 1, 1, 0, missing = 0),
    IN_LIXO_SERVICO_COLETA = if_else(IN_LIXO_SERVICO_COLETA == 1, 1, 0, missing = 0),
    
    # 2. Espaços Pedagógicos e Sociais
    BIBLIOOUSALADELEITURA = if_else(IN_BIBLIOTECA == 1 | IN_BIBLIOTECA_SALA_LEITURA == 1, 1, 0, missing = 0),
    IN_PROF_BIBLIOTECARIO = if_else(IN_PROF_BIBLIOTECARIO == 1, 1, 0, missing = 0),
    IN_QUADRA_ESPORTES = if_else(IN_QUADRA_ESPORTES == 1 | IN_QUADRA_ESPORTES_COBERTA == 1 | IN_QUADRA_ESPORTES_DESCOBERTA == 1, 1, 0, missing = 0),
    IN_REFEITORIO = if_else(IN_REFEITORIO == 1, 1, 0, missing = 0),
    IN_SALA_PROFESSOR = if_else(IN_SALA_PROFESSOR == 1, 1, 0, missing = 0),
    
    # 3. Inclusão (Mapeamento Refinado com Validação do Dicionário TP_AEE)
    IN_SALA_ATENDIMENTO_ESPECIAL = if_else(
      IN_SALA_ATENDIMENTO_ESPECIAL == 1 | coalesce(TP_AEE, 0) %in% c(1, 2), 
      1, 0, missing = 0
    ),
    TEMACESSIBILIDADERECODE = if_else(
      IN_BANHEIRO_PNE == 1 | IN_ACESSIBILIDADE_RAMPAS == 1 |
        IN_ACESSIBILIDADE_ELEVADOR == 1 | IN_ACESSIBILIDADE_CORRIMAO == 1 |
        IN_ACESSIBILIDADE_PISOS_TATEIS == 1 | IN_ACESSIBILIDADE_VAO_LIVRE == 1,
      1, 0, missing = 0
    ),
    
    # 4. Conforto Térmico
    prop_clima = if_else(QT_SALAS_UTILIZADAS > 0, QT_SALAS_UTILIZA_CLIMATIZADAS / QT_SALAS_UTILIZADAS, 0, missing = 0),
    i_climgt30lt70 = if_else(prop_clima > 0.30 & prop_clima <= 0.70, 1, 0, missing = 0),
    i_climgt70 = if_else(prop_clima > 0.70, 1, 0, missing = 0),
    
    # 5. Recursos Audiovisuais e Digitais
    TEMEQUIPAMENTOSRECODE = if_else(IN_EQUIP_SOM == 1 | IN_EQUIP_TV == 1 | IN_EQUIP_MULTIMIDIA == 1, 1, 0, missing = 0),
    i_tvgt03 = if_else(QT_EQUIP_TV > 3, 1, 0, missing = 0),
    IN_INTERNET_APRENDIZAGEM = if_else(IN_INTERNET_APRENDIZAGEM == 1, 1, 0, missing = 0),
    
    # 6. Tecnologia da Informação (Ajustado para Desmembramento de Desktops/Laptops de 2023)
    COMPUTADORESPARAALUNOSRECODE = if_else(
      coalesce(QT_DESKTOP_ALUNO, 0) > 0 | coalesce(QT_COMP_PORTATIL_ALUNO, 0) > 0 | coalesce(QT_TABLET_ALUNO, 0) > 0 |
        coalesce(IN_DESKTOP_ALUNO, 0) == 1 | coalesce(IN_COMP_PORTATIL_ALUNO, 0) == 1 | coalesce(IN_TABLET_ALUNO, 0) == 1, 
      1, 0, missing = 0
    ),
    
    # 7. Variáveis Estruturais Incondicionais (0 ou 1 absoluto para evitar distorção de NA)
    item_parque          = if_else(IN_PARQUE_INFANTIL == 1, 1, 0, missing = 0),
    item_sanitario_ei    = if_else(IN_BANHEIRO_EI == 1, 1, 0, missing = 0),
    item_lab_ciencias    = if_else(IN_LABORATORIO_CIENCIAS == 1, 1, 0, missing = 0),
    item_lab_informatica = if_else(IN_LABORATORIO_INFORMATICA == 1, 1, 0, missing = 0)
  )

# --- 3. Função de Cálculo TRI por Etapa (Criando as Réguas Isoladas 50/10) ---
calcular_indice_etapa <- function(dados, itens) {
  dados_validos <- dados %>%
    filter(rowSums(!is.na(across(all_of(itens)))) > 0)
  
  if(nrow(dados_validos) < 50) return(NULL)
  
  mirt_data <- dados_validos %>% select(all_of(itens))
  
  # Calibração do modelo unidimensional 3PL
  mod_3pl <- mirt(data = mirt_data, model = 1, itemtype = "3PL", verbose = FALSE)
  
  # Extração vetorial limpa para evitar conflitos de matriz com o dplyr
  escores_matriz <- fscores(mod_3pl, full.scores = TRUE)
  vetor_puro_f1  <- as.numeric(escores_matriz[, 1])
  
  # PADRONIZAÇÃO NA ETAPA: Cada etapa gera sua própria régua normativa isolada
  escore_padronizado <- ((vetor_puro_f1 - mean(vetor_puro_f1, na.rm = TRUE)) / sd(vetor_puro_f1, na.rm = TRUE)) * 10 + 50
  
  res <- tibble(
    CO_ENTIDADE = dados_validos$CO_ENTIDADE,
    escore_etapa = escore_padronizado
  )
  
  rm(mod_3pl, mirt_data, escores_matriz, vetor_puro_f1)
  gc()
  
  return(res)
}

# --- 4. Processamento das Réguas por Etapa de Ensino ---
itens_base <- c(
  "TEMÁGUARECODE", "IN_ENERGIA_REDE_PUBLICA", "TEMESGOTORECODE", "IN_LIXO_SERVICO_COLETA", 
  "BIBLIOOUSALADELEITURA", "IN_PROF_BIBLIOTECARIO", "IN_QUADRA_ESPORTES", "IN_REFEITORIO", 
  "IN_SALA_PROFESSOR", "IN_SALA_ATENDIMENTO_ESPECIAL", "TEMACESSIBILIDADERECODE", 
  "i_climgt30lt70", "i_climgt70", "TEMEQUIPAMENTOSRECODE", "i_tvgt03", 
  "COMPUTADORESPARAALUNOSRECODE", "IN_INTERNET_APRENDIZAGEM"
)

# Adicionando os itens específicos a cada régua conforme a estrutura pedagógica
itens_creche <- c(itens_base, "item_parque", "item_sanitario_ei")
itens_pre    <- c(itens_base, "item_parque", "item_sanitario_ei")
itens_efai   <- itens_base
itens_efaf   <- c(itens_base, "item_lab_ciencias", "item_lab_informatica")
itens_em     <- c(itens_base, "item_lab_ciencias", "item_lab_informatica")

message("Processando e aplicando os modelos estatísticos TRI por etapa...")
ind_creche <- calcular_indice_etapa(item_data_processed %>% filter(IN_INF_CRE == 1), itens_creche) %>% rename(ind_creche = escore_etapa)
ind_pre    <- calcular_indice_etapa(item_data_processed %>% filter(IN_INF_PRE == 1), itens_pre)    %>% rename(ind_pre = escore_etapa)
ind_efai   <- calcular_indice_etapa(item_data_processed %>% filter(IN_FUND_AI == 1), itens_efai)   %>% rename(ind_efai = escore_etapa)
ind_efaf   <- calcular_indice_etapa(item_data_processed %>% filter(IN_FUND_AF == 1), itens_efaf)   %>% rename(ind_efaf = escore_etapa)
ind_em     <- calcular_indice_etapa(item_data_processed %>% filter(IN_MED == 1),     itens_em)     %>% rename(ind_em = escore_etapa)

# --- 5. Agregação Final por Média Ponderada de Matrículas ---
message("Consolidando as réguas e calculando as médias ponderadas por entidade...")
escolas_final <- item_data_processed %>%
  select(CO_ENTIDADE, NO_ENTIDADE, SG_UF, NO_MUNICIPIO, TP_DEPENDENCIA, 
         QT_MAT_INF_CRE, QT_MAT_INF_PRE, QT_MAT_FUND_AI, QT_MAT_FUND_AF, QT_MAT_MED) %>%
  left_join(ind_creche, by = "CO_ENTIDADE") %>%
  left_join(ind_pre, by = "CO_ENTIDADE") %>%
  left_join(ind_efai, by = "CO_ENTIDADE") %>%
  left_join(ind_efaf, by = "CO_ENTIDADE") %>%
  left_join(ind_em, by = "CO_ENTIDADE") %>%
  mutate(
    # Cálculo ponderado estritamente pelas matrículas de quem possui nota válida
    numerador = coalesce(ind_creche * QT_MAT_INF_CRE, 0) + 
      coalesce(ind_pre * QT_MAT_INF_PRE, 0) + 
      coalesce(ind_efai * QT_MAT_FUND_AI, 0) + 
      coalesce(ind_efaf * QT_MAT_FUND_AF, 0) + 
      coalesce(ind_em * QT_MAT_MED, 0),
    
    denominador = if_else(!is.na(ind_creche), QT_MAT_INF_CRE, 0) + 
      if_else(!is.na(ind_pre), QT_MAT_INF_PRE, 0) + 
      if_else(!is.na(ind_efai), QT_MAT_FUND_AI, 0) + 
      if_else(!is.na(ind_efaf), QT_MAT_FUND_AF, 0) + 
      if_else(!is.na(ind_em), QT_MAT_MED, 0),
    
    Ind_entidade_final = if_else(denominador > 0, numerador / denominador, NA_real_)
  ) %>%
  mutate(
    nivel_infra = case_when(
      Ind_entidade_final < 40 ~ "Elementar", 
      Ind_entidade_final < 50 ~ "Básica", 
      Ind_entidade_final < 60 ~ "Adequada", 
      Ind_entidade_final >= 60 ~ "Avançada", 
      TRUE ~ "Sem Escala"
    )
  )

# --- 6. Exportação de Resultados ---
write_csv2(escolas_final, file.path(output_dir, "Indescola23_Final_Etapas.csv"))
message("Processo concluído com sucesso! Base de dados oficial gerada.")

# ============================================================
# Economia Brasileira e Analise de Dados
# Trabalho 1 - Brasil x Chile
# Diagnóstico macroeconômico comparado, 2000–2025
# Aluno: Rodrigo Yuta Fuziki
# RA00337065
# ============================================================

# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------
dir.create("data/raw",       recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figs",   recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

library(tidyverse)
library(wbstats)
library(lubridate)
library(patchwork)
library(gt)
library(scales)

options(scipen = 999)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# ------------------------------------------------------------
# País de comparação: Chile
# ------------------------------------------------------------
# Brasil e Chile sao economias de renda media da America Latina,
# expostas a choques externos semelhantes, como ciclos de commodities,
# crise financeira global de 2008-09 e pandemia de 2020. Apesar disso,
# apresentaram trajetorias distintas de crescimento, inflacao,
# investimento e contas externas.

# Apesar de ambos serem países latino-americanos, exportadores de commodities 
# e com regime de metas de inflação, ambos apresentam trajetórias 
# diferentes de crescimento, investimento, inflação e setor externo

# O Chile é frequentemente citado por regras fiscais e maior abertura
# comercial, enquanto o Brasil tem mercado interno maior, carga fiscal
# mais complexa e maior peso do setor publico. A comparacao permite
# avaliar como arranjos institucionais diferentes se refletem em
# indicadores macroeconomicos.

# OBSERVACAO IMPORTANTE:

# A cobertura dos dados é satisfatória nos Módulos 1, 2 e 3,porém há
# limitacoes nos modulos 4 e 5 especialmente em reservas internacionais, 
# dívida externa e dívida pública para o Chile no WDI
# Essas limitações foram mantidas como NA e documentadas no script.
# no inicio desses modulos informarei novamente sobre a falta de alguns dados

paises <- c("BR", "CL")

# ------------------------------------------------------------
# Funcoes auxiliares
# ------------------------------------------------------------
classificar_subperiodo <- function(ano) {
  case_when(
    ano <= 2004 ~ "2000-2004",
    ano <= 2009 ~ "2005-2009",
    ano <= 2014 ~ "2010-2014",
    ano <= 2019 ~ "2015-2019",
    TRUE        ~ "2020-2025"
  )
}

salvar_tabela_gt <- function(tabela, arquivo_png, titulo, subtitulo = NULL) {
  tabela_gt <- tabela |>
    gt() |>
    tab_header(
      title = titulo,
      subtitle = subtitulo
    ) |>
    fmt_number(
      columns = where(is.numeric),
      decimals = 2
    )
  
  arquivo_html <- stringr::str_replace(arquivo_png, "\\.png$", ".html")
  
  if (requireNamespace("webshot2", quietly = TRUE)) {
    tryCatch(
      gtsave(tabela_gt, arquivo_png),
      error = function(e) {
        gtsave(tabela_gt, arquivo_html)
        message("Nao foi possivel salvar PNG da tabela; tabela salva em HTML: ", arquivo_html)
      }
    )
  } else {
    gtsave(tabela_gt, arquivo_html)
    message("Tabela salva em HTML porque o pacote webshot2 nao esta instalado: ", arquivo_html)
  }
}


baixar_wdi_cache <- function(indicadores, arquivo_cache) {
  if (file.exists(arquivo_cache)) {
    readr::read_csv(arquivo_cache, show_col_types = FALSE)
  } else {
    dados <- wb_data(
      indicator  = indicadores,
      country    = paises,
      start_date = 2000,
      end_date   = 2025
    )
    
    readr::write_csv(dados, arquivo_cache)
    dados
  }
}

# ------------------------------------------------------------
# Indicadores WDI usados no trabalho
# ------------------------------------------------------------
# Modulo 1:
# NY.GDP.MKTP.KD.ZG    - PIB real, crescimento anual (% a.a.)
# NY.GDP.PCAP.PP.KD    - PIB per capita PPP, US$ constantes
# NE.CON.PRVT.ZS       - Consumo das familias (% PIB)
# NE.CON.GOVT.ZS       - Consumo do governo (% PIB)
# NE.GDI.FTOT.ZS       - Formacao bruta de capital fixo (% PIB)
# NE.RSB.GNFS.ZS       - Exportacoes liquidas de bens e servicos (% PIB)
#
# Modulo 2:
# NY.GNS.ICTR.ZS       - Poupanca bruta (% PIB)
# BX.KLT.DINV.WD.GD.ZS - IED, entrada liquida (% PIB)
#
# Modulo 3:
# FP.CPI.TOTL.ZG       - Inflacao ao consumidor, CPI (% a.a.)
# PA.NUS.FCRF          - Taxa de cambio oficial, moeda local por US$
# FR.INR.DPST          - Taxa de juros de deposito (%), proxy para juros
#
# Modulo 4:
# BN.CAB.XOKA.GD.ZS    - Conta corrente (% PIB)
# FI.RES.TOTL.DT.ZS    - Reservas internacionais (meses de importacao)
# PX.REX.REER          - Cambio real efetivo, indice 2010 = 100
# DT.DOD.DECT.GN.ZS    - Divida externa total (% RNB)
# BX.PEF.TOTL.CD.WD    - Investimento em carteira, entradas liquidas (US$)
# NY.GDP.MKTP.CD       - PIB corrente (US$), usado para converter carteira para % PIB
#
# Modulo 5:
# GC.DOD.TOTL.GD.ZS    - Divida publica bruta (% PIB)
# GC.NLD.TOTL.GD.ZS    - Resultado fiscal: net lending (+) / net borrowing (-), % PIB
# SL.UEM.TOTL.ZS       - Taxa de desemprego (% da forca de trabalho)
# GC.XPN.TOTL.GD.ZS    - Gasto publico total (% PIB)

indicadores_wdi <- c(
  pib_crescimento       = "NY.GDP.MKTP.KD.ZG",
  pib_pc_ppp            = "NY.GDP.PCAP.PP.KD",
  consumo_familias      = "NE.CON.PRVT.ZS",
  consumo_governo       = "NE.CON.GOVT.ZS",
  fbcf_pct_pib          = "NE.GDI.FTOT.ZS",
  exp_liquidas_pct_pib  = "NE.RSB.GNFS.ZS",
  poupanca_bruta        = "NY.GNS.ICTR.ZS",
  ied_pct_pib           = "BX.KLT.DINV.WD.GD.ZS",
  inflacao_cpi          = "FP.CPI.TOTL.ZG",
  cambio_nominal        = "PA.NUS.FCRF",
  juros_deposito        = "FR.INR.DPST",
  conta_corrente        = "BN.CAB.XOKA.GD.ZS",
  reservas_meses        = "FI.RES.TOTL.DT.ZS",
  cambio_real_efetivo   = "PX.REX.REER",
  divida_externa        = "DT.DOD.DECT.GN.ZS",
  carteira_usd          = "BX.PEF.TOTL.CD.WD",
  pib_corrente_usd      = "NY.GDP.MKTP.CD",
  divida_publica        = "GC.DOD.TOTL.GD.ZS",
  resultado_fiscal      = "GC.NLD.TOTL.GD.ZS",
  desemprego            = "SL.UEM.TOTL.ZS",
  gasto_publico         = "GC.XPN.TOTL.GD.ZS"
)

dados_wdi_raw <- baixar_wdi_cache(
  indicadores_wdi,
  "data/raw/wdi_brasil_chile_2000_2025.csv"
)

dados_wdi <- dados_wdi_raw |>
  rename(
    ano = date,
    iso3 = iso3c,
    pais = country
  ) |>
  mutate(
    pais = case_when(
      iso3 == "BRA" ~ "Brasil",
      iso3 == "CHL" ~ "Chile",
      TRUE          ~ pais
    ),
    subperiodo = classificar_subperiodo(ano),
    carteira_pct_pib = (carteira_usd / pib_corrente_usd) * 100,
    conta_financeira_proxy = ied_pct_pib + carteira_pct_pib,
    hiato_poupanca_investimento = poupanca_bruta - fbcf_pct_pib
  ) |>
  arrange(pais, ano)

readr::write_csv(dados_wdi, "data/processed/wdi_brasil_chile_2000_2025_limpo.csv")

cobertura_wdi <- dados_wdi |>
  group_by(pais) |>
  summarise(
    n_anos = n(),
    across(
      c(pib_crescimento, pib_pc_ppp, fbcf_pct_pib, inflacao_cpi,
        cambio_nominal, conta_corrente, reservas_meses, divida_publica,
        desemprego, gasto_publico),
      ~ sum(!is.na(.x)),
      .names = "com_{.col}"
    ),
    .groups = "drop"
  )

readr::write_csv(cobertura_wdi, "outputs/tables/00_cobertura_wdi.csv")

# ============================================================
# MODULO 1 - Crescimento e ciclo economico
# ============================================================

# Grafico 1: crescimento do PIB real
p1_crescimento <- dados_wdi |>
  filter(!is.na(pib_crescimento)) |>
  ggplot(aes(x = ano, y = pib_crescimento, color = pais)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  scale_color_manual(values = c("Brasil" = "steelblue", "Chile" = "firebrick")) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Crescimento do PIB real",
    subtitle = "Brasil x Chile, 2000-2025",
    x = NULL,
    y = "% a.a.",
    color = "Pais",
    caption = "Fonte: World Bank/WDI via wbstats. Indicador NY.GDP.MKTP.KD.ZG."
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

p1_crescimento
ggsave("outputs/figs/01_pib_crescimento_brasil_chile.png",
       p1_crescimento, width = 9, height = 5)

# Grafico 2: decomposicao do PIB pela otica da demanda
pib_demanda_longo <- dados_wdi |>
  select(pais, ano, consumo_familias, consumo_governo,
         fbcf_pct_pib, exp_liquidas_pct_pib) |>
  pivot_longer(
    cols = c(consumo_familias, consumo_governo, fbcf_pct_pib, exp_liquidas_pct_pib),
    names_to = "componente",
    values_to = "pct_pib"
  ) |>
  mutate(
    componente = factor(
      componente,
      levels = c("consumo_familias", "consumo_governo", "fbcf_pct_pib", "exp_liquidas_pct_pib"),
      labels = c("Consumo das familias", "Consumo do governo",
                 "Investimento (FBCF)", "Exportacoes liquidas")
    )
  )

p1_demanda <- pib_demanda_longo |>
  filter(!is.na(pct_pib)) |>
  ggplot(aes(x = ano, y = pct_pib, fill = componente)) +
  geom_col(position = "stack", width = 0.85) +
  facet_wrap(~ pais, ncol = 1) +
  scale_fill_manual(values = c(
    "Consumo das familias" = "steelblue",
    "Consumo do governo" = "darkgreen",
    "Investimento (FBCF)" = "firebrick",
    "Exportacoes liquidas" = "goldenrod"
  )) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "PIB pela otica da demanda",
    subtitle = "Componentes em % do PIB",
    x = NULL,
    y = "% do PIB",
    fill = "Componente",
    caption = "Fonte: World Bank/WDI via wbstats."
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

p1_demanda
ggsave("outputs/figs/01_pib_demanda_brasil_chile.png",
       p1_demanda, width = 10, height = 8)

tab1_crescimento <- dados_wdi |>
  group_by(pais, subperiodo) |>
  summarise(
    crescimento_medio = mean(pib_crescimento, na.rm = TRUE),
    pib_pc_ppp_medio = mean(pib_pc_ppp, na.rm = TRUE),
    anos_validos_crescimento = sum(!is.na(pib_crescimento)),
    anos_validos_pib_pc_ppp = sum(!is.na(pib_pc_ppp)),
    .groups = "drop"
  )

readr::write_csv(tab1_crescimento, "outputs/tables/01_tabela_crescimento.csv")
salvar_tabela_gt(
  tab1_crescimento,
  "outputs/figs/01_tabela_crescimento.png",
  "Modulo 1 - Crescimento e ciclo economico",
  "Medias por subperiodo, Brasil x Chile"
)

View(tab1_crescimento)

# Entre 2000 e 2025, o Chile cresceu mais que o Brasil em média: 3,45% a.a. contra 2,38% a.a.. 
# Essa diferença reflete, em parte, o maior dinamismo chileno nos anos 2000 e início dos anos 2010, 
# período favorecido pelo ciclo de commodities, especialmente o cobre

# O contraste fica mais forte em 2015-2019: o Brasil teve crescimento médio de -0,50%, 
# afetado pela recessão de 2015-2016, crise fiscal e instabilidade política, 
# enquanto o Chile ainda cresceu 1,98% a.a.. 
# Em 2020, os dois países sofreram com a pandemia, 
# mas o Chile teve queda mais intensa do PIB (-6,14%) que o Brasil (-3,28%), 
# seguida de forte recuperação em 2021 (11,31%)

# ============================================================
# MODULO 2 - Investimento e poupanca
# ============================================================

p2_fbcf <- dados_wdi |>
  filter(!is.na(fbcf_pct_pib)) |>
  ggplot(aes(x = ano, y = fbcf_pct_pib, color = pais)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  scale_color_manual(values = c("Brasil" = "steelblue", "Chile" = "firebrick")) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Formacao bruta de capital fixo",
    subtitle = "Brasil x Chile, 2000-2025",
    x = NULL,
    y = "% do PIB",
    color = "Pais",
    caption = "Fonte: World Bank/WDI via wbstats. Indicador NE.GDI.FTOT.ZS."
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

p2_fbcf
ggsave("outputs/figs/02_fbcf_brasil_chile.png",
       p2_fbcf, width = 9, height = 5)

p2_ied <- dados_wdi |>
  filter(!is.na(ied_pct_pib)) |>
  ggplot(aes(x = ano, y = ied_pct_pib, color = pais)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  scale_color_manual(values = c("Brasil" = "steelblue", "Chile" = "firebrick")) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Investimento estrangeiro direto",
    subtitle = "Entradas liquidas de IED, Brasil x Chile",
    x = NULL,
    y = "% do PIB",
    color = "Pais",
    caption = "Fonte: World Bank/WDI via wbstats. Indicador BX.KLT.DINV.WD.GD.ZS."
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

p2_ied
ggsave("outputs/figs/02_ied_brasil_chile.png",
       p2_ied, width = 9, height = 5)

tab2_investimento <- dados_wdi |>
  group_by(pais, subperiodo) |>
  summarise(
    fbcf_media = mean(fbcf_pct_pib, na.rm = TRUE),
    poupanca_media = mean(poupanca_bruta, na.rm = TRUE),
    ied_media = mean(ied_pct_pib, na.rm = TRUE),
    hiato_poupanca_investimento = mean(hiato_poupanca_investimento, na.rm = TRUE),
    anos_validos = sum(!is.na(fbcf_pct_pib)),
    .groups = "drop"
  )

readr::write_csv(tab2_investimento, "outputs/tables/02_tabela_investimento.csv")
salvar_tabela_gt(
  tab2_investimento,
  "outputs/figs/02_tabela_investimento.png",
  "Modulo 2 - Investimento e poupanca",
  "Medias por subperiodo, Brasil x Chile"
)

View(tab2_investimento)

# O Chile manteve taxa de investimento superior 
# a brasileira em todos os subperíodos: na média geral, 23,71% do PIB, contra 17,84% no Brasil. 
# Isso ajuda a explicar parte da diferença de crescimento e de PIB per capita entre os dois países.

# No Brasil, a FBCF caiu fortemente no período 2015-2019, para 15,70% do PIB, refletindo a recessão, 
# a queda da confiança empresarial e a redução do investimento público e privado. 
# No Chile, a FBCF permaneceu mais alta, em 23,89% do PIB, embora o hiato poupança-investimento 
# tenha piorado, indicando maior necessidade de financiamento externo.

# ============================================================
# MODULO 3 - Inflacao e politica monetaria
# ============================================================

p3_inflacao <- dados_wdi |>
  filter(!is.na(inflacao_cpi)) |>
  ggplot(aes(x = ano, y = inflacao_cpi, color = pais)) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  scale_color_manual(values = c("Brasil" = "steelblue", "Chile" = "firebrick")) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Inflacao ao consumidor",
    subtitle = "CPI, Brasil x Chile",
    x = NULL,
    y = "% a.a.",
    color = "Pais",
    caption = "Fonte: World Bank/WDI via wbstats. Indicador FP.CPI.TOTL.ZG. Linha tracejada = 10% a.a."
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

p3_inflacao
ggsave("outputs/figs/03_inflacao_cpi_brasil_chile.png",
       p3_inflacao, width = 9, height = 5)

p3_cambio <- dados_wdi |>
  filter(!is.na(cambio_nominal)) |>
  ggplot(aes(x = ano, y = cambio_nominal)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "darkblue", size = 1.4) +
  facet_wrap(~ pais, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title = "Taxa de cambio nominal",
    subtitle = "Moeda local por US$, media do periodo",
    x = NULL,
    y = "Moeda local / US$",
    caption = "Fonte: World Bank/WDI via wbstats. Indicador PA.NUS.FCRF."
  ) +
  theme_classic(base_size = 12)

p3_cambio
ggsave("outputs/figs/03_cambio_nominal_brasil_chile.png",
       p3_cambio, width = 9, height = 7)

p3_juros <- dados_wdi |>
  filter(!is.na(juros_deposito)) |>
  ggplot(aes(x = ano, y = juros_deposito, color = pais)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  scale_color_manual(values = c("Brasil" = "steelblue", "Chile" = "firebrick")) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Taxa de juros de deposito",
    subtitle = "Proxy de juros quando a taxa basica nao esta disponivel no WDI",
    x = NULL,
    y = "% a.a.",
    color = "Pais",
    caption = "Fonte: World Bank/WDI via wbstats. Indicador FR.INR.DPST."
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

p3_juros
ggsave("outputs/figs/03_juros_deposito_brasil_chile.png",
       p3_juros, width = 9, height = 5)

tab3_inflacao <- dados_wdi |>
  group_by(pais, subperiodo) |>
  summarise(
    inflacao_media = mean(inflacao_cpi, na.rm = TRUE),
    cambio_nominal_medio = mean(cambio_nominal, na.rm = TRUE),
    juros_deposito_medio = mean(juros_deposito, na.rm = TRUE),
    anos_validos = sum(!is.na(inflacao_cpi)),
    .groups = "drop"
  )

readr::write_csv(tab3_inflacao, "outputs/tables/03_tabela_inflacao_cambio.csv")
salvar_tabela_gt(
  tab3_inflacao,
  "outputs/figs/03_tabela_inflacao_cambio.png",
  "Modulo 3 - Inflacao e politica monetaria",
  "Medias por subperiodo, Brasil x Chile"
)

View(tab3_inflacao)

# O Brasil teve inflação média mais alta no período completo: 
# 6,28% a.a., contra 3,77% a.a. no Chile. 
# Isso reflete maior persistência inflacionária no Brasil, associada a choques cambiais, 
# indexação mais elevada e maior volatilidade macroeconômica

# No período recente, porém, o Chile também sofreu forte aceleração inflacionária: em 2022, 
# a inflação chilena chegou a 11,64%, superando a brasileira (9,28%). 
# Esse movimento está ligado ao choque global pós-pandemia, 
# aumento de preços internacionais, estímulos internos e pressões cambiais.

# ============================================================
# MODULO 4 - Setor externo e cambio
# ============================================================

# Como dito no inicio do script, esse modulo estara com menos informacoes
# devido a falta de dados do Chile sobre reservas internacionais
# e divida externa no WDI

externo_longo <- dados_wdi |>
  select(pais, ano, conta_corrente, exp_liquidas_pct_pib) |>
  pivot_longer(
    cols = c(conta_corrente, exp_liquidas_pct_pib),
    names_to = "indicador",
    values_to = "valor"
  ) |>
  mutate(
    indicador = factor(
      indicador,
      levels = c("conta_corrente", "exp_liquidas_pct_pib"),
      labels = c("Conta corrente", "Balanca comercial")
    )
  )

p4_contas_externas <- externo_longo |>
  filter(!is.na(valor)) |>
  ggplot(aes(x = ano, y = valor, color = pais)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  facet_wrap(~ indicador, ncol = 1) +
  scale_color_manual(values = c("Brasil" = "steelblue", "Chile" = "firebrick")) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Conta corrente e balanca comercial",
    subtitle = "Brasil x Chile, em % do PIB",
    x = NULL,
    y = "% do PIB",
    color = "Pais",
    caption = "Fonte: World Bank/WDI via wbstats. Indicadores BN.CAB.XOKA.GD.ZS e NE.RSB.GNFS.ZS."
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

p4_contas_externas
ggsave("outputs/figs/04_conta_corrente_balanca_brasil_chile.png",
       p4_contas_externas, width = 9, height = 7)

p4_reservas <- dados_wdi |>
  filter(!is.na(reservas_meses)) |>
  ggplot(aes(x = ano, y = reservas_meses, color = pais)) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "red") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  annotate("text", x = 2001, y = 3.4, label = "Regra de 3 meses",
           color = "red", hjust = 0, size = 3) +
  scale_color_manual(values = c("Brasil" = "steelblue", "Chile" = "firebrick")) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title = "Reservas internacionais",
    subtitle = "Meses de importacao, Brasil x Chile",
    x = NULL,
    y = "Meses de importacao",
    color = "Pais",
    caption = "Fonte: World Bank/WDI via wbstats. Indicador FI.RES.TOTL.DT.ZS."
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

p4_reservas
ggsave("outputs/figs/04_reservas_meses_brasil_chile.png",
       p4_reservas, width = 9, height = 5)

p4_reer <- dados_wdi |>
  filter(!is.na(cambio_real_efetivo)) |>
  ggplot(aes(x = ano, y = cambio_real_efetivo)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "darkblue", size = 1.4) +
  facet_wrap(~ pais, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title = "Cambio real efetivo",
    subtitle = "Indice 2010 = 100",
    x = NULL,
    y = "Indice",
    caption = "Fonte: World Bank/WDI via wbstats. Indicador PX.REX.REER."
  ) +
  theme_classic(base_size = 12)

p4_reer
ggsave("outputs/figs/04_cambio_real_efetivo_brasil_chile.png",
       p4_reer, width = 9, height = 7)

tab4_externo <- dados_wdi |>
  group_by(pais, subperiodo) |>
  summarise(
    balanca_comercial_media = mean(exp_liquidas_pct_pib, na.rm = TRUE),
    conta_corrente_media = mean(conta_corrente, na.rm = TRUE),
    ied_media = mean(ied_pct_pib, na.rm = TRUE),
    carteira_media = mean(carteira_pct_pib, na.rm = TRUE),
    conta_financeira_proxy = mean(conta_financeira_proxy, na.rm = TRUE),
    reservas_media = mean(reservas_meses, na.rm = TRUE),
    cambio_real_efetivo_medio = mean(cambio_real_efetivo, na.rm = TRUE),
    divida_externa_media = mean(divida_externa, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(tab4_externo, "outputs/tables/04_tabela_setor_externo.csv")
salvar_tabela_gt(
  tab4_externo,
  "outputs/figs/04_tabela_setor_externo.png",
  "Modulo 4 - Setor externo e cambio",
  "Medias por subperiodo, Brasil x Chile"
)

View(tab4_externo)

# O Chile apresentou forte superávit comercial em 2005-2009, com média de 8,96% do PIB, 
# período associado ao boom das commodities e aos altos preços do cobre. 
# O Brasil também melhorou seu saldo comercial nesse período, mas em escala menor, com média de 1,38% do PIB.

# A partir de 2010-2014, os dois países passaram a registrar déficits em conta corrente mais elevados. 
# No Chile, o déficit chegou a -3,52% do PIB, refletindo maior dependência externa e os ciclos do cobre; 
# no Brasil, o déficit foi de -3,79%, associado ao crescimento da demanda doméstica, 
# apreciação cambial em parte do período e piora da competitividade industrial.

# ============================================================
# MODULO 5 - Condicoes fiscais e mercado de trabalho
# ============================================================

# Como dito no comeco do script, assim como no módulo 4
# existe uma falta de dados nesse modulo 5, pois
# a divida publica do Chile possui apenas um ano valido no WDI

p5_divida <- dados_wdi |>
  filter(!is.na(divida_publica)) |>
  ggplot(aes(x = ano, y = divida_publica, color = pais)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  scale_color_manual(values = c("Brasil" = "steelblue", "Chile" = "firebrick")) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Divida publica bruta",
    subtitle = "Brasil x Chile",
    x = NULL,
    y = "% do PIB",
    color = "Pais",
    caption = "Fonte: World Bank/WDI via wbstats. Indicador GC.DOD.TOTL.GD.ZS."
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

p5_divida
ggsave("outputs/figs/05_divida_publica_brasil_chile.png",
       p5_divida, width = 9, height = 5)

p5_desemprego <- dados_wdi |>
  filter(!is.na(desemprego)) |>
  ggplot(aes(x = ano, y = desemprego, color = pais)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  scale_color_manual(values = c("Brasil" = "steelblue", "Chile" = "firebrick")) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Taxa de desemprego",
    subtitle = "Brasil x Chile",
    x = NULL,
    y = "% da forca de trabalho",
    color = "Pais",
    caption = "Fonte: World Bank/WDI via wbstats. Indicador SL.UEM.TOTL.ZS."
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

p5_desemprego
ggsave("outputs/figs/05_desemprego_brasil_chile.png",
       p5_desemprego, width = 9, height = 5)

p5_fiscal <- dados_wdi |>
  select(pais, ano, resultado_fiscal, gasto_publico) |>
  pivot_longer(
    cols = c(resultado_fiscal, gasto_publico),
    names_to = "indicador",
    values_to = "valor"
  ) |>
  mutate(
    indicador = factor(
      indicador,
      levels = c("resultado_fiscal", "gasto_publico"),
      labels = c("Resultado fiscal", "Gasto publico")
    )
  ) |>
  filter(!is.na(valor)) |>
  ggplot(aes(x = ano, y = valor, color = pais)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  facet_wrap(~ indicador, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Brasil" = "steelblue", "Chile" = "firebrick")) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Indicadores fiscais",
    subtitle = "Resultado fiscal e gasto publico, Brasil x Chile",
    x = NULL,
    y = "% do PIB",
    color = "Pais",
    caption = "Fonte: World Bank/WDI via wbstats. Indicadores GC.NLD.TOTL.GD.ZS e GC.XPN.TOTL.GD.ZS."
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

p5_fiscal
ggsave("outputs/figs/05_resultado_gasto_publico_brasil_chile.png",
       p5_fiscal, width = 9, height = 7)

tab5_fiscal_trabalho <- dados_wdi |>
  group_by(pais, subperiodo) |>
  summarise(
    divida_publica_media = mean(divida_publica, na.rm = TRUE),
    resultado_fiscal_medio = mean(resultado_fiscal, na.rm = TRUE),
    desemprego_medio = mean(desemprego, na.rm = TRUE),
    gasto_publico_medio = mean(gasto_publico, na.rm = TRUE),
    anos_validos_divida = sum(!is.na(divida_publica)),
    anos_validos_desemprego = sum(!is.na(desemprego)),
    .groups = "drop"
  )

readr::write_csv(tab5_fiscal_trabalho, "outputs/tables/05_tabela_fiscal_trabalho.csv")
salvar_tabela_gt(
  tab5_fiscal_trabalho,
  "outputs/figs/05_tabela_fiscal_trabalho.png",
  "Modulo 5 - Fiscal e mercado de trabalho",
  "Medias por subperiodo, Brasil x Chile"
)

View(tab5_fiscal_trabalho)

# No Brasil, o resultado fiscal se deteriorou fortemente após 2014. 
# Em 2015-2019, o déficit médio foi de -6,98% do PIB, refletindo os efeitos da recessão, 
# queda de arrecadação, rigidez do gasto público e crise fiscal

# O Chile também piorou fiscalmente, mas em menor intensidade: 
# o déficit médio foi de -2,40% do PIB em 2015-2019 e -3,80% em 2020-2025. 
# A piora recente está associada à pandemia, aos pacotes de apoio econômico e à desaceleração posterior.

# No mercado de trabalho, os dois países sofreram impacto forte em 2020. 
# O desemprego chegou a 13,70% no Brasil e 10,93% no Chile, refletindo o choque da pandemia 
# sobre serviços, comércio e ocupações presenciais.


# ============================================================
# SINTESE FINAL
# ============================================================

tab_sintese <- dados_wdi |>
  group_by(pais) |>
  summarise(
    crescimento_medio_2000_2025 = mean(pib_crescimento, na.rm = TRUE),
    inflacao_media_2000_2025 = mean(inflacao_cpi, na.rm = TRUE),
    fbcf_media_2000_2025 = mean(fbcf_pct_pib, na.rm = TRUE),
    conta_corrente_media_2000_2025 = mean(conta_corrente, na.rm = TRUE),
    divida_publica_media_2000_2025 = mean(divida_publica, na.rm = TRUE),
    desemprego_medio_2000_2025 = mean(desemprego, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(tab_sintese, "outputs/tables/06_tabela_sintese.csv")
salvar_tabela_gt(
  tab_sintese,
  "outputs/figs/06_tabela_sintese.png",
  "Sintese comparativa",
  "Medias gerais, Brasil x Chile"
)

View(tab_sintese)

# A comparação mostra que o Chile teve, em média, maior crescimento, 
# maior investimento e inflação mais baixa que o Brasil entre 2000 e 2025. 
# Esses resultados estão ligados a uma economia mais aberta, 
# maior exposição ao ciclo do cobre e maior taxa de investimento.

# O Brasil, por outro lado, apresentou maior instabilidade macroeconômica 
# no período, especialmente após a recessão de 2015-2016, com crescimento menor, 
# inflação média mais alta e deterioração fiscal mais intensa. 
# Apesar disso, ambos os países foram afetados por choques comuns, 
# como a crise global de 2008-2009, o fim do superciclo de commodities, 
# a pandemia de 2020 e o choque inflacionário de 2021-2022.

# ============================================================

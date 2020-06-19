library(tidyverse)

# 1: bajo la base de respuestas --------------------------------------------

library(fireData)

api_key <- Sys.getenv("API_KEY")
db_url <- Sys.getenv("DATABASE_URL")
project_id <- Sys.getenv("PROJECT_ID")
project_domain <- Sys.getenv("AUTH_DOMAIN")

# bajo la base a una lista
firebase <- fireData::download(projectURL = db_url, fileName = "evocaciones") 

# la divido en 3 tablas: sociodemograficos, terminos (formato tidy) y estimulos
sociodemograficos <- terminos <- estimulos <- data.frame() 
for (i in 1:length(firebase)) {
  sociodemograficos <- rbind(sociodemograficos,cbind( id = firebase[[i]]$id , as.data.frame(firebase[[i]]$sociodemo)))
  terminos <- rbind(terminos,cbind( id = firebase[[i]]$id , firebase[[i]]$terms))
  estimulos <- rbind(estimulos, cbind(id = firebase[[i]]$id, estimulo = firebase[[i]]$estimulo, tiempo = firebase[[i]]$tiempo))  
}
rm(firebase,i)
rm(api_key, db_url, project_domain, project_id)

# 2: datos de la muestra -------------------------------------------------

table(estimulos$estimulo)
respuestas <- terminos %>% pull(id) %>% as.character() %>% unique()

estimulos_bd <- estimulos %>% filter(estimulo == "Big data") %>% pull(id) %>% as.character()
socio_bd <- sociodemograficos %>% filter( id %in% estimulos_bd, 
                                          id %in% respuestas,
                                          edad != "2" )

rm(respuestas)

# n
nrow(socio_bd)

# edad
summary(as.numeric(as.character(socio_bd$edad)))
sd(as.numeric(as.character(socio_bd$edad)))

# sexo
table(socio_bd$sexo) / nrow(socio_bd) * 100

sort(table(socio_bd$carrera))
sort(table(socio_bd$carrera)) / nrow(socio_bd) * 100

# agrupar carreras 
socio_bd <- socio_bd %>% mutate(
  carrera2 = case_when(
    carrera %in% c("Matemáticas", "Computación e Informática", "Física", "Química", "Medio ambiente", "Biología", "Otras Naturales y Exactas") ~ "Naturales y exactas" , 
    carrera %in% c("Ing. Civil", "Ing. Eléctrica y de la Información", "Ing. Mecánica", "Ing. Química", "Ing. Médica", "Ing. del Medio Ambiente", "Biotecnología", "Nanotecnología", "Otras Ingenierías") ~ "Ingenierías y tecnologías" ,
    # carrera %in% c("Psicología", "Medicina", "Ciencias de la Salud", "Biotecnología", "Otras Médicas y de la Salud") ~ "Médicas y de la salud" ,
    carrera %in% c("Medicina", "Ciencias de la Salud", "Biotecnología", "Otras Médicas y de la Salud") ~ "Médicas y de la salud" ,
    carrera %in% c("Agricultura", "Producción Animal", "Veterinarias", "Biotecnología Agropecuaria", "Otras de Ciencias Agrícolas") ~ "Ciencias agrícolas" ,
    # carrera %in% c("Economía, Negocios y Administración", "Educación", "Sociología y Política", "Urbanismo, Geografía y Arquitectura", "Comunicación y Medios", "Turismo, Eventos y Gastronomía", "Derecho", "Otras Ciencias Sociales y empresariales") ~ "Sociales y empresariales" ,
    carrera %in% c("Psicología", "Economía, Negocios y Administración", "Educación", "Sociología y Política", "Urbanismo, Geografía y Arquitectura", "Comunicación y Medios", "Turismo, Eventos y Gastronomía", "Derecho", "Otras Ciencias Sociales y empresariales") ~ "Sociales y empresariales" ,
    carrera %in% c("Historia y Antropología", "Lengua y Literatura", "Filosofía y Religión", "Arte", "Otras Humanidades") ~ "Humanidades"
  )
)

table(socio_bd$carrera2)
tabla_carrera <- socio_bd %>% 
  group_by(carrera2) %>%
  summarise(n=n(), p=n() / nrow(socio_bd)) %>% inner_join(
    socio_bd %>% 
      group_by(carrera2) %>%
      summarise_if( .predicate = is.logical, .funs = function(x) sum(x) / n() ) , by="carrera2") 
tabla_carrera 

socio_bd %>% 
  group_by(carrera2) %>%
  summarise_if( .predicate = is.logical, .funs = function(x) sum(x) / n() )

# 3: limpiamos y lemmatizamos ------------------------------------------------

terminos_old <- terminos
udmodel <- udpipe::udpipe_load_model(file = "../spanish-gsd-ud-2.4-190531.udpipe")
terminos$id_orden <- paste(terminos$id,terminos$orden, sep = "$") # ponemos un id para las palabras
terminos$palabra <- tolower(trimws(terminos$palabra)) # minusculas
terminos$palabra <- sub(pattern = "rse$", replacement = "r", x = terminos$palabra) # borramos la declinacion "rse"
terminos_tagged <- udpipe::udpipe_annotate(udmodel, x = terminos$palabra, trace=100, doc_id = terminos$id_orden)
terminos_tagged <- as.data.frame(terminos_tagged)
terminos_tagged$lemma <- stringi::stri_trans_general(terminos_tagged$lemma, "Latin-ASCII") # sacamos acentos
terminos_tagged$lemma <- tolower(terminos_tagged$lemma)

lemmas <-   terminos_tagged %>% 
  filter(!is.na(upos), !upos %in% c("PUNCT","ADP") ) %>% 
  group_by(doc_id) %>%  
  summarize(lemma=paste(lemma, collapse = "_")) %>%
  rename(id_orden=doc_id)

terminos <- terminos %>% left_join( lemmas, by = "id_orden" )
rm(lemmas)

length(unique(terminos_old$palabra)) # palabras unicas
plot(table(table(terminos_old$palabra))) # frecuencias de palabras

length(unique(terminos$lemma)) # palabras unicas
plot(table(table(terminos$lemma))) # frecuencias de palabras

rm(udmodel)
rm(terminos_tagged, terminos_old)
terminos <- terminos %>% select(id, orden, palabra=lemma, valoracion)

terminos_bd <- terminos %>% filter( id %in% estimulos_bd, !is.na(palabra))

# corpus especifico de bd
length(unique(terminos_bd$palabra)) # palabras unicas
plot(table(table(terminos_bd$palabra))) # frecuencias de palabras

# 4: analisis de evocacion -----------------------------------------------

crear_evok <- function( tabla, palabra, orden, valoracion, frecuencia_minima = 2) {
  
  stopifnot(is.data.frame(tabla))
  stopifnot(is.numeric(frecuencia_minima))
  
  palabra_column = enquo(arg = palabra)
  orden_column = enquo(arg = orden)
  valor_column = enquo(arg = valoracion)
  
  evok <- tabla %>% 
    group_by(!!palabra_column) %>% summarise(
      n=n(), # frecuencia
      ofe=mean(!!orden_column), # media de orden de evocacion
      v=mean(!!valor_column),   # media de valoracion
      p=n()
    )     
  
  evok <- evok %>% 
    group_by(!!palabra_column) %>% 
    filter( n >= frecuencia_minima)
  
  evok$p = evok$n / sum( evok$n )
  
  # 2do: permitir meter estos valores como args
  
  n_cut <- mean(evok$n) # cut-off media de frecuencias de evocacion
  ofe_cut <- mean(evok$ofe) # cut-off between high and low rank = a mean split was used (De Rosa, 2003)
  
  message("n_cut = ", n_cut)
  message("ofe_cut = ", ofe_cut)
  
  evok <- evok %>% mutate( q = case_when(
    n >= n_cut & ofe < ofe_cut ~ 1,
    n >= n_cut & ofe >= ofe_cut ~ 2, 
    n < n_cut & ofe < ofe_cut ~ 3, 
    n < n_cut & ofe >= ofe_cut ~ 4 # o cuatro?
  )
  ) 
  
  evok <- evok %>% arrange(q,desc(n,ofe))
  
  return( 
    list( 
      data = evok ,
      n_cut = n_cut ,
      ofe_cut = ofe_cut
    )
  )
}

evok_bd <- crear_evok(tabla = terminos_bd, palabra = palabra, orden = orden, valoracion = valoracion, frecuencia_minima = 5)
evok_bd

evok_bd$data %>% ggplot(aes(x=p,y=ofe,label=palabra)) + # terminos x n x ofe
  scale_x_continuous(trans='log2') +
  geom_hline(yintercept = evok_bd$ofe_cut, linetype = 2) +
  geom_vline(xintercept = evok_bd$n_cut/sum(evok_bd$data$n), linetype = 2) +
  # geom_point(aes(size=n, colour=v)) +
  scale_colour_gradient(low = "red", high = "green", na.value = NA) +
  geom_label(aes(size=n, fill=v), colour = "white", fontface = "bold", show.legend = FALSE)+
  scale_fill_gradient(low = "red", high = "green", na.value = NA) +
  # geom_text( aes(size=20, colour=v), fontface = "bold", show.legend = FALSE, nudge_y = -.07, check_overlap = TRUE) +
  labs(y="Orden de evocación", x = "Frecuencia (log2)") +
  theme_minimal()

# 5: pca / factorial -----------------------------------------------

terminos_carreras <- terminos_bd %>%
  filter(palabra %in% evok_bd$data$palabra) %>%
  left_join(socio_bd) %>%
  group_by(palabra,carrera2) %>% summarize(n=n()) %>% 
  pivot_wider(names_from = carrera2, values_from = n, values_fill = 0)
terminos_carreras <- as.data.frame(terminos_carreras)
rownames(terminos_carreras) <- terminos_carreras %>% pull(palabra)
terminos_carreras <- terminos_carreras %>% select(-palabra)

terminos_carreras

terminos_carreras2 <- scale(terminos_carreras, center = FALSE, scale = TRUE)

# estilo lebart

afcor <- function(X) {
  
  # function Y = afcor(X) : Correspondence Analysis of matrix X
  
  # Example for a contingency table "tab.csv" in the folder ADT_R
  # (the source code:  - afcor_E.r -  being in the same folder)
  # source("c:/ADT_R/afcor_E.r")
  # path = "c:/ADT_R/tab.csv"
  # path leads to an Excel(c) file [format csv]
  # X = read.csv(path, row.names = 1, sep = ";")
  # Y = afcor(X)
  # plot_simult(Y) # simultaneous plot of rows and columns of the data table
  
  n  <- dim(X)[1]  
  p <- dim(X)[2]
  id_lig <- dimnames(X)[[1]]   #identifier of rows
  id_col <- dimnames(X)[[2]]   #identifier of columns
  K  <- matrix(as.matrix(X), nrow = n, ncol = p)
  
  #  rank: max number of non-zero eigenvalues
  dimK <- dim(K)
  rang <- min(dimK) -1 
  som <- sum(K) ; F <- K/som
  fi <- apply(F, 1, sum) 
  fj <- apply(F, 2, sum)
  
  #  Matrix to be decomposed
  fifj     <- fi %*% t(fj)
  S      <- (F - fifj) / sqrt(fifj)
  #  Singular Values Decomposition
  decomp    <- svd(S)  
  vs     <- decomp$d[1:rang]    # singular values
  u      <- decomp$u     # u eigenvectors (n, rank)
  v      <- decomp$v     # v eigenvectors (n, rank)
  vp     <- vs^2         # vp eigenvalues
  chi_2  <- som*sum(vp)
  
  #  Unitary coordinates (norm 1)
  phinorm1 <- as.matrix(u[,1:rang]) / sqrt(fi)
  psinorm1 <- as.matrix(v[,1:rang]) / sqrt(fj)
  
  # Coordinates (variance vp)
  phi = t(vs*t(phinorm1))
  psi = t(vs*t(psinorm1))
  
  #  Output list:
  afcor.output <-  list(   
    vp         = vp, 
    id_lig   = id_lig, 
    id_col   = id_col, 
    fi    = fi, 
    fj    = fj, 
    phi   = phi, 
    psi   = psi,
    chi_2 = chi_2)	 
  return(afcor.output)  
}
plot_simult <-  function (Y, hor=NA, ver=NA, font = NA) {
  # Simultaneous display of rows and columns 
  # font = 1 usual, 2 = bold, 5= greek, 8 = italic, 9 = ital bold

  if(is.na (hor)){ hor <- 1};if(is.na (ver)) {ver <- 2};
  XX = c(Y$phi[,hor], Y$psi[,hor])
  YY = c(Y$phi[,ver], Y$psi[,ver])
  
  xlab = paste("Axis ", hor, sep = "");
  ylab = paste("Axis ",  ver, sep = "");
  
  plot(XX,YY, xlab=xlab , ylab=ylab , type = "n");
    abline(h = 0,v = 0, col = "red")
    text(Y$phi[,hor],Y$phi[,ver],Y$id_lig, font = 1, col = "blue")
    text(Y$psi[,hor],Y$psi[,ver],Y$id_col, font = 2, col = "red")
}

factor_bd <- afcor(as.data.frame(terminos_carreras2))

factor_bd

plot_simult(factor_bd)

# estilo pca / tidy

library(broom)
library(ggfortify)
library(dplyr)

terminos_carreras2 <- terminos_carreras %>% 
  rownames_to_column(var = "palabra") %>% 
  rename_all(tolower) %>% 
  as_tibble()

terminos_carreras2

terminos_carreras_pca <- terminos_carreras2 %>% 
  nest() %>% 
  mutate(pca = map(data, ~ prcomp(x = terminos_carreras, center = TRUE, scale = TRUE)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y)))

terminos_carreras_pca$pca
terminos_carreras_pca$pca_aug

terminos_carreras_pca

terminos_carreras_pca2 <- terminos_carreras_pca %>% 
  unnest(pca_aug) %>%
  summarise( across(.cols = c(".fittedPC1", ".fittedPC2", ".fittedPC3", ".fittedPC4", ".fittedPC5"), var ) ) %>%
  gather(key = pc, value = variance) %>%
  mutate(var_exp = variance/sum(variance),
         cum_var_exp = cumsum(var_exp),
         pc = str_replace(pc, ".fitted", ""))

terminos_carreras_pca2

terminos_carreras_pca2 %>% 
  rename(
    `Variance Explained` = var_exp,
    `Cumulative Variance Explained` = cum_var_exp
  ) %>% 
  gather(key = key, value = value, `Variance Explained`:`Cumulative Variance Explained`) %>% 
  ggplot(aes(pc, value, group = key)) + 
  facet_wrap(~key, scales = "free_y") +
  geom_point() + 
  geom_line() +
  theme_bw() +
  lims(y = c(0, 1)) +
  labs(y = "Variance",
       title = "Variance explained by each principal component")

terminos_carreras_pca %>%
  mutate(
    pca_graph = map2(
      .x = pca,
      .y = data,
      ~ autoplot(.x, loadings = TRUE, loadings.label = TRUE,
                 loadings.label.repel = TRUE,
                 data = .y, label = TRUE,
                 label.repel = TRUE) +
        theme_minimal() +
        labs(x = "Principal Component 1",
             y = "Principal Component 2")
    )
  ) %>%
  pull(pca_graph)

rm(terminos_carreras_pca, terminos_carreras_pca2, terminos_carreras2)
rm(afcor, plot_simult)

# 6: redes ---------------------------------------------------------------

library(widyr)
library(ggraph)
library(igraph)
library(tidygraph)

terminos_bd2 <- terminos_bd %>% filter(palabra %in% evok_bd$data$palabra) # terminos en la tabla de evok
terminos_bd2$id <- as.character(terminos_bd2$id)

terminos_bd_corr <- terminos_bd2 %>% widyr::pairwise_cor(item = palabra, feature = id, sort = TRUE, method = "pearson")  
glimpse(terminos_bd_corr)

terminos_bd_graph <- terminos_bd_corr %>%
  filter(correlation > 0 ) %>%
  graph_from_data_frame( directed = FALSE ) 
terminos_bd_graph

nodos <- as.data.frame(V(terminos_bd_graph)$name) %>%
  rename(palabra=1) %>%
  left_join( terminos_bd %>% group_by(palabra) %>% 
               summarise(
                 valoracion = mean(valoracion), 
                 orden = mean(orden),
                 count = n())
  ) %>% left_join(evok_bd$data %>% select(palabra,q))
glimpse(nodos)
V(terminos_bd_graph)$valoracion <- nodos$valoracion
V(terminos_bd_graph)$orden <- nodos$orden
V(terminos_bd_graph)$count <- nodos$count
V(terminos_bd_graph)$q <- nodos$q

g <- terminos_bd_graph %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  mutate(community = as.factor(group_leading_eigen()))
  # mutate(community = as.factor(group_louvain()))
  # mutate(community = as.factor(group_infomap()))

comunidades <- g %>% activate(nodes) %>% as.data.frame() %>% 
  select(palabra=name, valoracion, community) %>% 
  group_by(community) %>% 
  summarize(val = mean(valoracion), palabras=paste(palabra, collapse = " ")) %>%
  arrange(val)

g %>% 
  ggraph(layout = "kk" ) + 
  geom_edge_link(aes(edge_alpha = correlation), color="gray", show.legend = TRUE )+
  # geom_node_point(aes(colour = community), size = 4) +
  geom_node_point(aes(size = count, color = community, shape = factor(q))) +
  geom_node_text(aes(label = name, color = community), repel = TRUE)+
  theme_graph()

g %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  mutate(comunidad = as.factor(group_leading_eigen())) %>% 
    ggraph(layout = "stress" ) + 
  geom_edge_link(width = 1, colour = "lightgray") + # falta correlation
  geom_node_point(aes(color = valoracion, shape = comunidad), size = 5, show.legend = TRUE) +
  geom_node_text(aes(label = name, color = valoracion), repel = TRUE, show.legend = FALSE) +
  scale_colour_gradient(low = "red", high = "green", na.value = NA) +
  theme_graph()

comunidades

rm(terminos_bd_corr, terminos_bd_graph, nodos, g)

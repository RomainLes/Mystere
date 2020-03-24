#library(rsconnect)
#rsconnect::setAccountInfo(name='romainlesauvage',
#                          token='365AA0EAB70E3C655733D32162A2C8FB',
#                          secret='s81AV6DDhBwIazc8AiIKnnMMpx6QmOeL0b3n2lth')
#rsconnect::deployApp("C:/Users/torna/Desktop/Esquisse/Esquisse")

####### LIBRARIES #########

library(shiny)
library(DBI)
library(RSQLite)
library(shinydashboard)
library(shinyalert)

######### VAR #########

sqlitePath <- "Mystere2.sqlite"
table <- "jeu"
deja_teste <- "LISTE DES CATEGORIES DEJA TESTEES : "

######## FONCTIONS ###########

saveData <- function(data) {
  if(data != ""){
    db <- dbConnect(SQLite(), sqlitePath)
    query <- paste("INSERT INTO JEU VALUES ('",data,"','0','-1')")
    dbGetQuery(db, query)
    dbDisconnect(db)
  }
  
}

deleteData <- function(){
  db <- dbConnect(SQLite(), sqlitePath)
  query <- sprintf("DELETE FROM JEU")
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
}


loadData <- function() {
  db <- dbConnect(SQLite(), sqlitePath)
  query <- sprintf("SELECT * FROM JEU")
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}


loadDataCateg <- function() {
  db <- dbConnect(SQLite(), sqlitePath)
  query <- sprintf("SELECT * FROM CAT_EN_JEU")
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

loadDataMot <- function() {
  db <- dbConnect(SQLite(), sqlitePath)
  query <- sprintf("SELECT * FROM MOT_EN_JEU")
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

finDuJeu <- function(){
  db <- dbConnect(SQLite(), sqlitePath)
  query <- sprintf("DELETE FROM CAT_EN_JEU")
  data <- dbGetQuery(db, query)
  query <- sprintf("DELETE FROM MOT_EN_JEU")
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
}


traiteSeed <- function(){
  db <- dbConnect(SQLite(), sqlitePath)
  query <- sprintf("SELECT * FROM SEED")
  data <- dbGetQuery(db, query)
  nombreSeeds <- nrow(data)
  seed <- data[sample(1:nombreSeeds,1),]
  set.seed(seed)
  query <- paste0("DELETE FROM SEED WHERE SEED_VAL = ",seed)
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
}

seclectionCategories <- function(){
  finDuJeu()
  traiteSeed()
  # Chargement des données
  jeu <- loadData()
  # Choix des 9 catégories 
  noms <- names(jeu[,-1])
  choix <- sample(noms,9)
  cat <- as.data.frame(choix)
  names(cat) <- "Liste des catégories en jeu"
  db <- dbConnect(SQLite(), sqlitePath)
  for(mot in choix){
    query <- paste0("INSERT INTO CAT_EN_JEU VALUES ('",mot,"')")
    data <- dbGetQuery(db, query)
  }
  # Choix de LA catégorie
  cat_en_jeu <- sample(choix,1)
  n <- nrow(jeu)
  indice <- sample(1:n,1)
  mot_en_jeu <- jeu[indice,cat_en_jeu]
  query <- paste0("INSERT INTO MOT_EN_JEU VALUES ('",cat_en_jeu,"','",mot_en_jeu,"')")
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  cat
}

afficheMot <- function(){
  data <- loadDataMot()
  cat <- data[1,"CATEG"]
  mot <- data[1,"MOT"]
  affiche <- paste0(mot," (",joli(cat),")")
  affiche
}

joli <- function(mot){
  rep = mot
  if(mot == "FORME_COULEUR"){
    rep = "FORME ou COULEUR"
  }
  if(mot == "PERSONNAGE_CELEBRE"){
    rep = "PERSONNAGE CELEBRE"
  }
  if(mot == "FILM_TV"){
    rep = "FILM ou TV"
  }
  if(mot == "CHANSON_MUSIQUE"){
    rep = "CHANSON ou MUSIQUE"
  }
  if(mot == "NOMBRE_DATE"){
    rep = "NOMBRE ou DATE"
  }
  if(mot == "PERSONNAGE_COMMUN"){
    rep = "PERSONNAGE COMMUN"
  }
  rep
}

icone_cat <- function(mot){
  rep = ""
  if(mot == "FORME_COULEUR"){ rep="palette"}
  if(mot == "FILM_TV"){ rep= "film"}
  if(mot == "OBJET"){ rep= "toolbox"}
  if(mot == "LIEU"){ rep= "location-arrow"}
  if(mot == "CHANSON_MUSIQUE"){ rep= "music"}
  if(mot == "NOMBRE_DATE"){ rep= "sort-numeric-up"}
  if(mot == "CONCEPT"){ rep= "brain"}
  if(mot == "PERSONNAGE_COMMUN"){ rep= "user"}
  if(mot == "MATIERE"){ rep= "pastafarianism"}
  if(mot == "ANIMAL"){ rep= "paw"}
  if(mot == "PERSONNAGE_CELEBRE"){ rep= "address-card"}
  if(mot == "LIVRE"){ rep= "book"}
  if(mot == "LOISIR"){ rep= "futbol"}
  if(mot == "VERBE"){ rep= "pencil-alt"}
  rep
}

estCorrect <- function(categorie){
  reponse <- loadDataMot()
  cat <- reponse$CATEG
  estOK <- categorie == cat
  estOK
}

maj_liste <- function(categorie,output){
  deja_teste <<- paste(deja_teste,joli(categorie)," - ")
  output$affiche <- renderText({deja_teste})
}

maj_liste_oui <- function(categorie,output){
  deja_teste <<- paste(deja_teste,joli(categorie),"(c'est celle là !) -")
  output$affiche <- renderText({deja_teste})
}



disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}

######### UI ############

ui <- dashboardPage(
  dashboardHeader(title = "Mystère"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Version bébé du Mystère !", tabName = "mots", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabsetPanel(id = "tabs",
                tabPanel(title = "Table de jeu",
                         fluidPage(useShinyalert(),  
                           actionButton("afficher", "Actualiser le plateau"),
                           actionButton("fin","Fin de la manche"),
                           h3(textOutput("")),
                           fluidRow(
                             column(12,
                                    actionButton("cat1","",icon=icon("chess-queen"),style='font-size:150%'),
                                    actionButton("cat2","",icon=icon("chess-queen"),style='font-size:150%'),
                                    actionButton("cat3","",icon=icon("chess-queen"),style='font-size:150%'),
                                    fluidRow(
                                      column(12,
                                             actionButton("cat4","",icon=icon("chess-queen"),style='font-size:150%'),
                                             actionButton("cat5","",icon=icon("chess-queen"),style='font-size:150%'),
                                             actionButton("cat6","",icon=icon("chess-queen"),style='font-size:150%'),
                                             fluidRow(
                                               column(12, 
                                                      actionButton("cat7","",icon=icon("chess-queen"),style='font-size:150%'),
                                                      actionButton("cat8","",icon=icon("chess-queen"),style='font-size:150%'),
                                                      actionButton("cat9","",icon=icon("chess-queen"),style='font-size:150%')
                                               )
                                             )
                                      )
                                    )
                             )
                           ),
                           h3(textOutput("affiche"))
                           
                           
                           
                           
                           
                         )
                ),
                tabPanel(title = "Sélection du mot à faire trouver",
                         infoBox(
                           "Mot à faire deviner", uiOutput("Word"), "", icon = icon("chess-queen")
                         ),
                         actionButton("mot_a_deviner","Sélectionner le mot à deviner"),
                         #tableOutput("mots"),
                         tableOutput("catégories")
                         
                )
    )
    
    
  )
)

######### SERVER ###########

server <- function(input, output, session) {
  
  #output$mots <- renderDataTable(loadData())
  observeEvent(input$mot_a_deviner,{
    output$catégories <- renderTable(seclectionCategories())
    #output$mots <- renderTable(loadDataMot())
    output$Word <- renderText({
      afficheMot()
    })
  })
  
  observeEvent(input$fin,{
    finDuJeu()
    deja_teste <<- "LISTE DES CATEGORIES DEJA TESTEES : "
    output$affiche <- renderText({deja_teste})
    output$Word <- renderText({
      ""
    })
  })
  
  observeEvent(input$afficher,{
    cat <- loadDataCateg()
    updateActionButton(session, "cat1", label = joli(cat[1,]),icon=icon(icone_cat(cat[1,])))
    updateActionButton(session, "cat2", label = joli(cat[2,]),icon=icon(icone_cat(cat[2,])))
    updateActionButton(session, "cat3", label = joli(cat[3,]),icon=icon(icone_cat(cat[3,])))
    updateActionButton(session, "cat4", label = joli(cat[4,]),icon=icon(icone_cat(cat[4,])))
    updateActionButton(session, "cat5", label = joli(cat[5,]),icon=icon(icone_cat(cat[5,])))
    updateActionButton(session, "cat6", label = joli(cat[6,]),icon=icon(icone_cat(cat[6,])))
    updateActionButton(session, "cat7", label = joli(cat[7,]),icon=icon(icone_cat(cat[7,])))
    updateActionButton(session, "cat8", label = joli(cat[8,]),icon=icon(icone_cat(cat[8,])))
    updateActionButton(session, "cat9", label = joli(cat[9,]),icon=icon(icone_cat(cat[9,])))
  })
  
  observeEvent(input$cat1,
               {
                 cat <- loadDataCateg()
                 if(!estCorrect(cat[1,])){
                   shinyalert("Oops!", "Ce n'est pas la bonne catégorie", type = "error")
                   maj_liste(cat[1,],output)
                 } else {
                   
                   shinyalert("Yess !", "C'est la bonne catégorie", type = "success")
                   maj_liste_oui(cat[1,],output)
                 }
                 })
  
  observeEvent(input$cat2,
               {
                 cat <- loadDataCateg()
                 if(!estCorrect(cat[2,])){
                   shinyalert("Oops!", "Ce n'est pas la bonne catégorie", type = "error")
                   maj_liste(cat[2,],output)
                 }else {
                   shinyalert("Yess !", "C'est la bonne catégorie", type = "success")
                   maj_liste_oui(cat[2,],output)
                 }
               })
  observeEvent(input$cat3,
               {
                 cat <- loadDataCateg()
                 if(!estCorrect(cat[3,])){
                   shinyalert("Oops!", "Ce n'est pas la bonne catégorie", type = "error")
                   maj_liste(cat[3,],output)
                 }else {
                   shinyalert("Yess !", "C'est la bonne catégorie", type = "success")
                   maj_liste_oui(cat[3,],output)
                 }
               })
  observeEvent(input$cat4,
               {
                 cat <- loadDataCateg()
                 if(!estCorrect(cat[4,])){
                   shinyalert("Oops!", "Ce n'est pas la bonne catégorie", type = "error")
                   maj_liste(cat[4,],output)
                 }else {
                   shinyalert("Yess !", "C'est la bonne catégorie", type = "success")
                   maj_liste_oui(cat[4,],output)
                 }
               })
  observeEvent(input$cat5,
               {
                 cat <- loadDataCateg()
                 if(!estCorrect(cat[5,])){
                   shinyalert("Oops!", "Ce n'est pas la bonne catégorie", type = "error")
                   maj_liste(cat[5,],output)
                 }else {
                   shinyalert("Yess !", "C'est la bonne catégorie", type = "success")
                   maj_liste_oui(cat[5,],output)
                 }
               })
  observeEvent(input$cat6,
               {
                 cat <- loadDataCateg()
                 if(!estCorrect(cat[6,])){
                   shinyalert("Oops!", "Ce n'est pas la bonne catégorie", type = "error")
                   maj_liste(cat[6,],output)
                 }else {
                   shinyalert("Yess !", "C'est la bonne catégorie", type = "success")
                   maj_liste_oui(cat[6,],output)
                 }
               })
  observeEvent(input$cat7,
               {
                 cat <- loadDataCateg()
                 if(!estCorrect(cat[7,])){
                   shinyalert("Oops!", "Ce n'est pas la bonne catégorie", type = "error")
                   maj_liste(cat[7,],output)
                 }else {
                   shinyalert("Yess !", "C'est la bonne catégorie", type = "success")
                   maj_liste_oui(cat[7,],output)
                 }
               })
  observeEvent(input$cat8,
               {
                 cat <- loadDataCateg()
                 if(!estCorrect(cat[8,])){
                   shinyalert("Oops!", "Ce n'est pas la bonne catégorie", type = "error")
                   maj_liste(cat[8,],output)
                 }else {
                   shinyalert("Yess !", "C'est la bonne catégorie", type = "success")
                   maj_liste_oui(cat[8,],output)
                 }
               })
  observeEvent(input$cat9,
               {
                 cat <- loadDataCateg()
                 if(!estCorrect(cat[9,])){
                   shinyalert("Oops!", "Ce n'est pas la bonne catégorie", type = "error")
                   maj_liste(cat[9,],output)
                 }else {
                   shinyalert("Yess !", "C'est la bonne catégorie", type = "success")
                   maj_liste_oui(cat[9,],output)
                 }
               })
  
  
  
  output$affiche <- renderText({deja_teste})
  
  
}




###### SHINYAPP ########

shinyApp(ui, server)


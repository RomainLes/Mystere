library(RSQLite)
drv <- SQLite()
db <- dbConnect(drv, dbname = "Mystere2.sqlite")
#db <- dbConnect(SQLite(), "Mystere.sqlite")

setwd("C:/Users/torna/Desktop/Mystere/")
jeu <- read.csv("cartes.csv",sep=";")


dbSendQuery(conn = db, "CREATE TABLE JEU(
ID INT PRIMARY KEY,
            FORME_COULEUR TEXT NOT NULL,
FILM_TV TEXT NOT NULL,
OBJET TEXT NOT NULL,
LIEU TEXT NOT NULL,
CHANSON_MUSIQUE TEXT NOT NULL,
NOMBRE_DATE TEXT NOT NULL,
CONCEPT TEXT NOT NULL,
PERSONNAGE_COMMUN TEXT NOT NULL,
MATIERE TEXT NOT NULL,
ANIMAL TEXT NOT NULL,
PERSONNAGE_CELEBRE TEXT NOT NULL,
LIVRE TEXT NOT NULL,
LOISIR TEXT NOT NULL,
VERBE TEXT NOT NULL
            )"
)

dbSendQuery(conn = db, "CREATE TABLE CAT_EN_JEU(
            NOM TEXT NOT NULL
)"
)

dbSendQuery(conn = db, "CREATE TABLE SEED(
            SEED_VAL INT
)"
)

dbSendQuery(conn = db, "CREATE TABLE MOT_EN_JEU(
            CATEG TEXT NOT NULL,
MOT TEXT NOT NULL
)"
)

dbWriteTable(conn = db, name = "JEU", value = jeu, row.names = FALSE, 
             append = TRUE)

data <- data.frame("SEED_VAL" = rep(1:10000))
dbWriteTable(conn = db, name = "SEED", value = data, row.names = FALSE, 
             append = TRUE)


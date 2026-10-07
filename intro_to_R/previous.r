#


#Data Frames

?data.frame
(df <- data.frame(A=c(1:10), B=11:20, C=c('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 'a10'), stringsAsFactors = FALSE))

str(df)
View(df)
df

head(df) # felső 6 eset
tail(df) # utolsó 6 eset

df <- data.frame(A=c(1:10), B=11:20, C=c('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 'a10'), stringsAsFactors = TRUE)
str(df) # Mi a különbség?


#
if (!requireNamespace("foreign", quietly = FALSE)) {install.packages("foreign") }
library(foreign)

Dataset <-  read.spss("ESS6_HUN_autotranslate.sav", 
						rownames=FALSE, 
						stringsAsFactors=TRUE, 
						tolower=FALSE, 
						to.data.frame = TRUE)

str(Dataset)
View(Dataset)

#install.packages("foreign")
library(foreign)
Dataset <-  read.spss("ESS7.sav", 
						rownames=FALSE, 
						stringsAsFactors=TRUE, 
						tolower=FALSE, 
						to.data.frame = TRUE)
						
View(Dataset)
str(Dataset)
str(Dataset, list.len=9999)

head(Dataset, 3) # elejét (3 sor, összes változó)
Dataset[1:6, 1:9] # vektor cimzés tartományra

Dataset$tvpol         # címzés névvel
table(Dataset$tvpol)

Dataset[,'tvpol']
str(Dataset$tvpol) # MÁR VEKTOR!

Dataset[1:10,'tvpol']

Dataset[,'tvpol'][1:10]

#Idáig jututtunk (2026.10.01.)

colnames(Dataset)
rownames(Dataset)

Dataset['10','tvpol']


#kiszedjük a férfiakat
Dataset$gndr == 'Male'
# Hány férfi van? (Hányszor "TRUE" a logikai? - azaz 1)
sum(Dataset$gndr == 'Male')
table(Dataset$gndr)

# csak azon tv nézés kell, amit férfiak mondtak:
Dataset[Dataset$gndr == 'Male','tvpol']
length(Dataset[Dataset$gndr == 'Male','tvpol'])

table(Dataset[Dataset$gndr == 'Male','tvpol'])
# melyik elemre igaz (indexet ad vissza)
which(Dataset$gndr == 'Male')

Dataset[which(Dataset$gndr == 'Male'),'tvpol']


#Listák
LISTA <- list(Dataframe=Dataset, szamok=a, gyumolcsok=gyumik)
str(LISTA)
#indexelés:
LISTA$gyumolcsok
LISTA[[3]]  # ilyenkor mindig dupla kapcsos (listánál) 

LISTA[3] # listát ad vissza!!!
# listán belül a Dataset első 10 eleme a Tvpol-nak:
LISTA$Dataframe[1:10,'tvpol']

table(LISTA$Dataframe$gndr)


LISTA[['krumpli']] <- LISTA 


LISTA$krumpli$gyumolcsok[2]
LISTA[[4]][[3]][2]

Lista2 <- list(szamok=c(1:10), betuk=LETTERS, tvpol=Dataset$tvpol[1:10])

# Kérjük le a listán belül krumpliból a data.frame-ből az idno első elemét!

#Változónév használati szabályok: számok, különleges karakterek (.!)
#értékadás speciális esetei: <<-, ->
#változók: 
ls()

# rejtett változók
print(ls(all.name = TRUE))
?.Random.seed

tomb <- 6

#változó törlése
rm(tomb)
print(tomb)
# FoNtos: memóriából való törlés: felül kell írni, mert megtartja a foglalást a blokkra!
tomb <- NULL
rm(tomb)

# minden törlése:
rm(list = ls())
print(ls())


#alapszintű műveletek:
# Aritmetikai
#+	
#–	
#*	
#/	
#^	
2^2 # hatványozás
#%%	Modulus - maradék
10%%8
#%/%	egész rész
10%/%8


?round # KEREKÍTÉS (0.5)
floor(10/8) #LEFELÉ KEREKÍT
ceiling(10/8) # FELFELÉ KEREKÍT
#[1] 1
trunc(10/8) # LEVÁG
#[1] 1

 10%%8/8
 
10/8-trunc(10/8) # CSAK TIZEDES RÉSZ

x <- c(2,8,3)
y <- c(6,4,1)
x+y

x <- c(2,8,3,2)
y <- c(6,4,1)
x+y

x <- c(2,8,3,2,4,5)
y <- c(6,4,1)
x+y

# relációs
#<	
#>	
#<=	
#>=	
#==	egyenlő?
#!=	nem egyenlő
x>y

# logikai
#!	Logikai NEM (tagadás)
#&	ÉS (minden elemre)
###&&	Logikai ÉS
#|	VAGY (minden elemre)
###||	Logikai vagy
0 0 0 0  == 0
0 0 0 1  == 1
0 0 1 0  == 2
0 0 1 1  == 3
1 1 1 1  == 15

Pisti Jenő
   0 | 0 => FALSE
   0 | 1 => TRUE
   1 | 0 => TRUE
   1 | 1 => TRUE

 0 & 0 => FALSE
 0 & 1 => FALSE
 1 & 0 => FALSE
 1 & 1 => TRUE

x <- c(TRUE,FALSE,0,1,1)
y <- c(FALSE,TRUE,FALSE,TRUE,0)

as.logical(x)
#[1]  TRUE FALSE FALSE  TRUE  TRUE
!x
#[1]  FALSE TRUE TRUE FALSE FALSE

as.logical(x)
#[1]  TRUE FALSE FALSE  TRUE  TRUE
as.logical(y)
#[1] FALSE  TRUE FALSE  TRUE FALSE
x&y
#FALSE FALSE FALSE TRUE FALSE

which(x&y)
#[1] 4

x&&y

#[1]  TRUE FALSE FALSE  TRUE  TRUE
#[1] FALSE  TRUE FALSE  TRUE FALSE
x|y
# TRUE TRUE FALSE TRUE TRUE 
#[1]  TRUE  TRUE FALSE  TRUE  TRUE
x||y

all # mindegyik elemre igaz az érték
any # bármelyik elemre igaz-e az érték

all(x|y)
any(x&y)

x&y[4]


summary(Dataset$tvpol)


any(is.na(Dataset$tvpol)) # van-e olyan ember aki nem válaszolt?
#TRUE
which(is.na(Dataset$tvpol)) # Kik nem válaszoltak?

anyNA(Dataset$tvpol) # bármelyik NA-e egy kéréssel == any(is.na))

any(Dataset$gndr == 'Male') # Van-e férfi az adatbázisban

sum(Dataset$gndr == 'Male', na.rm = TRUE)  # Hány férfi van? 
#[1] 903
table(Dataset$gndr)

length(Dataset$gndr == 'Male') # nem jó:(


which(Dataset$gndr != 'Female')
length(which(Dataset$gndr != 'Female'))

length(which(Dataset$gndr == 'Male'))

hossz <- which(Dataset$gndr == 'Male')
hossz
length(hossz)
#[1] 903
length(Dataset)
#[1] 322

dim(Dataset)
#[1] 2015  322

dd <- c(3,4,2,NA)

# egyéb halmaz és lefoglalt operátorok
# : 
# %in% részhalmaz
#%*% mátrix szorzás
#t transzponálás

a <- 1:20
2 %in% a

c(1,2,42) %in% a
a %in% c(1,2,42)

c1 <- 200:1000
c2 <- 800:2000
reszhalmaz <- c1 %in% c2
as.numeric(reszhalmaz)
c1[which(reszhalmaz)] # azon c1 értékek amelyekre igaz hogy c2-ben is megvannak.
c1[reszhalmaz]
c1[c1 %in% c2] # ugyanaz egyszerre - logikaival is tudok indexelni



(M1 <- matrix(1:4, 2, 2))
(M2 <- matrix(2:5, 2, 2))
M1*M2

M1%*%M2
#vs
M1*M2


M3 <- M1*1:4
M3
t(M3) 

#Alapvető műveletek:
a <- 1:20
sum(a)
mean(a)
#mean
#?mean
sd(a) # standard deviation
var(a) # variance
sqrt(var(a)) == sd(a)
var(a)^0.5 # hatványozás
var(a)**0.5 # hatványozás

## idáig: március 3.

mean(a)
mean(a, na.rm=TRUE)
mean(1:10)
?mean
help(mean)
help.search("t-test")

# string kezelés
paste('alma', 'korte', 'paradicsom', sep=" - ") # összefűzés
paste0('alma', 'korte')
paste(c(LETTERS), collapse=", ") # collapse egy vektor osszes elemét összemásolja

betuk <- c(paste(c(LETTERS), collapse=", "), paste(c(letters), collapse=", "))
paste(betuk, "akármi", sep=" - ")

paste(betuk, collapse=", ")

szavak <- c('alma', 'korte', 'narancs')
str(szavak)
szavak

paste(szavak, as.character(1:3))
str(paste(szavak, as.character(1:3)))

paste(szavak, collapse=", ")
str(paste(szavak, collapse=", "))


strsplit(betuk, ", ", fixed=TRUE)

substr("Almaleves", 1, 4) # kezdő és vég poziciót kell megadni
substr("Almaleves", 5, 9)
substr("Almaleves", 5, nchar("Almaleves")) # nchar: karakterek száma
substr("Almaleves", 5, 100)

# reguláris kifejezések: nagyon gyorsak
gsub('a', 'e', "almaleves") # karakter kicserélése / törlése
gsub('a', '', "almaleves") # karakter kicserélése / törlése

regexpr("l", "alma alma") # hol van az adott betű (mindig az első)
gregexpr("l", "alma alma") # minden helyet

# feltételes elágazás
# if - else , ifelse, (switch -> fgv-ként)

if(állítás) { igaz állítás esetén hajtsa végre } else if(állítás2) { csak akkor ha nem igaz az első }

#else nem kötelező 

ifelse(20<10, a<- 20, b <- 10) # állítás, igaz esetben, nem igaz esetben

################################################################################
##################################### ciklusok #################################
################################################################################

#repeat -> #break - megszakít

szamlalo <- 1
repeat {
   print(szamlalo)
   szamlalo <- szamlalo+1 
   if(szamlalo > 5) {
      break
   }
}
szamlalo


# while

szamlalo <- 1
while (szamlalo < 7) {
   print(szamlalo)
   szamlalo <- szamlalo + 1
}

#for ciklus
for (szamlalo in 1:6) { 
	print(szamlalo)
}

#next  - ugrat

for (i in LETTERS[1:6]) {
   if (i == "D") {
      next
   }
   print(i)
}

for (i in LETTERS[1:6]) {
   if (i == "D") {
      break
   }
   print(i)
}

#Feladatok:
#1. Hozz létre egy ciklust, amely a korábban LISTAKÉNT betöltött ESS7 adatfájlból a változónevekből kiválogatja, amelyekben van "a" betű. Majd ezeket a változókat kiválogatva létrehoz egy új Data.frame objektumot, amelyben csak az első 500 eset szerepel
#2. Hozz létre egy ciklust, amely egy 10x10-es, 1-100ig értéket tartalmazó mátrixon minden nem szélen lévő cellára kiszámítja a környező cellák átlagát és betölti azt egy 8x8-as mátrixba




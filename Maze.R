### AvailableUsername404@GitHUb

### Skrypt do przechodzenia labirytnów

rm(list = ls()) # funkcja pomocnicza - czyszczenie wszsytkich danych (wartości i funkcji)

### Wczytywanie mapy z pliku

readMap <- function(path) {
  
  lab_raw <- read.delim(file = path, header = FALSE) # Wczytywanie przez podanie adresu
  
  y_len <- nrow(lab_raw)
  
  x_len <- nchar(lab_raw[1,1])
  
  lab <- matrix(0, nrow = y_len, ncol = x_len)
  
  for (i in 1:y_len) {
    for (j in 1:x_len) {
      lab_row <- unlist(strsplit(lab_raw[i,1], split =""))
      lab[i,j] = lab_row[j]
    }
  }
  return (lab) # zwrócenie macierzy odwzorowującej labirynt
}

### Znajdowanie ścieżki - algorytm inspirowany algorytmem Dijkstry

# INDEKSOWANIE MACIERZY M[Y, X] !!!

findWay <- function (m) { # główna funkcja znajdowania ścieżki
  
  # poniższe funkcje kierunków zapobiegają wyjściu poza labirynt
  
  north <- function (y) { # czy można zrobić krok na północ
    if ((y-1) > 0) {
      return (T)
    } else {
      return (F)
    }
  }
  
  south <- function (y) { # czy można zrobić krok na południe
    if ((y+1) <= y_len) {
      return (T)
    } else {
      return (F)
    }
  }
  
  east <- function (x) { # czy można zrobić krok na wschód
    if ((x+1) <= x_len) {
      return (T)
    } else {
      return (F)
    }
  }
  
  west <- function (x) { # czy można zrobić krok na zachód
    if ((x-1) > 0) {
      return (T)
    } else {
      return (F)
    }
  }
  
  typ_pola <- function (y, x) { # funkcja reagująca na typ pola
    typ <- m[y, x]
    
    if (typ == "e") { # jeśli pole "e" to dodaj je do ściezki i przerwij pętlę
      a <<- rbind(a, c(y,x,(a[numer,]$Value+1)))
      break_flag <<- TRUE
    } else if (typ == " ") { # jeśli puste pole to sprawdź czy było już odwiedzone i jeśli tak to nic nie rób, jeśli nie to dodaj do ścieżki
      if (identical(rownames(a[(a$x == x) & (a$y == y),]), character(0))) {
        a <<- rbind(a, c(y,x,(a[numer,]$Value+1)))
      }
    }
  }
  # start funkcji
  
  y_len <- nrow(m)
  
  x_len <- ncol(m)
  
  start <- c(as.double(which(m == "s", arr.ind = TRUE)),1) # znajdowanie współrzędnych punktu startowego labiryntu
  
  a <- as.data.frame(matrix(start, ncol = 3)) # inicjowanie ścieżki z punktem startowym
  colnames(a) <- c("y", "x", "Value") # nadanie etykiet kolumn
  
  break_flag <- FALSE # ustawienie flagi przerywania pętli
  
  numer <- 1 # iterator do a
  
  # szukanie punktu "e"
  
  while (T) {
    x <- a[numer,]$x # współrzędna x aktualnie sprawdzanego pola
    y <- a[numer,]$y # współrzędna y aktualnie sprawdzanego pola
    value <- a[numer,]$Value # wartość odległości aktualnie sprawdzanego pola
    
    # sprawdzić gdzie się można ruszyć i jeśli można to sprawdzić typ pola
    
    if (north(y)) {
      typ_pola(y-1, x)
    }
    if (south(y)) {
      typ_pola(y+1, x)
    }
    if (east(x)) {
      typ_pola(y, x+1)
    }
    if (west(x)) {
      typ_pola(y, x-1)
    }
    if (break_flag) {
      numer <- numer + 1
      break
    }
    numer <- numer + 1
  }
  
  # budowanie ścieżki wyjścia
  
  end <- c(as.double(a[nrow(a),])) # znajdowanie współrzędnych końcowego startowego labiryntu
  
  b <- as.data.frame(matrix(end, ncol = 3)) # inicjowanie ścieżki z punktem końcowym
  colnames(b) <- c("y", "x", "Value") # nadanie etykiet kolumn
  #colnames(b) <- c("y", "x", "Value") # nadanie etykiet kolumn - do testowania
  
  while (as.double(b[nrow(b),3]) != 1) {
    x <- b[nrow(b),]$x # współrzędna x aktualnie sprawdzanego pola
    y <- b[nrow(b),]$y # współrzędna y aktualnie sprawdzanego pola
    value <- b[nrow(b),]$Value # wartość odległości aktualnie sprawdzanego pola
    
    # czy jest sąsiednia ścieżka north
    if (isFALSE(identical(rownames(a[(a$x == x) & (a$y == (y-1)),]), character(0)))) { #czy istnieje element o współrzędnych x i y-1 w a
      current_pos <- rownames(a[(a$x == x) & (a$y == (y-1)),])
      
      if ((value - 1) == (as.double(a[current_pos,3]))) {
        b <- rbind(b, c(as.double(a[current_pos,])))
        next
      }
    }
    
    # czy jest sąsiednia ścieżka south
    if (isFALSE(identical(rownames(a[(a$x == x) & (a$y == (y+1)),]), character(0)))) { #czy istnieje element o współrzędnych x i y+1 w a
      current_pos <- rownames(a[(a$x == x) & (a$y == (y+1)),])
      
      if ((value - 1) == (as.double(a[current_pos,3]))) {
        b <- rbind(b, c(as.double(a[current_pos,])))
        next
      }
    }
    
    # czy jest sąsiednia ścieżka east
    if (isFALSE(identical(rownames(a[(a$x == (x+1)) & (a$y == y),]), character(0)))) { #czy istnieje element o współrzędnych x+1 i y w a
      current_pos <- rownames(a[(a$x == (x+1)) & (a$y == y),])
      
      if ((value - 1) == (as.double(a[current_pos,3]))) {
        b <- rbind(b, c(as.double(a[current_pos,])))
        next
      }
    }
    
    # czy jest sąsiednia ścieżka west
    if (isFALSE(identical(rownames(a[(a$x == (x-1)) & (a$y == y),]), character(0)))) { #czy istnieje element o współrzędnych x-1 i y w a
      current_pos <- rownames(a[(a$x == (x-1)) & (a$y == y),])
      
      if ((value - 1) == (as.double(a[current_pos,3]))) {
        b <- rbind(b, c(as.double(a[current_pos,])))
        next
      }
    }
  }
  m_new <- matrix(0, nrow = y_len, ncol = x_len)
  for (i in 1:y_len) {
    for (j in 1:x_len) {
      if (m[i,j] == "x") {
        m_new[i,j] <- 0
      } else if (m[i,j] == " ") {
        m_new[i,j] <- 1
      } else if (m[i,j] == "s") {
        m_new[i,j] <- 2
      } else if (m[i,j] == "e") {
        m_new[i,j] <- 3
      }
    }
  }
  for (i in 2:(nrow(b)-1)) {
    m_new[b[i,]$y,b[i,]$x] <- 4
  }
  return(m_new) # zwrócenie labiryntu z naniesioną ścieżką
}

### Wizualizacja labiryntu

plotMap <- function (m) {
  
  y_len <- nrow(m)
  
  x_len <- ncol(m)
  
  m_new <- matrix(0, nrow = y_len, ncol = x_len)
  
  for (i in 1:y_len) {
    for (j in 1:x_len) {
      if (m[i,j] == "x") {
        m_new[i,j] <- 0
      } else if (m[i,j] == " ") {
        m_new[i,j] <- 1
      } else if (m[i,j] == "s") {
        m_new[i,j] <- 2
      } else if (m[i,j] == "e") {
        m_new[i,j] <- 3
      }
    }
  }
  kolory <- c("0" = "black", "1" = "white", "2" = "green", "3" = "blue") # ustawienie kolorów
  image(x = 0:x_len, y = 0:y_len, z = t(apply(m_new, 2, rev)), col = kolory, xlab = "", ylab = "") # wyplotowanie image()
  grid(nx = x_len, ny = y_len, lty = "solid") # dodanie siatki
  box(col = "black") # dodanie ramki
}

### plotpath() - wizualizacja ścieżki

plotPath <- function(m) {
  
  y_len <- nrow(m)
  
  x_len <- ncol(m)
  
  kolory <- c("0" = "black", "1" = "white", "2" = "green", "3" = "blue", "4" = "red") # ustawienie kolorów
  image(x = 0:x_len, y = 0:y_len, z = t(apply(m, 2, rev)), col = kolory, xlab = "", ylab = "") # wyplotowanie image()
  grid(nx = x_len, ny = y_len, lty = "solid") # dodanie siatki
  box(col = "black") # dodanie ramki
}

# Przykład 1

m <- readmap(path = "./maze_0.map") # wywołanie funkcji wczytującej maze_0
# UWAGA! W środowisku RStudio podać ścieżkę BEZWZGLĘDNĄ lub zmienić katalog roboczy na "To source file location"
a <- findway(m = m) # wywołane funkcji szukania ścieżki
plotmap(m = m) # wywołanie funkcji plotowania labiryntu
plotpath(m = a) # wywołanie funkcji plotowania labiryntu ze ścieżką

# Przykład 2

m <- readmap(path = "./maze_1.map") # wywołanie funkcji wczytującej maze_1
# UWAGA! W środowisku RStudio podać ścieżkę BEZWZGLĘDNĄ lub zmienić katalog roboczy na "To source file location"
a <- findway(m = m) # wywołane funkcji szukania ścieżki
plotmap(m = m) # wywołanie funkcji plotowania labiryntu
plotpath(m = a) # wywołanie funkcji plotowania labiryntu ze ścieżką

# Część do uruchamiania

m <- readmap(path = "")
# UWAGA! W środowisku RStudio podać ścieżkę BEZWZGLĘDNĄ lub zmienić katalog roboczy na "To source file location"

a <- findway(m = m) # wywołane funkcji szukania ścieżki
plotmap(m = m) # wywołanie funkcji plotowania labiryntu
plotpath(m = a) # wywołanie funkcji plotowania labiryntu ze ścieżką
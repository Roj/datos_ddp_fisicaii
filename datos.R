datos <- read.table("../Desktop/Datos TP2 - Sheet1.tsv",stringsAsFactors=F,sep="\t",quote="",na.strings = "")
datos[is.na(datos)] <- 0
datos <- data.frame(datos)
datos <- data.matrix(datos)

typeof(datos)
campoEx <- matrix(c(0),ncol=28,nrow=20)
for(i in seq(1,20)) { #fila
  for(j in seq(1,28)) { #columna
    if(all(datos[i,j] == 0.00) || (j > 1 && j<28 && all(datos[i,j-1] == 0) && all(datos[i,j+1] == 0) )) {
      aproximacionCentrada <- 0.00
    } else if(j==1 || all(datos[i,j-1] == 0.00)) { #extremo izq
      aproximacionCentrada <- (-datos[i,j+2] + 4 * datos[i,j+1] - 3 * datos[i,j]) / 2
    } else if (j == 28 || all(datos[i,j+1] == 0.00)) { #extremo der
      aproximacionCentrada <- (+datos[i,j-2] - 4 * datos[i,j-1] + 3 * datos[i,j]) / 2
    } else { #punto tranqui
      aproximacionCentrada <- (datos[i,j+1] - datos[i,j-1])/2 # deltaV/deltaX
    }
    if(length(aproximacionCentrada) == 0) {
      aproximacionCentrada <- 0
    }
    campoEx[i,j] <- aproximacionCentrada
  }
}
campoEx
datos
campoEy <- matrix(c(0),ncol=28,nrow=20)
for(i in seq(1,20)) { #fila
  for(j in seq(1,28)) { #columna
    if(all(datos[i,j] == 0.00)) {
      aproximacionCentrada <- 0.00
    } else if(i==1 || all(datos[i-1,j] == 0.00)) { #extremo sup
      aproximacionCentrada <- (-datos[i+2,j] + 4 * datos[i+1,j] - 3 * datos[i,j]) / 2
    } else if (i == 20 || all(datos[i+1,j] == 0.00)) { #extremo inf
      aproximacionCentrada <- ( datos[i-2,j] - 4 * datos[i-1,j] + 3 * datos[i,j]) / 2
    } else { #punto tranqui
      aproximacionCentrada <- (datos[i+1,j] - datos[i-1,j])/2 # deltaV/deltaX
    }
    campoEy[i,j] <- aproximacionCentrada
  }
}

campoE <- matrix(c(0),ncol=28,nrow=20)
for(i in seq(1,20)) {
  for(j in seq(1,28)) {
    campoE[i,j] <- sqrt(campoEx[i,j][[1]]^2 + campoEy[i,j][[1]]^2)
  }
}
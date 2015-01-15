thurstone_dm <- function (matrix){
#Toma una matriz o dataframe de las frecuencias relativas o absolutas
#Devuelve los valores de escala de Thurstone y la discrepancia media 
        
    matrix = data.matrix(matrix) #datos brutos
    n = matrix[1][1]*2 #si las frecuencias dadas son absolutas, el número de participantes es
                       #la primera celda multipilcada por dos
    observed_ps <- matrix/n  #frecuencias relativas observadas

    matrix_z <- apply(observed_ps, c(1,2), qnorm) #puntuaciones z correspondientes a las frecuencias
    
    if (sum(is.na(observed_ps)) > 0){incomplete = TRUE} 
    #comprobar si no hay datos perdidos
    else{incomplete = FALSE}
    
    if (incomplete == FALSE){   
        means = apply(matrix_z, 2, mean, na.rm = TRUE)
        centered_means = means-min(means)
        v_escala = centered_means
    
    if (any(observed_ps<0.01| (any(observed_ps>0.99)))) 
        #Avisar de valores menores que 0.01 o mayores que 0.99
        warning("Hay proporciones fuera del intervalo (0.01, 0.99). Considerar usar procedimiento para datos incompletos")
    }
   
    else{
        #Avisar de valores perdidos (NAs)
        warning("Hay valores perdidos (NA). Utilizar procedimiento para datos incompletos")
        break 
        }
        
        z_distances = data.frame() #dataframe vacío
    
        for (i in 1:length(v_escala)){
            # Puntuaciones z'ij en la distribución normal correspondiente
            #a las distancias entre pares de valores de la escala obtenida
            # para los ítems
                row = v_escala-v_escala[i]
                z_distances = rbind(z_distances, row) 
               
        }
    
        theoretical_ps =data.frame(apply(z_distances, c(1,2), pnorm)) #Frecuencias teóricas
        dif <- observed_ps - theoretical_ps #diferencia entre frecuencias observadas y teóricas
    
        sum_dif_all = c() # vector vacío
    
        for (i in 1:nrow(dif)){
            #suma de las diferencias (por debajo de la diagonal)
            
            sum_dif = sum(abs(dif[i][(i+1):nrow(dif),]))
            sum_dif_all = c(sum_dif_all, sum_dif)
    
        }
        
        dm = sum(sum_dif_all, na.rm = TRUE)/(nrow(dif)*(nrow(dif)-1)/2) #Discrepancia media
        return (list(val_escala = v_escala, DM = dm)) #devolver los valores de escala y la DM
    }

thurstone_incompletos = function(matrix, umbral1 = 0, umbral2 = 1){
    #Toma una matriz o dataframe de las frecuencias relativas o absolutas
    #Devuelve los valores de escala de Thurstone utilizando el procedimiento para datos 
    #incompletos
    #Se considerarán valores perdidos aquellos que estén fuera del intervalo (umbral1, umbral2)
    #Si no se especifican valores para umbral1 y umbral2, se establecerán automáticamente como 
    #(0.01, 0.99) si hay 200 o más comparaciones o (0.02, 0.98) si hay menos de 200 comparaciones
    
    matrix = data.matrix(matrix) #datos brutos
    n = matrix[1][1]*2 #número de participantes
    juicios = (nrow(matrix)*(nrow(matrix)-1)/2)
    observed_ps <- matrix/n  #frecuencias relativas observadas
    
    if ((umbral1 == 0)&(umbral2 == 1)){ 
        #establece umbrales automáticamente si no se han especificado
        if (juicios >= 200){umbral1 = 0.01; umbral2 = 0.99}      
        else {umbral1 = 0.02; umbral2 = 0.98} 
    
    }
    
    for (i in 1:length(observed_ps)){ 
        #Converitr en NAs los valores fuera del intervalo establecido 
        if (is.na(observed_ps[i]) | observed_ps[i]<umbral1 | observed_ps[i]>umbral2){
            observed_ps[i] = NA 
        }
    }
    
    matrix_z <- apply(observed_ps, c(1,2), qnorm) 
    #puntuaciones z correspondientes a las frecuencias
   
    matrix3 = c() #vector vacío

    for (col in 2:ncol(matrix_z)){ 
        # crear matriz de diferencias entre columnas de matrix_z
        column = matrix_z[,col] - matrix_z[,(col-1)] 
        matrix3 = cbind(matrix3, column)
    }
    
    medias = apply(matrix3, 2, mean, na.rm = TRUE) #medias de las columnas
    medias = append(medias, 0, 0) #añadir 0 al principio
    
    v_escala = 0 #valor de escala del primer elemento. Establecer 0 como origen.
    
    for (i in 2:length(medias)){ 
        #obtener valores de escala uno por uno, sumando a cada media el valor de escala anterior
        s = medias[i]+v_escala[(i-1)]
        v_escala = cbind(v_escala, s)
    }
    colnames(v_escala) = colnames(matrix) #cambiar nombres de las columnas
    rownames(v_escala) = "Valores" #cambiar nombre de fila
    
v_escala = v_escala -min(v_escala) #Poner el mínimo en 0.
return (val_escala = v_escala)

}

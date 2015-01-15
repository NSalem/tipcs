v_escala = function(data){
    
    total = data.frame() # dataframe vacio
    
    for (i in (1:ncol(data))){ #proceso para cada ítem
        
        table(data[,i]) #frecuencia absoluta
        
        fr = table(data[,i])/nrow(data) #frecuencia relativa    
        
        cfr = cumsum(fr) #relativas acumuladas
        
        fr_2 = fr/2 #frecuencias relativas entre 2
        
        #half_cfr = cumsum(fr_2) #elativas acumuladas entre 2
        
        fr_2_cfr = fr/2+(c(0,cfr[c(-length(cfr))])) # relativas entre 2 + acumuladas "desplazadas"
        
        z1 = qnorm(fr_2_cfr) #z de lo anterior
        
        min_z1 = min(subset(z1, z1!=-Inf)) # mínimo de z1 evitando infinitos por hacer qnorm de 0
        
        table = z1 #valores de escala antes de poner 0 como mínimo.
        
        
        if (min_z1<0){
            table = z1 - rep(min_z1, length(z1)) #poner el mínimo en 0.
        }
        
        for (i in 1:length(table)){
            if ((table[i] == Inf) | (table[i] == -Inf)) {table[i] = NA}
        }
        
        table = round(table) #redondear
        
        if (length(table)<4){table = append(table, NA)} #añadir NAs para valores desconocidos 
       
        total = rbind (total, table) 
    }
    names(total) = c(1:ncol(total)) #poner nombres de números a los valores
    
    rownames(total) = colnames(data) # poner nombre a los ítems
    
    return (total) # devolver lista de valores
}

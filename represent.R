### =========== REPRESENTACION Y GRAFICAS ===========


## REPRESENTACION EN ESTRELLA
# @params: confidence, influence, relevance, user
representUserPuntuation <- function(confidence, influence, relevance, user){
    
    # Crear Dataset
    data <- as.data.frame(matrix( c(confidence,influence,relevance) , ncol=3))
    colnames(data) <- c("Confianza", "Influencia", "Relevancia")
    
    # Añade los max y min al dataset para pintar la grafica
    data <- rbind(rep(10,3) , rep(0,3) , data)
    print(data)
    
    # Custom radar
    radarchart(data, axistype = 1, title = user, seg = 4,
               
               #custom polygon
               pcol = rgb(0.2,0.5,0.7,0.9), pfcol = rgb(0.2,0.5,0.5,0.3), plwd = 3, 
               
               #custom the grid
               cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0,10,2.5), cglwd = 1,
               
               #custom labels
               vlcex = 0.8
    )
}


## REPRESENTACION CIRCULAR
# @params: confidence, influence, relevance, user
representUserPuntuation2 <- function(confidence, influence, relevance, user){
    
    # make data
    data=data.frame(group=c("C ","I ","R ") , value=c(10*confidence,10*influence,10*relevance) )
    
    # Usual bar plot :
    ggplot(data, aes(x = group, y = value ,fill = group )) + 
        geom_bar(width = 0.85, stat="identity")
    
    # Circular one
    ggplot(data, aes(x = group, y = value ,fill = group)) + 
        geom_bar(width = 0.85, stat="identity") +    
        
        # To use a polar plot and not a basic barplot
        coord_polar(theta = "y") +    
        
        #Remove useless labels of axis
        xlab("") + ylab("") +
        
        #Increase ylim to avoid having a complete circle
        ylim(c(0,100)) + 
        
        #Add group labels close to the bars :
        geom_text(data = data, hjust = 1, size = 3, aes(x = group, y = 0, label = group)) +
        
        #Remove useless legend, y axis ticks and y axis text
        theme(legend.position = "none" , axis.text.y = element_blank() , axis.ticks = element_blank())
    
   
}
#representUserPuntuation2(confidence = stored_users_points_1$fact_confianza[2],influence = stored_users_points_1$fact_capacidad_influencia[2],relevance = stored_users_points_1$fact_relevancia_campana[2],user = stored_users_points_1$user_screen_nameP[2])



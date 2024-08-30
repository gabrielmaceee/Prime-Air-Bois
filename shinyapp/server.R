# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




# Define server logic
shinyServer(function(input, output, session) {
  
  output$logo_inrae <- renderImage({
    list( src = "www/images/Logo_inrae.PNG",
          width = "120px",
          height = "auto")
  }, deleteFile = FALSE)
  
  output$logo_rf <- renderImage({
    list( src = "www/images/Logo_republique_francaise.PNG",
          width = "120px",
          height = "auto")
  }, deleteFile = FALSE)
  
  output$logo_uga <- renderImage({
    list( src = "www/images/Logo_Université_Grenoble_Alpes_2020.svg.PNG",
          width = "120px",
          height = "auto")
  }, deleteFile = FALSE)
  
    output$distPlot <- renderPlotly({
      territoire_name <- input$un_territoire
      column_name <- column_names[column_names$name == input$column_name,]$id
      
      if(territoire_name == "Tous"){filter_data <- data}
      else{filter_data <- data %>%
        filter(territoire == territoire_name)}
      
        couleur = couleur_terr %>% filter(territoire == territoire_name)
        get_graph(filter_data, column_name, couleur$couleur)
    })

      output$selected_var <- renderText({
        paste("Territoire(s) sélectionné(s) : ", input$un_territoire)
      })
      observeEvent(input$bouton_explo, {
        session$sendCustomMessage("ouvrirNouvelleFenetre", texte_explo)
      })
      
      output$Plot_bivarié <- renderPlotly({
        territoire_name <- input$un_territoire
        col1 <- column_names[column_names$name == input$var1,]$id
        col2 <- column_names[column_names$name == input$var2,]$id
        
        if(territoire_name == "Tous"){filter_data <- data}
        else{filter_data <- data %>%
          filter(territoire == territoire_name)}
        
        couleur = couleur_terr %>% filter(territoire == territoire_name)
        get_graph_bivar(filter_data, col1, col2, couleur$couleur)
      })

      output$selected_terr <- renderText({
        paste("Territoire(s) sélectionné(s) : ", input$un_territoire)
      })
      
      output$cartographie_var <- renderLeaflet({
        territoire_name <- input$un_territoire
        column_name <- column_names[column_names$name == input$column_name,]$id
        
        if(territoire_name == "Tous"){filter_data <- data}
        else{filter_data <- data %>%
          filter(territoire == territoire_name)}
        
        carto_var(filter_data, column_name)
      })
      observeEvent(input$bouton_carto, {
        session$sendCustomMessage("ouvrirNouvelleFenetre", texte_carto_var)
      })
      
    output$cartographie <- renderLeaflet({
      carto(input$var_commune)
      })
    output$evo_demande <- renderPlotly({
          demande_territoire()
    })
    output$evo_majoration <- renderPlotly({
      taux_majoration()
    })
    
    output$evo_revenus <- renderPlotly({
      territoire_name <- input$un_territoire_revenus
      if(territoire_name == "Tous"){filter_data <- data}
      else{filter_data <- data %>%
        filter(territoire == territoire_name)}
      evo_revenus(filter_data)
    })
    
    output$montant_aide <- renderPlotly({
      aide_terr()
    })
    output$somme_aide<- renderPlotly({
      somme_des_aides()
    })
    
    output$changement_type <- renderPlotly({
      territoire_name <- input$un_territoire_usage
      if(territoire_name == "Tous"){filter_data <- data}
      else{filter_data <- data %>%
        filter(territoire == territoire_name)}
      changement_app(filter_data)
    })
    
    output$chgt_usage <- renderPlotly({
      territoire_name <- input$un_territoire_usage
      if(territoire_name == "Tous"){filter_data <- data}
      else{filter_data <- data %>%
        filter(territoire == territoire_name)}
      changement_usage(filter_data)
    })
    
    output$evo_combu <- renderPlotly({
      territoire_name <- input$un_territoire_usage
      if(territoire_name == "Tous"){filter_data <- data}
      else{filter_data <- data %>%
        filter(territoire == territoire_name)}
      evo_combustible(filter_data)
    })
    
    output$evo_conso <- renderPlot({
      territoire_name <- input$un_territoire_usage
      if(territoire_name == "Tous"){filter_data <- data}
      else{filter_data <- data %>%
        filter(territoire == territoire_name)}
      evo_conso(filter_data)
    })
    

    output$diff_moyennes <- renderPlotly({
      differences_moy()
    })
    observeEvent(input$bouton_diff_com, {
      session$sendCustomMessage("ouvrirNouvelleFenetre", texte_aide_diff_com)
    })
    
    output$lien_comm <- renderPlot({
      lien_comm_clust()
    })
    observeEvent(input$bouton_OR, {
      session$sendCustomMessage("ouvrirNouvelleFenetre", aide_OR)
    })
    
    output$cartographie_com <- renderLeaflet({
      carto_cluster_com()
    })
    
    output$OR <- renderPlotly({
      plot_OR()
    })
    observeEvent(input$bouton_OR2, {
      session$sendCustomMessage("ouvrirNouvelleFenetre", aide_OR)
    })
    
    output$evo_clust <- renderPlot({
      territoire_name <- input$un_territoire_clust_ind
      
      if(territoire_name == "Tous"){filter_data <- data}
      else{filter_data <- data %>%
        filter(territoire == territoire_name)}
      evo_clust_ind(filter_data)
    })
    
    output$diff_clust_ind <- renderPlotly({
      territoire_name <- input$un_territoire_clust_ind
      
      if(territoire_name == "Tous"){filter_data <- data}
      else{filter_data <- data %>%
        filter(territoire == territoire_name)}
      column_name <- column_names[column_names$name == input$var,]$id
      diff_cluster_ind(column_name, filter_data)
    })
    observeEvent(input$bouton_aide_grp_ind, {
      session$sendCustomMessage("ouvrirNouvelleFenetre", aide_graph_clust_ind)
    })
    output$cartographie_ind <- renderLeaflet({
      carto_cluster_ind()
    })
    
})







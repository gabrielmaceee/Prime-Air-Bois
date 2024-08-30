#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)


# Define UI for application
shinyUI(

  dashboardPage(
    dashboardHeader(title = "Analyse des dossiers de suivi de la Prime Air-Bois", titleWidth = 800),
    dashboardSidebar(
      sidebarMenu(  
        menuItem("Présentation",  tabName = "presentation", icon = icon("font-awesome"), startExpanded = FALSE),
        menuItem("Exploration", tabName = "exploration", icon = icon("a"), startExpanded = FALSE,
                 menuSubItem(tabPanel("Custom",
                                      selectizeInput('un_territoire',
                                                     label='Sélectionner un territoire',
                                                     choices = c("Tous",unique(data$territoire))
                                      )
                 )),
                 menuItem("Univarié", tabName = "univarié", icon = icon("1"), startExpanded = FALSE,

                menuSubItem(tabPanel("Variables",
                                     selectizeInput('column_name',
                                                    label='Sélectionner une variable',
                                                    choices = sort(column_names[order(column_names$order, decreasing = FALSE),]$name)
                             )), tabName = "univarié")),
                menuItem("Cartographie", tabName = "carto_var", icon = icon("2"), startExpanded = FALSE,
                         
                         menuSubItem(tabPanel("Variables",
                                              selectizeInput('column_name',
                                                             label='Sélectionner une variable',
                                                             choices = sort(column_names_var1[order(column_names_var1$order, decreasing = FALSE),]$name)
                                              )), tabName = "carto_var")),
                
                            menuItem("Bivarié", tabName = "bivarié", icon = icon("3"), startExpanded = FALSE,
                                     menuSubItem(tabPanel("Variable1",
                                                          selectizeInput('var1',
                                                                         label='Première variable',
                                                                         choices = sort(column_names_var1[order(column_names_var1$order, decreasing = FALSE),]$name))
                                                          ), tabName = "bivarié"),
                                      menuSubItem(tabPanel("Variable2",
                                                           selectizeInput('var2',
                                                                          label='Deuxième variable',
                                                                          choices = sort(column_names_var2[order(column_names_var2$order, decreasing = FALSE),]$name))
                                                           ), tabName = "bivarié"))

        ),
        
        
        
        
        menuItem("Indicateurs", tabName = "indicateurs", icon = icon("b"), startExpanded = FALSE, 
                 menuItem("La prime et ses bénéficiaires", startExpanded = FALSE,
                           menuItem("Cartographie",tabName = "carto",
                                       menuSubItem(tabPanel("select_var",
                                                            selectizeInput('var_commune',
                                                                           label='Sélectionner une variable',
                                                                           choices = c("Nombre de dossiers", "Population en 2020", "Dossiers par habitant")
                                                            )), tabName = "carto")),
                 
                           menuSubItem(HTML('Évolution demande par <br> territoire') , tabName = "evolution1"),
                           menuSubItem("Évolution prime majorée", tabName = "evolution2"),
                           menuItem("Évolution tranche revenus", tabName = "evolution3",
                                       menuSubItem(tabPanel("select_terr_revenus",
                                                            selectizeInput('un_territoire_revenus',
                                                                           label='Sélectionner un territoire',
                                                                           choices = c("Tous",unique(data$territoire))
                                                            )), tabName = "evolution3")
                                       ),
                           menuSubItem("Montant aide territoires", tabName = "comp_terr"),
                           menuSubItem("Somme totale des aides", tabName = "somme_aide")
                          ),
                 
                 menuItem("Modes d'usages", startExpanded = FALSE,
                          tabPanel("select_terr_usage",
                                               selectizeInput('un_territoire_usage',
                                                              label='Sélectionner un territoire',
                                                              choices = c("Tous",unique(data$territoire))
                                               )),
                          menuSubItem("Évolution type de matériel",tabName = "type_mat"),
                          menuSubItem("Évolution type combustible",tabName = "combustible"),
                          menuSubItem("Évolution usage matériel",tabName = "usage"),
                          menuSubItem("Évolution consommation",tabName = "conso"))
                 ),
        menuItem("Typologies", tabName = "typologie", icon = icon("c"), startExpanded = FALSE, 
                 menuItem("Communale", startExpanded = FALSE,
                          menuSubItem("Description méthode", tabName = "description_com"),
                          menuSubItem("Analyse des groupes", tabName = "diff_com"),
                          menuSubItem(HTML("Lien avec <br>le type de commune"), tabName = "type_com"),
                          menuSubItem("Cartographie des groupes", tabName = "carto_com")),
                 
                 menuItem("Individuelle", startExpanded = FALSE,
                          menuSubItem("Description méthode", tabName = "description_ind"),
                          menuSubItem("Analyse des groupes", tabName = "diff_ind"),
                          menuSubItem("Cartographie des groupes", tabName = "carto_ind"),
                          menuItem("Comparaison des groupes", startExpanded = FALSE,
                            tabPanel("Variables",
                                               selectizeInput('un_territoire_clust_ind',
                                                              label='Sélectionner un territoire',
                                                              choices = c("Tous",unique(data$territoire))
                                               )),
                            menuSubItem(tabPanel("Univarié",
                                               selectizeInput('var',
                                                              label='Sélectionner une variable',
                                                              choices = sort(column_names[order(column_names$order, decreasing = FALSE),]$name)
                                               ))
                          , tabName = "var_grp_ind"),
                          menuSubItem("Évolution des groupes", tabName = "tab_evo_clust")
                          )
                          )# expliquer qu'en regardant les périodes, il n'y a pas de différences de types de groupes
                 
        )
      )
    ),
    dashboardBody(
      tags$head(
        tags$script(HTML("
      Shiny.addCustomMessageHandler('ouvrirNouvelleFenetre', function(message) {
        var nouvelleFenetre = window.open('', '_blank', 'width=600,height=400');
        nouvelleFenetre.document.write('<html><head><title>Aide</title></head><body>' + message + '</body></html>');
        nouvelleFenetre.document.close();
      });
    "))
      ),
      tabItems(
        tabItem(tabName = "presentation",
                h2(HTML("<center><font face='Comic sans MS'>Bienvenue sur explorePAB !</font></center>")),
                h5(texte_presentation),
                h5(texte_donnees),
                
                div(
                  style = "
      position: fixed;
      bottom: 0;
      left: 0;
      width: 100%;
      background-color: transparent;
      padding: 10px;
    ",
                  fluidRow(
                    column(4, align = "left", offset = 2,
                           div(style = "text-align: left;", imageOutput("logo_inrae", width = "auto", height = "auto"))),
                    column(2, align = "center", 
                           div(style = "text-align: center;", imageOutput("logo_rf", width = "auto", height = "auto"))),
                    column(4, align = "right", 
                           div(style = "text-align: right;", imageOutput("logo_uga", width = "auto", height = "auto")))
                  )
                )
        ),
        
        tabItem(tabName = "univarié",
                plotlyOutput("distPlot", height = "750px", width =  "100%"),
                h4(textOutput("selected_var")),
                actionButton("bouton_explo", "Aide"),
                h4(texte_auteur),
                h4(source)
        ),
        tabItem(tabName = "carto_var",
                leafletOutput("cartographie_var", height = "750px", width =  "100%"),
                actionButton("bouton_carto", "Aide"),
                h4(texte_auteur),
                h4(source)
        ),
        
        tabItem(tabName = "bivarié",
                plotlyOutput("Plot_bivarié", height = "750px", width =  "100%"),
                h4(textOutput("selected_terr")),
                h4(texte_auteur),
                h4(source)
        ),

        
        tabItem(tabName = "carto",
                leafletOutput("cartographie", height = "750px", width =  "100%"),
                h4(texte_auteur),
                h4(source)
        ),
        tabItem(
          tabName = "evolution1",
              plotlyOutput("evo_demande", width = "100%", height = "750px"),
              h4(texte_auteur),
              h4(source),
              h4(precaution_demande)
            ),
        tabItem(tabName = "evolution2", plotlyOutput("evo_majoration", height = "750px", width =  "100%"),
                h4(texte_auteur),
                h4(source),
                h4(precaution_majoration )),
        tabItem(tabName = "evolution3", plotlyOutput("evo_revenus", height = "750px", width =  "100%"),
                h4(texte_auteur),
                h4(source),
                h4(precaution_tranche_revenu)),
        tabItem(tabName = "comp_terr", plotlyOutput("montant_aide", height = "750px", width =  "100%"),
                h4(texte_auteur),
                h4(source)),
        tabItem(tabName = "somme_aide", plotlyOutput("somme_aide", height = "750px", width =  "100%"),
                h4(texte_auteur),
                h4(HTML("<B> <U> Données</U> : </B> <font size='3'>
                        6197 primes versées de janvier 2016 à décembre 2023, source : Dossiers Prime Air-Bois : AGEDEN / ALEC </font>")),
                h4(precaution_somme_aide)),
        
        tabItem(tabName = "type_mat", plotlyOutput("changement_type", height = "750px", width =  "100%"),
                h4(texte_auteur),
                h4(source),
                h4(precaution_materiel)),
        tabItem(tabName = "combustible", plotlyOutput("evo_combu", height = "750px", width =  "100%"),
                h4(texte_auteur),
                h4(source),
                h4(precaution_combustible)),
        tabItem(tabName = "usage", plotlyOutput("chgt_usage", height = "750px", width =  "100%"),
                h4(texte_auteur),
                h4(source),
                h4(precaution_usage)),
        tabItem(tabName = "conso", plotOutput("evo_conso", height = "750px", width =  "100%"),
                h4(texte_auteur),
                h4(source_conso),
                h4(precaution_evo_conso)),
        
        tabItem(tabName = "description_com", h4(methode_acp)),
        tabItem(tabName = "diff_com", plotlyOutput("diff_moyennes", height = "890px", width =  "100%"),
                actionButton("bouton_diff_com", "Aide interprétation"), h4(texte_auteur)),
        tabItem(tabName = "type_com", plotOutput("lien_comm", height = "750px", width =  "100%"),
                actionButton("bouton_OR", "Aide Odds ratios"), h4(texte_auteur)),
        tabItem(tabName = "carto_com", leafletOutput("cartographie_com", height = "750px", width =  "100%"),
                h4(texte_auteur),
                h4(texte_types_communes)),
        
        tabItem(tabName = "description_ind", h4(methode_afdm)),
        tabItem(tabName = "diff_ind", plotlyOutput("OR", height = "890px", width =  "100%"),
                actionButton("bouton_OR2", "Aide Odds Ratios"),
                h4(texte_auteur)),
        tabItem(tabName = "tab_evo_clust", plotOutput("evo_clust", height = "750px", width =  "100%"),
                h4(texte_auteur)),
        tabItem(tabName = "var_grp_ind", plotlyOutput("diff_clust_ind", height = "750px", width =  "100%"),
                actionButton("bouton_aide_grp_ind", "Aide"),
                h4(texte_auteur)),
        tabItem(tabName = "carto_ind", leafletOutput("cartographie_ind", height = "750px", width =  "100%"),
                h4(aide_map_clust_ind),
                h4(texte_auteur))
        
    )
  )
)
)
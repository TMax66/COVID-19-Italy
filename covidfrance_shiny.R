library(leaflet)
library(sf)
library(rmapshaper)
library(dplyr, warn.conflicts = FALSE)
library(smoothr)
library(shiny)

u <- httr::GET('https://www.data.gouv.fr/api/1/datasets/5e7e104ace2080d9162b61d8/')
url_search <- httr::content(u)$resources

df_date <- tibble(url = url_search %>% purrr::map_chr('url'),
                  timestamp = url_search %>% purrr::map_chr('last_modified')) %>% 
    filter(grepl('hospitalieres-covid', url)) %>% 
    arrange(desc(timestamp)) %>% 
    pull(timestamp) %>% 
    .[1] %>% 
    lubridate::as_datetime() %>% 
    format(., '%Y-%m-%d à %Hh%Mm')

dat_cov <- readr::read_csv2('https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7') %>% 
    filter(sexe == 0) %>% 
    filter(!is.na(jour)) %>% 
    select(dep, jour, hosp, rad, rea, dc)

ui <- bootstrapPage(
    
    tags$head(
        tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
        tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
        #includeHTML("meta.html"),
        tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                    type="text/javascript")
        # tags$script('
        #         $(document).ready(function () {
        #           navigator.geolocation.getCurrentPosition(onSuccess, onError);
        #         
        #           function onError (err) {
        #             Shiny.onInputChange("geolocation", false);
        #           }
        #         
        #           function onSuccess (position) {
        #             setTimeout(function () {
        #               var coords = position.coords;
        #               console.log(coords.latitude + ", " + coords.longitude);
        #               Shiny.onInputChange("geolocation", true);
        #               Shiny.onInputChange("lat", coords.latitude);
        #               Shiny.onInputChange("long", coords.longitude);
        #             }, 1100)
        #           }
        #         });
        #         ')
    ),
    
    # titlePanel("Covid Simple Map"),
    # 
    # Sidebar with a slider input for year of interest
    # sidebarLayout(
    #   sidebarPanel(
    #     sliderInput("jour",h3(""),
    #                 min = min(dat_cov$jour), max = max(dat_cov$jour), step = 1, 
    #                 value = max(dat_cov$jour),
    #                 animate = animationOptions(interval = 1000, loop = TRUE)),
    #     shinyWidgets::prettyRadioButtons('sel_data', 'Donnée affichée', choices = c('Hospitalisation', 'Réanimation', 'Retour à domicile', 'Décès'), 
    #                                      selected = 'Hospitalisation', 
    #                                      shape = "round", animation = "jelly",plain = TRUE,bigger = TRUE,inline = FALSE)
    #   ),
    
    leafletOutput("covid", width = "100%", height = "100%"),
    
    absolutePanel(
        bottom = 20, left = 40, draggable = TRUE, width = "20%", style = "z-index:500; min-width: 300px;",
        titlePanel("France | Covid"),
        # br(),
        em('La donnée est affichée en plaçant la souris sur la carte'),
        sliderInput("jour",h3(""),
                    min = min(dat_cov$jour), max = max(dat_cov$jour), step = 1, 
                    value = max(dat_cov$jour),
                    animate = animationOptions(interval = 1700, loop = FALSE)),
        
        shinyWidgets::prettyRadioButtons('sel_data', 'Donnée affichée', 
                                         choices = c('Hospitalisés', 'En réanimation', 'Retours à domicile (cumulés)', 'Décès (cumulés)'), 
                                         selected = 'Hospitalisés', 
                                         shape = "round", animation = "jelly",plain = TRUE,bigger = FALSE,inline = FALSE),
        shinyWidgets::prettySwitch('pop', "Ratio / 100 000 habitants*", FALSE),
        em(tags$small("*à noter sur ce ratio : un patient peut être hospitalisé plus d'une fois")),
        em(tags$small(br(), "Pour les décès, il s'agit de ceux ayant lieu à l'hôpital")),
        h5(tags$a(href = 'http://github.com/GuillaumePressiat', 'Guillaume Pressiat')),
        
        h5(em('Dernière mise à jour le ' , df_date)),
        
        #br(),
        tags$small(tags$li(tags$a(href = 'https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19', 'Données recueil Covid')),
                   tags$li(tags$a(href = 'https://github.com/gregoiredavid/france-geojson', 'Geojson contours départements')),
                   tags$li(tags$a(href = 'https://www.insee.fr/fr/statistiques/2012713#tableau-TCRD_004_tab1_departements', 'Populations Insee')),
                   tags$li(tags$a(href = 'http://r.iresmi.net/2020/04/01/covid-19-decease-animation-map/', 'Voir également ce lissage territorial')),
                   tags$li(tags$a(href = 'http://www.fabiocrameri.ch/resources/ScientificColourMaps_FabioCrameri.png', 'Scientific colour maps'), ' with ',
                           tags$a(href = 'https://cran.r-project.org/web/packages/scico/index.html', 'scico package'))))
    
    
)


#data.p <- sf::st_read("Downloads/contours-simplifies-des-departements-francais-2015.geojson") %>% 
# https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson
# data.p <- sf::st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements-avec-outre-mer.geojson") %>%
# #  filter(! code_reg %in% c('01', '02', '03', '04', '06')) %>%
#   ms_simplify(keep = 0.03) %>%
#   smooth(method = "chaikin")

pops <- readr::read_csv2('pop_insee.csv')

#st_write(data.p, 'deps.geojson', delete_dsn = TRUE)
 data.p <- st_read('deps.geojson')

data <- data.p %>% 
    #left_join(dat_cov, by = c('code_dept' = 'dep')) %>% 
    left_join(dat_cov, by = c('code' = 'dep')) %>% 
    left_join(pops, by = c('code' = 'dep'))


server <- function(input, output) {
    
    
    get_data <- reactive({
        temp <- data[which(data$jour == input$jour),]
        if (input$sel_data == "Hospitalisés"){
            temp$val <- temp$hosp
        } else if (input$sel_data == "En réanimation"){
            temp$val <- temp$rea
        } else if (input$sel_data == "Retours à domicile (cumulés)"){
            temp$val <- temp$rad
        } else if (input$sel_data == "Décès (cumulés)"){
            temp$val <- temp$dc
        } 
        
        
        
        temp$label <- temp$val
        
        if (input$pop){
            temp$val <- (temp$val * 100000) / temp$pop2020
            temp$label <- paste0(temp$label, '<br><em>', round(temp$val,1), ' / 100 000 hab.</em><br>', prettyNum(temp$pop2020, big.mark = ' '), ' habitants')
        }
        
        
        return(temp)
        
    })
    values_leg <- reactive({
        temp <- data
        if (input$sel_data == "Hospitalisés"){
            temp$leg <- temp$hosp
        } else if (input$sel_data == "En réanimation"){
            temp$leg <- temp$rea
        } else if (input$sel_data == "Retours à domicile (cumulés)"){
            temp$leg <- temp$rad
        } else if (input$sel_data == "Décès (cumulés)"){
            temp$leg <- temp$dc
        } 
        if (input$pop){
            temp$leg <- (temp$leg * 100000) / temp$pop2020
        }
        temp <- temp$leg
        # if (input$log){
        # temp <- log(temp)
        # temp[temp < 0] <- 0
        # }
        return(temp)
    })
    
    leg_title <- reactive({
        if (input$pop){
            htmltools::HTML('Nb pour<br>100 000 hab.')
        } else{
            'Nb'
        }
    })
    
    output$covid <- renderLeaflet({
        leaflet(data = data.p) %>%
            addProviderTiles("CartoDB", options = providerTileOptions(opacity = 1, minZoom = 3, maxZoom = 7), group = "Open Street Map") %>%
            setView(lng = 1, lat = 46.71111, zoom = 6) %>%
            addPolygons(group = 'base', 
                        fillColor = NA, 
                        color = 'white',
                        weight = 1.5)  %>%
            addLegend(pal = pal(), values = values_leg(), opacity = 1, title = leg_title(), 
                      position = "topright")
    })
    
    
    
    pal <- reactive({
        
        if (input$sel_data != "Retours à domicile (cumulés)"){
            return(colorNumeric(scico::scico(n = 300, palette = "tokyo", direction = - 1, end = 0.85), values_leg(), na.color = NA))
        } else { 
            return(colorNumeric(scico::scico(n = 300, palette = "oslo", direction = - 1, begin = 0.2, end = 0.85), domain = values_leg(), na.color = NA))
        }
    })
    
    
    observe({
        if(input$jour == min(dat_cov$jour)){
            data <- get_data()
            leafletProxy('covid', data = data) %>%
                clearGroup('polygons') %>%
                addPolygons(group = 'polygons', 
                            fillColor = ~pal()(val), 
                            fillOpacity = 1, 
                            stroke = 2,
                            color = 'white',
                            weight = 1.5, label = ~ lapply(paste0("<b>", code, " - ", nom, "</b><br>",jour, ' : ', label), htmltools::HTML))
        } else {
            data <- get_data()
            leafletProxy('covid', data = data) %>%
                #clearGroup('polygons') %>%
                addPolygons(group = 'polygons',
                            fillColor = ~pal()(val), 
                            fillOpacity = 1, 
                            stroke = 2,
                            color = 'white',
                            weight = 1.5, label = ~ lapply(paste0("<b>", code, " - ", nom, "</b><br>",jour, ' : ', label), htmltools::HTML))
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



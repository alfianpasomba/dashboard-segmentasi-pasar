library(shiny)
library(shinydashboard)
library(factoextra)
library(ggplot2)
library(dplyr)

options(shiny.maxRequestSize=50*1024^2)


ui <- dashboardPage(
  dashboardHeader(title="Market Segmentation"),
  dashboardSidebar(sidebarMenu(
    menuItem("Upload Data", icon = icon("cloud-upload"), tabName = "data"),
    menuItem("Informasi Data", icon = icon("info-circle"), tabName = "summary"),
    menuItem("Visualisasi Data", icon = icon("area-chart"), tabName = "eda"),
    menuItem("Segmentasi Data", icon = icon("pie-chart"), tabName = "model")
  )),
  dashboardBody(
    
    #-------------------------custom css-------------------------
    tags$script(HTML("$('body').addClass('fixed');")),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      
      #-------------------------modul upload data-------------------------
      
      tabItem(tabName = "data",
              
              fluidRow(
                column(width=12,
                    box(title = "Petunjuk",  collapsible = TRUE, width = 12, collapsed = TRUE,
                        valueBox(1, 'Ceklis "Gunakan Data Contoh" jika belum memiliki data', icon = icon("cog"),width = 3),
                        valueBox(2, "Unceklis jika ingin mengupload data (csv maks 100mb)", icon = icon("file-excel-o"), width = 3),
                        valueBox(3, 'Klik "Browse" dan pilih file yang ingin diupload', icon = icon("arrow-down"), width = 3),
                        valueBox(4, "Data akan tampil jika berhasil terupload", icon = icon("table"), width = 3),
                        )
                  )
              ),
              
              #--------------------upload data--------------------
              
              fluidRow(
                column(width=9,
                    box(title = "Data",  width=12,
                      dataTableOutput("contents"))
                  ),
                column(width=3,
                    box(width = 12,
                      checkboxInput("default", "Gunakan Data Contoh", TRUE),
                    ),
                      uiOutput("form_file")
                    )
              ),
              
      ),
      
      #-------------------------modul Summary-------------------------
      
      tabItem(tabName = "summary",
              fluidRow(
                column(width=12,
                       box(title = "Petunjuk",  collapsible = TRUE, width = 12, collapsed = TRUE,
                           valueBox(1, 'Pilih salah satu kolom', icon = icon("columns"),width = 3),
                           valueBox(2, "Informasi kolom akan tampil", icon = icon("file-text-o"), width = 3),
                           )
                )),
              fluidRow(
                uiOutput("info_full"),
                column(width=3,
                  box(width=12,
                      uiOutput("info_kolom")
                  )
                )
              )
      ),
      
      #-------------------------modul edit-------------------------
      
      # tabItem(tabName = "feature",
      #         fluidRow(
      #           column(width=12,
      #             box(title = "Petunjuk",  collapsible = TRUE, collapsed = TRUE, width = 12,
      #                 p("ini adalah modul edit")
      #             )
      #           )
      #         ),
      #         fluidRow(width=12,
      #           column(width = 9,
      #             box(title = "Data", width = 12,
      #                 dataTableOutput("data_edit")
      #                 )),
      #           column(width=3,
      #             box(width = 12,
      #                 radioButtons("null_input", "Data Kosong", c("Hapus Baris","Ganti Rata-Rata/Modus"))
      #             ),
      #             box(width=12,
      #                 radioButtons("kategori_handling", "Variabel Kategori",c("Hapus Kolom", "Ubah Menjadi Numerik"))
      #                 )
      #           )
      #         )
      # ),
      
      #-------------------------modul EDA-------------------------
      
      tabItem(tabName = "eda",
              fluidRow(
                column( width = 12,
                        box(title = "Petunjuk",  collapsible = TRUE, width = 12, collapsed = TRUE,
                            valueBox(1, 'Pilih berapa banyak variabel', icon = icon("sort-numeric-asc"),width = 3),
                            valueBox(2, "Pilih salah satu jenis variabel", icon = icon("sort-amount-asc"), width = 3),
                            valueBox(3, "Pilih salah satu jenis plot", icon = icon("bar-chart"), width = 3),
                            valueBox(4, 'Pilih salah satu variabel', icon = icon("crosshairs"), width = 3),
                        )
                )
              ),
              
              fluidRow(
                column(width = 9,
                  box(title = "Visualisasi",  width = 12,
                      plotOutput("hasil_visualisasi")
                      )
              ),
                column(width = 3,
                  box(width = 12,
                      radioButtons("jumlah_variabel", "Jumlah Variabel", c("Satu", "Dua"), inline = TRUE)
                      ),
                  uiOutput("pilih_jenis_variabel"),
                  uiOutput("pilih_grafik")
                )
              )
      ),
      
      #-------------------------modul segmentasi-------------------------
      
      tabItem(tabName = "model",
              fluidRow(
                column(width = 12,
                       box(title = "Petunjuk",  collapsible = TRUE, width = 12, collapsed = TRUE,
                           valueBox(1, 'Geser slider untuk memilih berapa banyak cluster', icon = icon("sliders"),width = 3),
                           valueBox(2, "Model hanya akan mengambil variabel numerik", icon = icon("sort-numeric-desc"), width = 3),
                           )
                )
              ),
              
              
              fluidRow(
                column(width = 9,
                  box(title = "Visualisasi Kluster",width = 12, 
                      plotOutput("kmeans")
                  )
                ),
                column(width = 3,
                       box(width = 12,
                           sliderInput("cluster", label = "Jumlah Kluster", min=3, max=15, value=5))
                  )
                )
      )
    )
  )
)





server <- function(input, output) {
  
  #-------------------------output tabel-------------------------
  
  data <- reactive({
    file_path <- "data/demografi.csv"
    
    if (input$default == FALSE) {
      req(input$file1)
      file_path <- input$file1$datapath
    }
    
    tryCatch(
      {
        df <- read.csv(file_path,
                       header = TRUE,
                       na.strings = ""
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return(df)
  })
  
  
  output$contents <- renderDataTable({
    data()
  }, options = list(pageLength = 7, paging=TRUE, scrollX= TRUE, dom="tp", searching=FALSE))

  
  
  output$form_file <- renderUI({
    if (input$default == FALSE){
      box(width=12, fileInput("file1", "Upload File CSV",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv"))
      )
    }
  })
  
  
  
  #----------------------output kluster-------------------------
  
  
  output$kmeans <- renderPlot({
    kolom_numerik = colnames(select_if(data(), is.numeric))
    data_numerik = select(data(), kolom_numerik)
    
    data_normal <- scale(data_numerik)
    result <- kmeans(data_normal, iter.max = 1000, input$cluster, nstart = 10)
    
    fviz_cluster(result, geom = "point", data = data_normal) + ggtitle(paste("k = ", input$cluster))
  })
  
  
  #-----------------------------output summary-----------------------------
  
  output$info_kolom <- renderUI({
    selectInput("kolom","Kolom", c(colnames(data())))
  })
  
  output$info_full <- renderUI({
    kolom_numerik = colnames(select_if(data(), is.numeric))
    kolom_kategori = colnames(select(data(), -kolom_numerik))
    
    if (input$kolom %in% kolom_kategori){
      column(width=9,
             box(title="Info", width=12, 
                 infoBox(
                   "Tipe Data", typeof(data()[[input$kolom]]), icon = icon("sort-alpha-desc"),
                   color = "blue",fill = TRUE
                 ),
                 infoBox(
                   "Data Kosong", sum(is.na(data()[[input$kolom]])), icon = icon("trash-o"),
                   color = "blue",fill = TRUE
                 ),
                 infoBox(
                   "Data Unik", length(unique(data()[[input$kolom]])), icon = icon("user-o"),
                   color = "blue",fill = TRUE
                 )
             )
        )
    }
    else{
    column(width=9,
           box(title="Info", width=12,
             infoBox(
               "Tipe Data", typeof(data()[[input$kolom]]), icon = icon("sort-numeric-asc"),
               color = "blue",fill = TRUE
             ),
             infoBox(
               "Data Kosong", sum(is.na(data()[[input$kolom]])), icon = icon("trash-o"),
               color = "blue",fill = TRUE
             ),
             infoBox(
               "Data Unik", length(unique(data()[[input$kolom]])), icon = icon("user-o"),
               color = "blue",fill = TRUE
             ),
             infoBox(
               "Nilai Terendah", summary(data()[[input$kolom]])[1], icon = icon("arrow-circle-o-down"),
               color = "blue",fill = TRUE
             ),
           infoBox(
             "Nilai Tertinggi", summary(data()[[input$kolom]])[6], icon = icon("arrow-circle-o-up"),
             color = "blue",fill = TRUE
           ),
           infoBox(
             "Rata-rata", round(summary(data()[[input$kolom]])[4],2), icon = icon("circle-o"),
             color = "blue",fill = TRUE
           ),
           infoBox(
             "Kuartil 1", summary(data()[[input$kolom]])[2], icon = icon("arrow-circle-o-right"),
             color = "blue",fill = TRUE
           ),
           infoBox(
             "Nilai Tengah", summary(data()[[input$kolom]])[3], icon = icon("dot-circle-o"),
             color = "blue",fill = TRUE
           ),
           infoBox(
             "Kuartil 3", summary(data()[[input$kolom]])[5], icon = icon("arrow-circle-o-left"),
             color = "blue",fill = TRUE
           )
        )
    )
    }
  })
  
  
  #-----------------------------output EDA-----------------------------
  
  
  output$pilih_jenis_variabel <- renderUI({
      if (input$jumlah_variabel == "Satu"){
        box(width = 12,
          radioButtons("jenis_variabel","Jenis Variabel",c("Numerik","Kategorik")),
        )
      }
      else {
        box(width = 12,
          radioButtons("jenis_variabel","Jenis Variabel",c("2 Numerik","1 Numerik 1 Kategorik", "2 Kategorik"))
        )
      }
  })

  output$pilih_grafik <- renderUI({
    
    
    
    kolom_numerik = colnames(select_if(data(), is.numeric))
    if("id" %in% colnames(data()))
    {
      kolom_kategori = colnames(select(data(), -kolom_numerik,-id))
    }else{
      kolom_kategori = colnames(select(data(), -kolom_numerik))
    }
    
    if (input$jenis_variabel == "Numerik") {
      box(width = 12,
          selectInput("jenis_grafik", "Jenis Plot", c("Area", "Density", "Histogram")),
          selectInput("variabel1", "Variabel", c(kolom_numerik))
          )
    }
    else if (input$jenis_variabel == "Kategorik") {
      box(width = 12,
          selectInput("jenis_grafik", "Jenis Plot", c("Bar")),
          selectInput("variabel1", "Variabel", c(kolom_kategori))
      )
    }
    else if (input$jenis_variabel == "2 Numerik") {
      box(width = 12,
          selectInput("jenis_grafik", "Jenis Plot", c("Scatter")),
          selectInput("variabel1", "Variabel 1", c(kolom_numerik)),
          selectInput("variabel2", "Variabel 2", c(kolom_numerik))
      )
    }
    else if (input$jenis_variabel == "1 Numerik 1 Kategorik") {
      box(width = 12,
          selectInput("jenis_grafik", "Jenis Plot", c("Bar 2 Variabel", "Box", "Violin")),
          selectInput("variabel1", "Variabel Kategorik", c(kolom_kategori)),
          selectInput("variabel2", "Variabel Numerik", c(kolom_numerik))
      )
    }
    else if (input$jenis_variabel == "2 Kategorik") {
      box(width = 12,
          selectInput("jenis_grafik", "Jenis Plot", c("Count","Jitter")),
          selectInput("variabel1", "Variabel 1", c(kolom_kategori)),
          selectInput("variabel2", "Variabel 2", c(kolom_kategori))
      )
    }
  })
  
  #-----------------------------output visualisasi-----------------------------
  
  output$hasil_visualisasi <- renderPlot({
    
    options(scipen=10000)
    
    if (input$jenis_grafik == "Area"){
      max <- max(data()[[input$variabel1]])
      p <- ggplot(data(), aes_string(x=input$variabel1))
      p + geom_area(stat = "bin", bins=15)
    }
    else if(input$jenis_grafik == "Density"){
      p <- ggplot(data(), aes_string(x=input$variabel1))
      p + geom_density(kernel="gaussian")
    }
    else if(input$jenis_grafik == "Histogram"){
      max <- max(data()[[input$variabel1]])
      p <- ggplot(data(), aes_string(x=input$variabel1))
      p + geom_histogram(binwidth = max/15)
    }
    else if(input$jenis_grafik == "Bar"){
      p <- ggplot(data(), aes_string(x=input$variabel1))
      p + geom_bar()
    }
    else if(input$jenis_grafik == "Scatter"){
      p <- ggplot(data(), aes_string(x=input$variabel1,y=input$variabel2))
      p + geom_point()
    }
    else if(input$jenis_grafik == "Bar 2 Variabel"){
      p <- ggplot(data(), aes_string(x=input$variabel1,y=input$variabel2))
      p + geom_col()
    }
    else if(input$jenis_grafik == "Box"){
      p <- ggplot(data(), aes_string(x=input$variabel1,y=input$variabel2))
      p + geom_boxplot()
    }
    else if(input$jenis_grafik == "Violin"){
      p <- ggplot(data(), aes_string(x=input$variabel1,y=input$variabel2))
      p + geom_violin(scale="area")
    }
    else if(input$jenis_grafik == "Count"){
      p <- ggplot(data(), aes_string(x=input$variabel1,y=input$variabel2))
      p + geom_count()
    }
    else if(input$jenis_grafik == "Jitter"){
      p <- ggplot(data(), aes_string(x=input$variabel1,y=input$variabel2))
      p + geom_jitter(height = 2, width = 2)
    }
  })
  
  #----------------------------output edit-------------------------
  
  
  # data_sebelum_edit <- reactive({
  #   data()
  # })
  # 
  # output$data_edit <- renderDataTable({
  #   
  #   kolom_numerik = colnames(select_if(data(), is.numeric))
  #   kolom_kategori = colnames(select(data(), -kolom_numerik))
  #   
  #   if(input$null_input == "Hapus Baris"){
  #     data_hasil_edit <- na.omit(data_sebelum_edit())
  #     return(data_hasil_edit)
  #   }
  #   else if (input$null_input == "Ganti Rata-Rata/Modus"){
  #     data_hasil_edit <- na_mean(data_sebelum_edit())
  #     return(data_hasil_edit)
  #   }
  #   }, options = list(pageLength = 7, paging=TRUE, scrollX= TRUE, dom="tpi", searching=FALSE)
  # )
  
    
}

shinyApp(ui, server)
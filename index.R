# shiny-related package
library(shiny)
library(shinymanager)
library(shinyalert)
library(bs4Dash)
library(shinyWidgets)
library(DT)

# database package
library(DBI)
library(RMariaDB)
library(dplyr)
library(dbplyr)
library(RSQLite)

# misc
library(readxl)

# utils function data and function
db <- data.frame(host = "localhost",
                 dbname = "dewantara",
                 port = 3307,
                 username = "user1", 
                 password = "P@ssw0rd", stringsAsFactors = FALSE)
cred <- function(db){
  mdbcon <- dbConnect(MariaDB(),
                      host = db$host,
                      dbname = db$dbname,
                      port = db$port,
                      username = db$username, 
                      password = db$password)
  credentials <- mdbcon %>% 
    tbl("view_credentials") %>% 
    collect() %>% 
    mutate(admin = as.logical(admin))
  create_db(credentials, sqlite_path = "db/database.sqlite", passphrase = "secret")
  dbDisconnect(mdbcon)
}
cred(db)

getTable <- function(db, tblnm){
  mdbcon <- dbConnect(MariaDB(),
                      host = db$host,
                      dbname = db$dbname,
                      port = db$port,
                      username = db$username, 
                      password = db$password)
  outtbl <- mdbcon %>% 
    tbl(tblnm) %>% 
    collect()
  dbDisconnect(mdbcon)
  return(outtbl)
}


hasTable <- function(db, tblnm){
  mdbcon <- dbConnect(MariaDB(),
                      host = db$host,
                      dbname = db$dbname,
                      port = db$port,
                      username = db$username, 
                      password = db$password)
  out <- tblnm %in% dbListTables(mdbcon)
  dbDisconnect(mdbcon)
  return(out)
}

set_labels(language = "en", "Please authenticate" = "", "Username:" = "NIS", "Password:" = "Password")

ui <- bs4DashPage(navbar = bs4DashNavbar(skin = "dark", status = "info", border = TRUE, compact = FALSE#, 
                                          # rightUi = actionBttn(inputId = "logout", icon = icon("power-off"), style = "simple", color = "danger", size = "sm")
                                         ), 
                  sidebar = bs4DashSidebar(inputId = "sidebar", disable = FALSE, title = p(HTML("SMK Kesehatan Dewantara<br/>Aplikasi Ujian Berbasis Daring"), style = "text-align:left;font-size: 10px;font-weight:bold;margin-left:8px;margin-top:0;margin-bottom:0;"), 
                                           skin = "light", status = "info", src = "img/about-img.jpg", 
                                           expand_on_hover = TRUE, elevation = 1, opacity = 0.6, 
                                           
                                           # bs4SidebarHeader(title = img(src = "img/about-img.jpg", width = "80%")),
                                           bs4SidebarMenu(id = "", flat = FALSE, 
                                                          bs4SidebarMenuItem(text = p("Beranda"), tabName = "beranda", icon = "home"), 
                                                          bs4SidebarMenuItem(text = p("Ujian"), tabName = "set_sch_ujian", icon = "paste", 
                                                                             bs4SidebarMenuSubItem(text = p("Atur Jadwal"), tabName = "atur_jadwal", icon = "calendar"), 
                                                                             bs4SidebarMenuSubItem(text = p("Upload Soal"), tabName = "upload_soal", icon = "envelope-open-text"), 
                                                                             bs4SidebarMenuSubItem(text = p("Preview Soal"), tabName = "preview_soal", icon = "leanpub")
                                                                             )
                                                          )
                                           ), 
                  body = bs4DashBody(
                    # auth_ui(
                    #   id = "auth",
                    #   tags_top = tags$div(style="display:flex;",
                    #                       div(style="width: 20%;margin-left:20px;padding:0;",
                    #                           img(src="img/about-img.jpg", width="80px", style="margin:0")
                    #                       ),
                    #                       div(style="width: 79%;margin:0;padding-left:10px;",
                    #                           h4("SMK Kesehatan Dewantara", style="font-weight: bold;text-align: left;margin-left: 0;margin-top: 10px;margin-bottom: 0;padding: 0;"),
                    #                           h5("Sistem Informasi Pendidikan", style="text-align: left;margin-left: 0;margin-top: 3px;margin-bottom: 3px;padding: 0;font-size: 120%;"),
                    #                           h5("Aplikasi Berbasis Daring", style="text-align: left;margin: 0;padding: 0;font-size: 120%;")
                    #                       )
                    #   ), 
                    #   choose_language = FALSE
                    # ),
                    useShinyalert(),
                    bs4TabItems(
                      bs4TabItem(tabName = "beranda",
                                 p("Text bla bla bla ...")),
                      bs4TabItem(tabName = "atur_jadwal",
                                 bs4Card(title = "Atur Jadwal Ujian", status = "primary", width = 12, 
                                         solidHeader = TRUE, collapsed = FALSE, closable = FALSE, 
                                         maximizable = FALSE, labelStatus = "primary", labelTooltip = "Jadwal Ujian",
                                         uiOutput("pengatur_jadwal")),
                                 bs4Card(title = "Jadwal Ujian", status = "primary", width = 12, 
                                         solidHeader = TRUE, collapsed = FALSE, closable = FALSE, 
                                         maximizable = FALSE, labelStatus = "primary", labelTooltip = "Jadwal Ujian",
                                         selectInput("hariujian", "Pilih Hari Ujian", choices = c("Senin", "Selasa", "Rabu", "Kamis", "Jumat", "Sabtu")),
                                         dataTableOutput("jadwal")),
                                 br()),
                      bs4TabItem(tabName = "upload_soal",
                                 p("Upload bla bla bla ..."),
                                 bs4Card(title = "Upload File Soal", status = "primary", width = 12,
                                         solidHeader = TRUE, collapsed = FALSE, closable = FALSE,
                                         maximizable = FALSE, labelStatus = "primary",
                                         fileInput(inputId = "data_upload_soal", label = "Upload Excel File Soal", multiple = FALSE),
                                         uiOutput("upload_soal_ui")),
                                 # bs4Card(title = "Jadwal Ujian", status = "primary", width = 12, 
                                 #         solidHeader = TRUE, collapsed = FALSE, closable = FALSE, 
                                 #         maximizable = FALSE, labelStatus = "primary", 
                                 #         dataTableOutput("jadwal")),
                                 br()),
                      bs4TabItem(tabName = "preview_soal",
                                 p("Preview bla bla bla ..."))
                    )
                  ), 
                  controlbar = NULL, 
                  footer = NULL, #bs4DashFooter(copyrights = HTML("2020 &copy; aephidayatuloh")), 
                  title = "Aplikasi Ujian Berbasis Daring", 
                  old_school = FALSE, sidebar_mini = , 
                  controlbar_collapsed = TRUE, sidebar_collapsed = FALSE,
                  enable_preloader = FALSE)
# ui <- secure_app(ui, theme = "paper", status = "primary", enable_admin = TRUE,language = "en",
#                  tags_top = tags$div(style="display:flex;",
#                                      div(style="width: 20%;margin-left:20px;padding:0;",
#                                          img(src="img/about-img.jpg", width="80px", style="margin:0")
#                                          ),
#                                      div(style="width: 79%;margin:0;padding-left:10px;",
#                                          h4("SMK Kesehatan Dewantara", style="font-weight: bold;text-align: left;margin-left: 0;margin-top: 10px;margin-bottom: 0;padding: 0;"),
#                                          h5("Sistem Informasi Pendidikan", style="text-align: left;margin-left: 0;margin-top: 3px;margin-bottom: 3px;padding: 0;font-size: 131%;"),
#                                          h5("Aplikasi Ujian Berbasis Daring", style="text-align: left;margin: 0;padding: 0;font-size: 124%;")
#                                          )
#                                      )
#                  )

server <- function(input, output, session){
  
    # auth <- callModule(
    #   module = auth_server,
    #   id = "auth",
    #   check_credentials = check_credentials("db/database.sqlite")
    # )
    # auth <- secure_server(
    #   check_credentials = check_credentials("db/database.sqlite", passphrase = "secret")
    # )
    
    # observeEvent(input$logout, {
    #   shinyalert(text = "Anda yakin ingin logout?", showCancelButton = TRUE, showConfirmButton = TRUE,
    #              closeOnEsc = FALSE, closeOnClickOutside = FALSE, confirmButtonText = "Ya", cancelButtonText = "Batal", 
    #              callbackR = function(x){
    #                if(isTRUE(x)){
    #                  dbDisconnect(mdbconn)
    #                  session$reload()
    #                }
    #              })
    # })
  
  kodejadwal <- reactive({
    getTable(db, "jadwal_ujian")$KodeJadwal
  })
    
    output$pengatur_jadwal <- renderUI({
      kode_jadwal_ujian <- kodejadwal()
      tagList(
        fluidRow(
          column(10,
               fluidRow(
                 column(6,
                      fluidRow(
                        selectInput(inputId = "kode_atur_jadwal_ujian", label = "Kode Jadwal", 
                                    choices = c(kode_jadwal_ujian, kode_jadwal_ujian + 1), selected = kode_jadwal_ujian + 1, width = "35%"),
                        selectInput(inputId = "hari_atur_jadwal_ujian", label = "Hari", 
                                    choices = c("Senin", "Selasa", "Rabu", "Kamis", "Jumat", "Sabtu", "Minggu"), width = "65%")
                        ),
                      fluidRow(
                        selectInput(inputId = "mapel_atur_jadwal_ujian", label = "Mata Pelajaran", 
                                    choices = getTable(db, "master_mapel")$NamaMaPel, width = "100%")
                        )
                      ),
               column(6,
                        fluidRow(
                          selectInput(inputId = "jam_mulai_atur_jadwal_ujian", label = "Jam Mulai", 
                                      choices = if_else(6:23 < 10, paste0("0", 6:23), paste0(6:23)), width = "50%"),
                          selectInput(inputId = "menit_mulai_atur_jadwal_ujian", label = "Menit Mulai", 
                                      choices = if_else(0:59 < 10, paste0("0", 0:59), paste0(0:59)), width = "50%")
                        ),
                      fluidRow(
                          selectInput(inputId = "jam_selesai_atur_jadwal_ujian", label = "Jam Selesai", 
                                      choices = if_else(6:23 < 10, paste0("0", 6:23), paste0(6:23)), width = "50%"),
                          selectInput(inputId = "menit_selesai_atur_jadwal_ujian", label = "Menit Selesai", 
                                      choices = if_else(0:59 < 10, paste0("0", 0:59), paste0(0:59)), width = "50%")
                        )
                      )
               )
               ),
        column(2,
               br(),
               # h6("Update/Tambah Jadwal", style="text-align: center;font-weight: bold;font-size: 90%;margin-top:8px;"),
               actionBttn(inputId = "update_jadwal_ujian", label = "Update", icon = icon("calendar-plus"), 
                            style = "simple", size = "sm", block = TRUE, color = "primary"),
               br(),
               # h6("Refresh Tabel Jadwal", style="text-align: center;font-weight: bold;font-size: 90%;margin-top:10px;"),
               actionBttn(inputId = "refresh_jadwal_ujian", label = "Refresh", icon = icon("history"), 
                          style = "simple", size = "sm", block = TRUE, color = "primary"),
               br(),
               actionBttn(inputId = "delete_jadwal_ujian", label = "Delete", icon = icon("calendar-times"), 
                          style = "simple", size = "sm", block = TRUE, color = "danger")
               )
        )
      )
    })
    
    observeEvent(input$delete_jadwal_ujian, {
      mdbcon <- dbConnect(MariaDB(),
                          host = db$host,
                          dbname = db$dbname,
                          port = db$port,
                          username = db$username, 
                          password = db$password)
      rq <- dbSendStatement(mdbcon, statement = sprintf("DELETE FROM jadwal_ujian WHERE KodeJadwal = %s;",
                                                          input$kode_atur_jadwal_ujian)
                            )
      dbClearResult(rq)
      dbDisconnect(mdbcon)
      shinyalert(text = "Data selesai dihapus")
    })
    
    observeEvent(input$update_jadwal_ujian, {
      tbl_jadwal_ujian <- getTable(db, "jadwal_ujian")
      tbl_master_mapel <- getTable(db, "master_mapel")
      kodemapeljadwal <- tbl_master_mapel$KodeMaPel[tbl_master_mapel$NamaMaPel == input$mapel_atur_jadwal_ujian]
      mdbcon <- dbConnect(MariaDB(),
                          host = db$host,
                          dbname = db$dbname,
                          port = db$port,
                          username = db$username, 
                          password = db$password)
      if(input$kode_atur_jadwal_ujian %in% tbl_jadwal_ujian$KodeJadwal){
        rq <- dbSendStatement(conn = mdbcon, 
                              statement = sprintf("UPDATE jadwal_ujian SET Hari = '%s', 
                                                                          JamMulai = %s, 
                                                                          MenitMulai = %s, 
                                                                          JamSelesai = %s, 
                                                                          MenitSelesai = %s, 
                                                                          KodeMaPel = %s WHERE KodeJadwal = %s;", 
                                                  input$hari_atur_jadwal_ujian, 
                                                  input$jam_mulai_atur_jadwal_ujian, 
                                                  input$menit_mulai_atur_jadwal_ujian, 
                                                  input$jam_selesai_atur_jadwal_ujian, 
                                                  input$menit_selesai_atur_jadwal_ujian, 
                                                  kodemapeljadwal,
                                                  input$kode_atur_jadwal_ujian))
      } else {
        rq <- dbSendStatement(mdbcon, statement = sprintf("INSERT INTO jadwal_ujian VALUES(%s, '%s', %s, %s, %s, %s, %s)",
                              input$kode_atur_jadwal_ujian, 
                              input$hari_atur_jadwal_ujian, 
                              as.numeric(input$jam_mulai_atur_jadwal_ujian), 
                              as.numeric(input$menit_mulai_atur_jadwal_ujian), 
                              as.numeric(input$jam_selesai_atur_jadwal_ujian), 
                              as.numeric(input$menit_selesai_atur_jadwal_ujian), 
                              kodemapeljadwal))
      }
      dbClearResult(rq)
      dbDisconnect(mdbcon)
      shinyalert(text = "Selesai meyimpan data")
    })
    
    jadwal_r <- eventReactive(input$refresh_jadwal_ujian, {
        getTable(db, "view_jadwal_ujian") %>% 
                    transmute(`Kode Jadwal` = KodeJadwal,
                              Hari = Hari, 
                              `Waktu Mulai` = paste(if_else(JamMulai < 10, 
                                                            paste0("0", JamMulai), 
                                                            paste0(JamMulai)), 
                                                    if_else(MenitMulai < 10, 
                                                            paste0("0", MenitMulai), 
                                                            paste0(MenitMulai)), sep = ":"),
                              `Waktu Selesai` = paste(if_else(JamSelesai < 10, 
                                                              paste0("0", JamSelesai), 
                                                              paste0(JamSelesai)), 
                                                      if_else(MenitSelesai < 10, 
                                                              paste0("0", MenitSelesai), 
                                                              paste0(MenitSelesai)), sep = ":"),
                              `Mata Pelajaran` = NamaMaPel) %>% 
                    arrange(`Waktu Mulai`)
    })
    
    output$jadwal <- DT::renderDataTable(rownames = FALSE, options = list(dom = 't'), {
      jadwal_r() %>% 
        filter(Hari == input$hariujian)
    })
    
    data_soal <- reactive({
      if(!is.null(input$data_upload_soal)){
        read_excel(input$data_upload_soal$datapath)
      } else {
        return(NULL)
      }
    })
    output$upload_soal_ui <- renderUI({
      if(!is.null(input$data_upload_soal)){
        data_soal <- data_soal()
        if(!hasTable(db, "soal")){
          mdbcon <- dbConnect(MariaDB(),
                              host = db$host,
                              dbname = db$dbname,
                              port = db$port,
                              username = db$username, 
                              password = db$password)
          rq <- dbSendStatement(mdbcon, statement = "CREATE TABLE soal AS SELECT * FROM template_soal;")
          dbClearResult(rq)
          dbDisconnect(mdbcon)
        }
        mapel <- getTable(db, "soal")
        kelas <- getTable(db, "master_kelas")
        kelas_soal <- data_soal %>% 
            distinct(KodeKelas, KodeSubKelas, KodeMaPel) %>% 
            # left_join(mapel, by = "KodeMaPel") %>% 
            left_join(kelas, by = c("KodeKelas", "KodeSubKelas")) %>% 
            transmute(Kelas = if_else(is.na(NamaSubKelas), paste(NamaKelas), paste(NamaKelas, NamaSubKelas)))
        
        
        output$prev_soal <- renderDataTable({
          datatable(data_soal %>% 
                      select(-KodeKelas, -KodeSubKelas, -KodeMaPel) %>% 
                      mutate(Soal = withMathJax(Soal),
                             A = withMathJax(A),
                             B = withMathJax(B),
                             C = withMathJax(C),
                             D = withMathJax(D),
                             E = withMathJax(E)
                      ), 
                    options = list(dom = 't'), rownames = FALSE)
        })
        
        tagList(
          h5(sprintf("Kelas: %s", kelas_soal$Kelas)),
          dataTableOutput("prev_soal"),
          br()
        )
      } else {
        return(NULL)
      }
    })
    
    # proxy <- dataTableProxy('jadwal')
    # observe({
    #   replaceData(proxy, jadwal_r(), rownames = FALSE)
    # })
    
    # session$onSessionEnded(function(){
    #     shinyalert(text = "Anda yakin ingin logout?", showCancelButton = TRUE, showConfirmButton = TRUE,
    #                closeOnEsc = FALSE, closeOnClickOutside = FALSE, confirmButtonText = "Ya", cancelButtonText = "Batal",
    #                callbackR = function(x){
    #                  if(isTRUE(x)){
    #                    dbDisconnect(mdbconn)
    #                    session$reload()
    #                  }
    #                })
    # })
  }

shinyApp(ui, server)
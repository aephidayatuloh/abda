# shiny-related package
library(shiny)
library(shinymanager)
library(shinyalert)
library(shinyjs)
library(shinythemes)
library(argonR)
library(argonDash)
library(shinyWidgets)
library(DT)

# database package
library(DBI)
library(RMariaDB)
library(dplyr)
# library(dbplyr)


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
  create_db(credentials, sqlite_path = "db/database.sqlite")
  dbDisconnect(mdbcon)
}
# cred(db)

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

instansi <- "SMK Kesehatan Dewantara"

set_labels(language = "en", 
           "Please authenticate" = "", 
           "Username:" = "NIS", 
           "Password:" = "Password"
           )

ui <- fluidPage(
  auth_ui(
    id = "auth", 
    # add image on top ?
    tags_top = tags$div(style = "display:flex;",
                        div(style="margin:0;padding:0;",
                            img( # image for instance
                              src = "img/big-logo.png",
                              # height = 70,
                              width = "80px",
                              style = "margin:0 0 0 0;"
                            )
                        ),
                        div(style="margin:0;padding-left:10px;",
                            h5(HTML(instansi), style = "font-weight:bold;text-align:center;margin-left:0;margin-bottom:0;margin-top:10px;padding:0;font-size:135%;"),
                            h5(HTML("Sistem Informsasi Pendidikan"), style = "text-align:center;margin-left:0;margin-bottom:3px;margin-top:3px;padding:0;font-size:120%;"),
                            h5(HTML("Aplikasi Ujian Online"), style = "text-align:center;margin:0;padding:0;font-size:120%;")
                            
                        )
    ),
    # change auth ui background ?
    # https://developer.mozilla.org/fr/docs/Web/CSS/background
    background  = "linear-gradient(rgba(0, 0, 255, 0.5),
                       rgba(255, 255, 0, 0.5));", 
    choose_language = FALSE
  ),
  shinyjs::useShinyjs(),
  shinyalert::useShinyalert(),
  withMathJax(),
  theme = shinytheme("paper"),
  tags$style('body{background-color:#ffffff;}'),
  title = instansi,
  h4("Sistem Informasi Pendidikan", style = "text-align:center;font-weight:bold;color: #03b1fc;"),
  div(id = "home",
      dataTableOutput("user"),
      h4("Pengumuman", style="font-wieght:bold;text-align: center;"),
      uiOutput("pengumuman")
  ),
  div(id = "profile_user",
      uiOutput("panduan_ui")
  ),
  uiOutput("soals"),
  
  uiOutput("fabbtn")
  ###############################################################################################
)



server <- function(input, output, session){
  
  ###############################################################################################
  # source("global2.R")
  
  # app/shiny/
  # dataloc = "data/soal.sqlite"
  
  getTable = function(tblnm)
  {
    mdbcon <- dbConnect(MariaDB(),
                        host = db$host,
                        dbname = db$dbname,
                        port = db$port,
                        username = db$username, 
                        password = db$password)
    tbl_dt <- dbReadTable(mdbcon, tblnm)
    dbDisconnect(mdbcon)
    return(tbl_dt)
  }
  credentials <- getTable("view_credentials")
  # message("Got credentials")
  data_soal <- getTable("view_soal")
  data_soal <- subset(data_soal, KodeMaPel == 1)
  # message("Got data_soal")
  kunci_jawaban <- data_soal$KunciJawaban
  # message("Got kunci_jawaban")
  ###############################################################################################
  
  # call the server part
  # check_credentials returns a function to authenticate users
  # Use secure_app()
  # auth <- secure_server(
  #   check_credentials = check_credentials(db = "db/database.sqlite")
  # )
  
  # authentication module
  # Use auth_ui()
  auth <- callModule(
    module = auth_server,
    id = "auth",
    # check_credentials = check_credentials(db = "db/database.sqlite")
    check_credentials = check_credentials(db = credentials)
  )
  
  observe({
    req(auth$user)
    output$fabbtn <- renderUI({
      fab_button(inputId = "fab", status = "primary", icon = icon("medrt"),
                 # actionButton(inputId = "logout", label = NULL, tooltip = "Logout", icon = icon("sign-out")),
                 # actionButton(inputId = "ujian", label = NULL, tooltip = "Ujian", icon = icon("clipboard-list")),
                 # # actionButton(inputId = "pengumuman", label = NULL, tooltip = "Pengumuman", icon = icon("envelope-open-text")),
                 # actionButton(inputId = "panduan", label = NULL, tooltip = "Panduan", icon = icon("book-reader"))
                 actionBttn(style = "material-circle", color = "primary", size = "xs", no_outline = TRUE, 
                            inputId = "logout", label = NULL, icon = icon("sign-out")),
                 actionBttn(style = "material-circle", color = "primary", size = "xs", no_outline = TRUE, 
                            inputId = "ujian", label = NULL, icon = icon("clipboard-list")),
                 actionBttn(style = "material-circle", color = "primary", size = "xs", no_outline = TRUE, 
                            inputId = "panduan", label = NULL, icon = icon("book-reader"))
      )
    })
  })
  
  observeEvent(input$panduan, {
    panduan <- ""
    shinyalert(imageUrl = "img/big-logo.png", imageWidth = 80, imageHeight = 78, animation = "slide-from-top",
               title = sprintf('<div style="margin:auto;text-align: center;">
                 <h5 style="font-weight:bold;text-align:center;margin-left:0;margin-bottom:0;margin-top:2px;padding:0;font-size:45%%;">%s</h5>
                 <h5 style="text-align:center;margin-left:0;margin-bottom:3px;margin-top:3px;padding:0;font-size:30%%;">Sistem Informsasi Pendidikan</h5>
                 <h5 style="text-align:center;margin:0;padding:0;font-size:30%%;">Aplikasi Ujian Online</h5>
                 </div>', instansi),
               text = panduan, 
               html = TRUE
    )
  })

  
  output$pengumuman <- renderUI({
    req(auth$user)
    pengumuman <- getTable("master_pengumuman") %>% 
      filter(KodeKelas == auth$user_info$kodekelas) %>% 
      # filter(KodeSubKelas == auth$user_info$kodesubkelas) %>% 
      filter(as.Date(TanggalMulai) <= Sys.Date() & Sys.Date() <= as.Date(TanggalSelesai))
    
    if(nrow(pengumuman) > 0){
      p(HTML(pengumuman$Pengumuman))
    } else {
      return(NULL)
    }
  })
  
  nosoal <- reactiveVal(0)
  observeEvent(input$ujian, {
    mdbcon <- dbConnect(MariaDB(),
                        host = db$host,
                        dbname = db$dbname,
                        port = db$port,
                        username = db$username, 
                        password = db$password)
    # tbl_dt <- dbReadTable(mdbcon, tblnm)
    # dbDisconnect(mdbcon)
    rq <- dbSendStatement(mdbcon, statement = sprintf("INSERT INTO `login_logs` (`NIS`, `LoginTime`) VALUES ('%s', '%s');", auth$user_info$user, Sys.time()))
    dbClearResult(rq)
    rq <- dbGetQuery(mdbcon, statement = sprintf("SELECT * FROM nilai WHERE NIS = '%s' AND KodeMaPel = %s;", auth$user_info$user, 1))
    if(nrow(rq) > 0){
      dbDisconnect(mdbcon)
      rm(mdbcon)
      shinyalert(text = "Anda sudah menyelesaikan ujian ini.", type = "error")
      
      showElement("ujian")
      # showElement("profile_user")
      # showElement("profile")
      showElement("logout")
      showElement("home")
    } else {
      dbDisconnect(mdbcon)
      rm(mdbcon)
      hideElement("ujian")
      hideElement("profile_user")
      hideElement("profile")
      hideElement("logout")
      hideElement("home")
      
      lapply(1:length(kunci_jawaban), function(i) {
        output[[paste0("soal",i)]] <- renderUI({
          # soal(i, input)
          tagList(
            h5(paste("Soal no.", i), style = "font-weight:bold;font-size:130%;"),
            p(withMathJax(HTML(data_soal$Soal[i]))),
            # p(MathJaxR(HTML(data_soal$Soal[i]))), 
            # hidden(f7Text("gunakan_gambar", label = NULL, value = data_soal$gunakan_gambar[i])),
            # hidden(f7Text("type_soal", label = NULL, value = data_soal$type_soal[i])),
            # plotOutput("pgimg"),
            if(data_soal$TypeSoal[i] == "pg"){
              if(!is.null(input[[paste0(data_soal$TypeSoal[i], i)]])){
                radioButtons(inputId = paste0(data_soal$TypeSoal[i],i), label = NULL,
                             choiceNames = list(paste(HTML(data_soal$A[i])),
                                                paste(HTML(data_soal$B[i])),
                                                paste(HTML(data_soal$C[i])),
                                                paste(HTML(data_soal$D[i])),
                                                paste(HTML(data_soal$E[i]))),
                             choiceValues = LETTERS[1:5],
                             selected = input[[paste0(data_soal$TypeSoal[i], i)]])
              } else {
                HTML(sprintf('
              <div id="%s" class="form-group shiny-input-radiogroup shiny-input-container">
              <label class="control-label shiny-label-null" for="%s"></label>
              <div class="shiny-options-group">
                <div class="radio">
                  <label>
                    <input type="radio" name="%s" value="A"/>
                    <span>%s</span>
                  </label>
                </div>
                <div class="radio">
                  <label>
                    <input type="radio" name="%s" value="B"/>
                    <span>%s</span>
                  </label>
                </div>
                <div class="radio">
                  <label>
                    <input type="radio" name="%s" value="C"/>
                    <span>%s</span>
                  </label>
                </div>
                <div class="radio">
                  <label>
                    <input type="radio" name="%s" value="D"/>
                    <span>%s</span>
                  </label>
                </div>
                <div class="radio">
                  <label>
                    <input type="radio" name="%s" value="E"/>
                    <span>%s</span>
                  </label>
                </div>
              </div>
            </div>
              ', 
                             paste0(data_soal$TypeSoal[i],i), 
                             paste0(data_soal$TypeSoal[i],i),
                             paste0(data_soal$TypeSoal[i],i),
                             paste(HTML(data_soal$A[i])), 
                             paste0(data_soal$TypeSoal[i],i),
                             paste(HTML(data_soal$B[i])),
                             paste0(data_soal$TypeSoal[i],i),
                             paste(HTML(data_soal$C[i])),
                             paste0(data_soal$TypeSoal[i],i),
                             paste(HTML(data_soal$D[i])),
                             paste0(data_soal$TypeSoal[i],i),
                             paste(HTML(data_soal$E[i]))
                )
                )
              }
            } else if(data_soal$TypeSoal[i] == "essay"){
              if(!is.null(input[[paste0(data_soal$TypeSoal[i], i)]])){
                textAreaInput(inputId = paste0(data_soal$TypeSoal[i],i), label = NULL, value = input[[paste0(data_soal$TypeSoal[i], i)]])
              } else {
                textAreaInput(inputId = paste0(data_soal$TypeSoal[i],i), label = NULL, placeholder = "Tuliskan jawaban disini")
              }
            }
          )
        })
      })
      
      output$fabbtn <- renderUI({
        fab_button(inputId = "fab", status = "primary", icon = icon("medrt"),
                   # actionButton(inputId = "submit", label = NULL, tooltip = "Submit", icon = icon("send")),
                   # actionButton(inputId = "prevs", label = NULL, tooltip = "Sebelumnya", icon = icon("chevron-circle-left")),
                   # actionButton(inputId = "nexts", label = NULL, tooltip = "Berikutnya", icon = icon("chevron-circle-right"))
                   actionBttn(style = "material-circle", color = "primary", size = "xs", no_outline = TRUE, 
                              inputId = "submit", label = NULL, icon = icon("send")),
                   actionBttn(style = "material-circle", color = "primary", size = "xs", no_outline = TRUE, 
                              inputId = "prevs", label = NULL, icon = icon("chevron-circle-left")),
                   actionBttn(style = "material-circle", color = "primary", size = "xs", no_outline = TRUE, 
                              inputId = "nexts", label = NULL, icon = icon("chevron-circle-right"))
        )
      })
      output$soals <- renderUI({
        nosoal(1)
        uiOutput(paste0("soal",as.numeric(nosoal())))
      })
    }
  })
  
  observeEvent(input$logout, {
    shinyalert(text = "Anda yakin ingin logout?", showCancelButton = TRUE, showConfirmButton = TRUE,
               closeOnEsc = FALSE, closeOnClickOutside = FALSE, 
               confirmButtonText = "Ya", cancelButtonText = "Batal",
               callbackR = function(x){
                 if(isTRUE(x)){
                   # dbDisconnect(mdbconn)
                   session$reload()
                 }
               })
  })
  
  observe({
    req(auth$user)
    shinyalert(text = sprintf("<div style'text-align:center;'>Sistem Informasi Pendidikan<br/>
                     %s<br/>
                     <br/>
                     <div style='font-size:180%%;'><strong>Selamat Datang</strong></div>
                     </div>
                     <br/>
                     
                     NIS: %s<br/>
                     Nama: %s", instansi, auth$user_info$user, auth$user_info$nama), html = TRUE)
  })
  
  output$user <- DT::renderDataTable({
    data.frame(NIS = as.character(auth$user_info$user), 
               Nama = as.character(auth$user_info$nama),
               Kelas = as.character(auth$user_info$namakelas)) %>% 
      datatable(rownames = FALSE, options = list(dom = "t",
                                                 columnDefs = list(list(className = 'dt-center', targets = 0),
                                                                   list(className = 'dt-center', targets = 1),
                                                                   list(className = 'dt-center', targets = 2)),
                                                 initComplete = JS(
                                                   "function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': '#03b1fc', 'color': '#fff'});",
                                                   "}")
                                                 ))
  })
  
  # observeEvent(input$ujian, {
  #   hideElement("home")
  #   hideElement("ujian")
  #   hideElement("logout")
  #   showElement("submit")
  #   # showElement("botbtn")
  #   nosoal(1)
  #   output$soals <- renderUI({
  #     uiOutput(paste0("soal",as.numeric(nosoal())))
  #   })
  # })
  
  observeEvent(input$prevs, {
    if(nosoal()-1 < 1){
      shinyalert(text = "Anda sudah mencapai pertanyaan pertama. Menampilkan pertanyaan terakhir.", 
                 callbackR = function(x){
                   if(x == TRUE){
                     nosoal(length(kunci_jawaban))
                   }
                 }
      )
    } else {
      nosoal(nosoal()-1)
    }
    output$soals <- renderUI({
      uiOutput(paste0("soal",as.numeric(nosoal())))
    })
  })
  
  observeEvent(input$nexts, {
    if(as.numeric(nosoal()+1) > length(kunci_jawaban)){
      shinyalert(text = "Anda sudah mencapai pertanyaan terakhir. Menampilkan pertanyaan pertama.", 
                 callbackR = function(x){
                   if(x == TRUE){
                     nosoal(1)
                   }
                 }
      )
      
    } else {
      nosoal(nosoal()+1)
    }
    output$soals <- renderUI({
      uiOutput(paste0("soal",as.numeric(nosoal())))
    })
  })
  observeEvent(input$submit, {
    # future({
    jwbn <- unlist(lapply(1:length(kunci_jawaban), function(i)ifelse(is.null(input[[paste0(data_soal$TypeSoal[i],i)]]), "", input[[paste0(data_soal$TypeSoal[i],i)]])))
    dijawab <- sum(jwbn != "")
    kosong <- length(kunci_jawaban) - dijawab
    
    
    shinyalert::shinyalert(title = "Anda yakin akan submit jawaban?", 
                           text = sprintf("Dijawab: %s | Tidak Dijawab: %s<br/><br/><strong>PERHATIAN!</strong><br/>Jawaban Anda akan disimpan dan dinilai, kemudian ujian ini akan ditutup dan selesai. Tidak dapat diulang atau diperbaiki.", dijawab, kosong), 
                           html = TRUE,
                           closeOnClickOutside = FALSE, showCancelButton = TRUE, confirmButtonText = "Lanjut", cancelButtonText = "Batal", 
                           callbackR = function(x){
                             if(x == TRUE){
                               
                               showModal(modalDialog("Tunggu sebentar...\nData Anda sedang diproses", title = NULL, easyClose = FALSE, footer = NULL, size = "l"))
                               # shinyalert(text = "Tunggu sebentar...<br/>Data Anda sedang diproses", 
                               #            html = TRUE, closeOnClickOutside = FALSE, 
                               #            showCancelButton = FALSE, showConfirmButton = FALSE, closeOnEsc = FALSE)
                               benar <- kunci_jawaban[data_soal$TypeSoal == "pg"] == jwbn[data_soal$TypeSoal == "pg"]
                               salah <- sum(jwbn[data_soal$TypeSoal == "pg"] != "" & kunci_jawaban[data_soal$TypeSoal == "pg"] != jwbn[data_soal$TypeSoal == "pg"])
                               nilai <- data_soal$BobotNilai[data_soal$TypeSoal == "pg"]
                               nilai <- nilai[benar]
                               nilai <- sum(nilai)
                               # future({
                               x <- data.frame(NIS = auth$user_info$user, KodeMapel = 1, 
                                               NoSoal = 1:length(kunci_jawaban),
                                               Jawaban = jwbn, TanggalSubmit = Sys.time(), stringsAsFactors = FALSE)
                               
                               # dbcon <- dbConnect(RSQLite::SQLite(), dataloc)
                               dbcon <- dbConnect(MariaDB(), 
                                                  host = db$host,
                                                  dbname = db$dbname, 
                                                  port = db$port,
                                                  username = db$user, 
                                                  password = db$password)
                               rc <- dbSendStatement(conn = dbcon, statement = sprintf("DELETE FROM jawaban_ujian WHERE NIS = '%s' AND KodeMaPel = %s;", auth$user_info$user, 1))
                               dbClearResult(rc)
                               if(sprintf("tmp_jwbn_%s", auth$user_info$user) %in% dbListTables(dbcon))dbRemoveTable(dbcon, sprintf("tmp_jwbn_%s", auth$user_info$user))
                               dbWriteTable(dbcon, name = sprintf("tmp_jwbn_%s", auth$user_info$user), value = x)
                               rc <- dbSendStatement(conn = dbcon, statement = sprintf("INSERT INTO jawaban_ujian SELECT * FROM tmp_jwbn_%s;", auth$user_info$user))
                               dbClearResult(rc)
                               if(sprintf("tmp_jwbn_%s", auth$user_info$user) %in% dbListTables(dbcon)){
                                 dbRemoveTable(dbcon, sprintf("tmp_jwbn_%s", auth$user_info$user))
                               }
                               rq <- dbSendStatement(dbcon, statement = sprintf("INSERT INTO nilai VALUES ('%s', %s, %s);", auth$user_info$user, 1, nilai))
                               dbClearResult(rq)
                               dbDisconnect(dbcon)
                               rm(dbcon)
                               # })
                               
                               shinyalert(title = NULL,
                                          text = sprintf("<div style='text-align:center;'>Ujian Tengah Semester I<br/>
                                                         %s<br/>
                                                         Tahun Ajaran 2019/2020<br/>
                                                         <br/>
                                                         Nama:<br/>%s<br/>
                                                         NIS:<br/>%s<br/>
                                                         <br/>
                                                         <div style='font-weight:bold;'>Jawaban:<br/>
                                                         <span style='color:#5ad668'>Benar: %s</span> | 
                                                         <span style='color:#db6153'>Salah: %s</span> | 
                                                         <span style='color:#f2df63'>Kosong: %s</span><br/>
                                                         <div style='font-weight:bold;'>Nilai: %s</div>
                                                         </div>
                                                         </div>", instansi, auth$user_info$nama, 
                                                         auth$user_info$user, sum(benar), salah, kosong, nilai), html = TRUE, closeOnEsc = FALSE, 
                                          closeOnClickOutside = FALSE, showConfirmButton = TRUE, showCancelButton = FALSE, 
                                          confirmButtonText = "Selesai & Logout", cancelButtonText = "Batal", 
                                          callbackR = function(x){
                                            if(x == TRUE){
                                              # showNotification("Tunggu sebentar...\nData Anda sedang diproses", closeButton = FALSE, type = "message")
                                              session$reload()
                                            }
                                          })
                             }
                           })
    # })
  })
  
}

shinyApp(ui, server)

# APLIKASI ANALISIS STUDENT LIFESTYLE MENGGUNAKAN R SHINY
# Nama: Honai Mony
# NIM: (isi NIM jika ingin ditampilkan)
# ============================================================
# BAGIAN 0: PACKAGE DAN PERSIAPAN DATA
# ============================================================

pkgs <- c("shiny", "bslib", "readxl", "plotly", "DT", "magrittr", "nortest", "lmtest")

miss <- setdiff(pkgs, rownames(installed.packages()))
if (length(miss) > 0) install.packages(miss)

invisible(lapply(pkgs, library, character.only = TRUE))

# Membaca dataset
kandidat <- c(
  "student_lifestyle.xlsx",
  file.path("data", "student_lifestyle.xlsx")
)

file_data <- kandidat[file.exists(kandidat)][1]

if (length(file_data) == 0 || is.na(file_data)) {
  file_data <- file.choose()
}

raw <- as.data.frame(read_excel(file_data))
names(raw) <- trimws(names(raw))

# Nama variabel sesuai dataset Student Lifestyle
num_vars <- c(
  "Study_Hours_Per_Day",
  "Extracurricular_Hours_Per_Day",
  "Sleep_Hours_Per_Day",
  "Social_Hours_Per_Day",
  "Physical_Activity_Hours_Per_Day",
  "GPA"
)

cat_vars <- c("Stress_Level")

# Memeriksa kelengkapan kolom
kolom_wajib <- c(num_vars, cat_vars)

if (!all(kolom_wajib %in% names(raw))) {
  stop(
    paste(
      "Kolom berikut tidak ditemukan:",
      paste(setdiff(kolom_wajib, names(raw)), collapse = ", ")
    )
  )
}

# Mengambil variabel yang digunakan
df <- raw[, kolom_wajib]

# Mengubah variabel numerik
for (v in num_vars) {
  df[[v]] <- suppressWarnings(
    as.numeric(sub(",", ".", as.character(df[[v]])))
  )
}

# Mengubah variabel kategorik
df$Stress_Level <- factor(
  trimws(as.character(df$Stress_Level))
)

# Menghapus data yang tidak lengkap
n_missing <- sum(is.na(df))
df <- df[complete.cases(df), ]

if (nrow(df) < 10) {
  stop("Jumlah data lengkap terlalu sedikit untuk dianalisis.")
}

# Label variabel
label_vars <- c(
  Study_Hours_Per_Day = "Jam Belajar per Hari",
  Extracurricular_Hours_Per_Day = "Jam Kegiatan Ekstrakurikuler",
  Sleep_Hours_Per_Day = "Jam Tidur per Hari",
  Social_Hours_Per_Day = "Jam Bersosialisasi per Hari",
  Physical_Activity_Hours_Per_Day = "Jam Aktivitas Fisik per Hari",
  GPA = "Grade Point Average (GPA)",
  Stress_Level = "Tingkat Stres"
)

lab <- function(x) {
  hasil <- label_vars[x]
  ifelse(is.na(hasil), x, unname(hasil))
}

nama_mahasiswa <- "Honai Mony"

# ============================================================
# BAGIAN 1: WARNA DAN FUNGSI BANTU
# ============================================================

bg <- "#0B0F1A"
surface <- "#131A2B"
edge <- "#26314D"
grid_col <- "#2A3553"

ink <- "#E6EDF7"
muted <- "#8A97B2"

mint <- "#34E0B5"
pink <- "#FF5C8A"
amber <- "#FFC857"
violet <- "#8B7CFF"
sky <- "#4CC9F0"

cols <- c(mint, pink, amber, violet, sky)

# Mendukung input satu angka maupun vektor (untuk tabel)
fmt <- function(x, d = 2) {
  if (length(x) == 0) return(character(0))

  hasil <- format(
    round(x, d),
    big.mark = ".",
    decimal.mark = ",",
    nsmall = d,
    trim = TRUE
  )

  hasil[is.na(x)] <- "-"
  hasil
}

pv <- function(p) {
  if (is.na(p)) return("-")
  if (p < 0.0001) return("< 0,0001")
  fmt(p, 4)
}

rgba <- function(hex, a = 0.5) {
  r <- col2rgb(hex)
  sprintf(
    "rgba(%d,%d,%d,%.2f)",
    r[1], r[2], r[3], a
  )
}

interp <- function(...) {
  div(
    class = "interp",
    strong("Interpretasi: "),
    ...
  )
}

kpi <- function(label, value, color) {
  div(
    class = "kpi",
    style = paste0("--accent:", color),
    div(class = "kpi-label", label),
    div(class = "kpi-value", value)
  )
}

kekuatan <- function(r) {
  a <- abs(r)
  
  if (a < 0.2) "sangat lemah"
  else if (a < 0.4) "lemah"
  else if (a < 0.6) "sedang"
  else if (a < 0.8) "kuat"
  else "sangat kuat"
}

skew <- function(x) {
  m <- mean(x)
  mean((x - m)^3) / mean((x - m)^2)^1.5
}

kurt <- function(x) {
  m <- mean(x)
  mean((x - m)^4) / mean((x - m)^2)^2
}

fence <- function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  
  c(
    bawah = q[1] - 1.5 * iqr,
    atas = q[2] + 1.5 * iqr
  )
}

ringkas_num <- function(x) {
  q <- quantile(x, c(0.25, 0.50, 0.75), na.rm = TRUE)
  
  data.frame(
    Ukuran = c(
      "Jumlah data",
      "Mean",
      "Median",
      "Varians",
      "Standar deviasi",
      "Minimum",
      "Maksimum",
      "Range",
      "Q1",
      "Q2",
      "Q3",
      "Skewness",
      "Kurtosis"
    ),
    Nilai = c(
      fmt(length(x), 0),
      fmt(mean(x)),
      fmt(median(x)),
      fmt(var(x)),
      fmt(sd(x)),
      fmt(min(x)),
      fmt(max(x)),
      fmt(diff(range(x))),
      fmt(q[1]),
      fmt(q[2]),
      fmt(q[3]),
      fmt(skew(x)),
      fmt(kurt(x))
    )
  )
}

base_plotly <- function(p) {
  p %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = ink, size = 12),
      legend = list(bgcolor = "rgba(0,0,0,0)"),
      margin = list(t = 30, r = 15)
    ) %>%
    config(displaylogo = FALSE)
}

style_plotly <- function(p) {
  base_plotly(p) %>%
    layout(
      xaxis = list(gridcolor = grid_col),
      yaxis = list(gridcolor = grid_col)
    )
}

# ============================================================
# BAGIAN 2: TEMA DAN CSS
# ============================================================

app_theme <- bs_theme(
  version = 5,
  bg = bg,
  fg = ink,
  primary = mint,
  secondary = violet,
  success = mint,
  info = sky,
  warning = amber,
  danger = pink,
  base_font = font_collection(
    "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"
  )
)

css <- paste0(
  "
  body {
    background: radial-gradient(
      1200px 600px at 85% -10%,
      #1b2345 0%, ", bg, " 60%
    ) fixed;
  }

  .navbar {
    background:", surface, " !important;
    border-bottom:1px solid ", edge, ";
  }

  .navbar-brand {
    font-weight:800;
    color:", mint, " !important;
  }

  .navbar .nav-link {
    color:", ink, " !important;
    font-weight:600;
  }

  .navbar .nav-link.active {
    color:", mint, " !important;
    border-bottom:2px solid ", mint, ";
  }

  .bslib-sidebar-layout > .sidebar {
    background:", surface, " !important;
    border-right:1px solid ", edge, ";
  }

  .card {
    background:", surface, " !important;
    border:1px solid ", edge, " !important;
    border-radius:16px;
  }

  .card-header {
    background:transparent !important;
    border-bottom:1px solid ", edge, ";
    font-weight:700;
    color:", ink, ";
  }

  .kpi {
    background:", surface, ";
    border:1px solid ", edge, ";
    border-radius:16px;
    padding:16px 20px;
    position:relative;
  }

  .kpi:before {
    content:'';
    position:absolute;
    left:0;
    top:0;
    bottom:0;
    width:5px;
    background:var(--accent);
  }

  .kpi-label {
    font-size:.72rem;
    color:", muted, ";
    text-transform:uppercase;
  }

  .kpi-value {
    font-size:1.6rem;
    font-weight:800;
    color:var(--accent);
  }

  .note {
    color:", muted, ";
    font-size:.85rem;
  }

  .section-title {
    font-weight:800;
    color:", mint, ";
  }

  .table {
    --bs-table-bg:transparent;
    --bs-table-color:", ink, ";
  }

  .interp {
    background:", bg, ";
    border-left:4px solid ", mint, ";
    border-radius:10px;
    padding:12px;
    margin-top:10px;
    color:", ink, ";
  }

  .hero {
    background:linear-gradient(
      120deg,#2b2d6e,#5b2a86 60%,#a5326b
    );
    border-radius:18px;
    padding:22px 28px;
    margin-bottom:14px;
  }

  .hero h2 {
    color:white;
    font-weight:800;
  }

  .hero p {
    color:#e9e3ff;
  }

  .author {
    display:inline-block;
    background:linear-gradient(90deg,", mint, ",", sky, ");
    color:#07130f;
    font-weight:700;
    border-radius:999px;
    padding:5px 12px;
    font-size:.8rem;
  }

  pre {
    background:", bg, " !important;
    color:", ink, " !important;
  }
  "
)

# ============================================================
# BAGIAN 3: USER INTERFACE
# ============================================================

ui <- page_navbar(
  title = "STUDENT LIFESTYLE ANALYTICS",
  id = "nav",
  theme = app_theme,
  fillable = FALSE,
  
  header = tags$head(
    tags$style(HTML(css))
  ),
  
  sidebar = sidebar(
    width = 280,
    open = "desktop",
    
    h5("FILTER DATA", class = "section-title"),
    
    checkboxGroupInput(
      "f_stress",
      "Tingkat Stres",
      choices = levels(df$Stress_Level),
      selected = levels(df$Stress_Level)
    ),
    
    sliderInput(
      "f_gpa",
      "Rentang GPA",
      min = min(df$GPA),
      max = max(df$GPA),
      value = range(df$GPA)
    ),
    
    actionButton(
      "f_reset",
      "Reset Filter",
      class = "btn-primary w-100"
    ),
    
    hr(),
    
    div(
      class = "note",
      textOutput("f_info")
    ),
    
    hr(),
    
    div(class = "note", "Dikerjakan oleh"),
    span(class = "author", nama_mahasiswa)
  ),
  
  # ----------------------------------------------------------
  # 1. BERANDA
  # ----------------------------------------------------------
  
  nav_panel(
    "Beranda",
    
    div(
      class = "hero",
      h2("Analisis Student Lifestyle"),
      p(
        "Eksplorasi Data dan Regresi Linear Berganda",
        br(),
        "Program Studi Statistika"
      )
    ),
    
    uiOutput("kpis"),
    
    br(),
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header("Latar Belakang"),
        
        p(
          "Gaya hidup mahasiswa merupakan salah satu aspek ",
          "yang dapat berkaitan dengan aktivitas akademik. ",
          "Kegiatan belajar, tidur, aktivitas fisik, kegiatan ",
          "ekstrakurikuler, dan interaksi sosial merupakan ",
          "beberapa faktor yang dapat diamati dalam kehidupan ",
          "sehari-hari mahasiswa."
        ),
        
        p(
          "Melalui analisis data Student Lifestyle, penelitian ",
          "ini berusaha menggambarkan karakteristik gaya hidup ",
          "mahasiswa dan mengkaji hubungannya dengan prestasi ",
          "akademik yang diukur menggunakan GPA."
        )
      ),
      
      card(
        card_header("Tujuan Analisis"),
        
        tags$ol(
          tags$li("Menggambarkan karakteristik setiap variabel."),
          tags$li("Mengetahui hubungan antarvariabel."),
          tags$li("Mengamati pola data secara multivariat."),
          tags$li("Membangun model regresi linear berganda."),
          tags$li("Melakukan pemeriksaan asumsi model regresi.")
        )
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header("Variabel Penelitian"),
        
        tableOutput("tbl_def")
      ),
      
      card(
        card_header("Informasi Dataset"),
        
        tableOutput("tbl_info")
      )
    )
  ),
  
  # ----------------------------------------------------------
  # 2. UNIVARIAT
  # ----------------------------------------------------------
  
  nav_panel(
    "Univariat",
    
    navset_card_tab(
      
      nav_panel(
        "Variabel Numerik",
        
        layout_columns(
          col_widths = c(4, 8),
          
          card(
            card_header("Pengaturan Grafik"),
            
            selectInput(
              "un_var",
              "Pilih Variabel",
              choices = setNames(num_vars, lab(num_vars))
            ),
            
            selectInput(
              "un_type",
              "Jenis Grafik",
              choices = c(
                "Histogram",
                "Density Plot",
                "Boxplot"
              )
            ),
            
            sliderInput(
              "un_bins",
              "Jumlah Bin Histogram",
              min = 5,
              max = 40,
              value = 15
            )
          ),
          
          card(
            card_header("Visualisasi Distribusi"),
            
            plotlyOutput("un_plot", height = "350px")
          )
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          
          card(
            card_header("Statistik Deskriptif"),
            
            tableOutput("un_tbl")
          ),
          
          card(
            card_header("Interpretasi"),
            
            uiOutput("un_interp")
          )
        )
      ),
      
      nav_panel(
        "Variabel Kategorik",
        
        card(
          card_header("Distribusi Tingkat Stres"),
          
          layout_columns(
            col_widths = c(6, 6),
            
            plotlyOutput("cat_bar", height = "350px"),
            
            plotlyOutput("cat_pie", height = "350px")
          )
        ),
        card(
          card_header("Diagram Pareto Tingkat Stres"),
          plotlyOutput("cat_pareto", height = "350px")
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          
          card(
            card_header("Tabel Frekuensi"),
            
            tableOutput("cat_tbl")
          ),
          
          card(
            card_header("Interpretasi"),
            
            uiOutput("cat_interp")
          )
        )
      )
    )
  ),
  
  # ----------------------------------------------------------
  # 3. BIVARIAT
  # ----------------------------------------------------------
  
  nav_panel(
    "Bivariat",
    
    navset_card_tab(
      
      nav_panel(
        "Scatter Plot dan Korelasi",
        
        card(
          card_header("Pilih Pasangan Variabel"),
          
          layout_columns(
            col_widths = c(6, 6),
            
            selectInput(
              "bi_x",
              "Variabel X",
              choices = setNames(num_vars, lab(num_vars)),
              selected = "Study_Hours_Per_Day"
            ),
            
            selectInput(
              "bi_y",
              "Variabel Y",
              choices = setNames(num_vars, lab(num_vars)),
              selected = "GPA"
            )
          )
        ),
        
        card(
          card_header("Scatter Plot"),
          
          plotlyOutput("bi_scatter", height = "400px"),
          
          uiOutput("bi_interp")
        ),
        
        card(
          card_header("Matriks Korelasi"),
          
          radioButtons(
            "cor_method",
            "Metode Korelasi",
            choices = c(
              "Pearson" = "pearson",
              "Spearman" = "spearman"
            ),
            inline = TRUE
          ),
          
          plotlyOutput("bi_heat", height = "400px"),
          
          tableOutput("bi_cor_tbl")
        )
      ),
      
      nav_panel(
        "Box Plot",
        
        card(
          card_header("Perbandingan GPA Berdasarkan Tingkat Stres"),
          
          plotlyOutput("bi_box", height = "400px"),
          
          uiOutput("bi_box_interp")
        )
      )
    )
  ),
  
  # ----------------------------------------------------------
  # 4. MULTIVARIAT
  # ----------------------------------------------------------
  
  nav_panel(
    "Multivariat",
    
    navset_card_tab(
      
      nav_panel(
        "PCA",
        
        card(
          card_header("Principal Component Analysis"),
          
          plotlyOutput("mv_pca", height = "450px"),
          
          uiOutput("mv_pca_interp")
        )
      ),
      
      nav_panel(
        "Scatter Matrix",
        
        card(
          card_header("Matriks Sebar Variabel Numerik"),
          
          plotlyOutput("mv_splom", height = "550px")
        )
      ),
      nav_panel(
        "Outlier & Normalitas Multivariat",
        card(
          card_header("Deteksi Outlier dengan Jarak Mahalanobis"),
          p(class = "note", "Observasi ditandai sebagai kandidat outlier jika jarak Mahalanobis melebihi kuantil Chi-Square 95% dengan derajat bebas sebanyak variabel numerik."),
          plotlyOutput("mv_mahal", height = "400px"),
          tableOutput("mv_outlier_tbl")
        ),
        card(
          card_header("Q-Q Plot Normalitas Multivariat"),
          plotlyOutput("mv_qq", height = "400px")
        )
      )
    )
  ),
  
  # ----------------------------------------------------------
  # 5. REGRESI LINEAR BERGANDA
  # ----------------------------------------------------------
  
  nav_panel(
    "Regresi Linear Berganda",
    
    layout_columns(
      col_widths = c(4, 8),
      
      card(
        card_header("Pengaturan Model"),
        
        selectInput(
          "reg_y",
          "Variabel Respons (Y)",
          choices = setNames("GPA", "GPA"),
          selected = "GPA"
        ),
        
        checkboxGroupInput(
          "reg_x",
          "Variabel Prediktor (X)",
          choices = setNames(
            setdiff(num_vars, "GPA"),
            lab(setdiff(num_vars, "GPA"))
          ),
          selected = setdiff(num_vars, "GPA")
        ),
        
        checkboxInput(
          "reg_stress",
          "Tambahkan Tingkat Stres",
          value = TRUE
        ),
        
        p(
          class = "note",
          "Model menggunakan GPA sebagai variabel respons. ",
          "Prediktor dapat dipilih sesuai kebutuhan analisis."
        )
      ),
      
      card(
        card_header("Persamaan dan Ringkasan Model"),
        
        verbatimTextOutput("reg_formula"),
        
        uiOutput("reg_kpis")
      )
    ),
    
    card(
      card_header("Estimasi Koefisien dan Pengujian"),
      
      tableOutput("reg_coef"),
      
      uiOutput("reg_interp")
    ),
    
    card(
      card_header("Pengujian Asumsi Model"),
      
      tableOutput("reg_assumption"),
      
      tableOutput("reg_vif")
    ),
    
    navset_card_tab(
      
      nav_panel(
        "Aktual vs Prediksi",
        
        plotlyOutput("reg_actual", height = "400px")
      ),
      
      nav_panel(
        "Residual vs Fitted",
        
        plotlyOutput("reg_residual", height = "400px")
      ),
      
      nav_panel(
        "Q-Q Plot Residual",
        
        plotlyOutput("reg_qq", height = "400px")
      ),
      
      nav_panel(
        "Cook's Distance",
        
        plotlyOutput("reg_cook", height = "400px")
      )
    ),
    
    card(
      card_header("Keterbatasan Model"),
      
      tags$ul(
        tags$li(
          "Model regresi hanya menggambarkan hubungan statistik ",
          "dan tidak secara otomatis membuktikan hubungan sebab-akibat."
        ),
        
        tags$li(
          "Hasil model bergantung pada kualitas dan kelengkapan data."
        ),
        
        tags$li(
          "Hubungan antarvariabel diasumsikan linear."
        ),
        
        tags$li(
          "Multikolinearitas dapat memengaruhi kestabilan koefisien."
        ),
        
        tags$li(
          "Variabel lain di luar dataset mungkin juga berhubungan ",
          "dengan prestasi akademik mahasiswa."
        )
      )
    )
  ),
  
  # ----------------------------------------------------------
  # 6. KESIMPULAN
  # ----------------------------------------------------------
  
  nav_panel(
    "Kesimpulan",
    
    card(
      card_header("Kesimpulan dan Rekomendasi"),
      
      uiOutput("kesimpulan")
    )
  ),
  
  # ----------------------------------------------------------
  # 7. DATA
  # ----------------------------------------------------------
  
  nav_panel(
    "Data",
    
    card(
      card_header("Tabel Dataset"),
      
      DTOutput("tbl_data"),
      
      br(),
      
      downloadButton(
        "download_data",
        "Unduh Data CSV",
        class = "btn-primary"
      )
    )
  )
)

# ============================================================
# BAGIAN 4: SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # DATA FILTER
  # ----------------------------------------------------------
  
  filtered <- reactive({
    
    req(input$f_stress, input$f_gpa)
    
    df[
      df$Stress_Level %in% input$f_stress &
        df$GPA >= input$f_gpa[1] &
        df$GPA <= input$f_gpa[2],
    ]
  })
  
  dat <- reactive({
    
    d <- filtered()
    
    validate(
      need(
        nrow(d) >= 10,
        "Data terlalu sedikit. Silakan longgarkan filter."
      )
    )
    
    d$Stress_Level <- droplevels(d$Stress_Level)
    
    d
  })
  
  output$f_info <- renderText({
    
    paste(
      nrow(filtered()),
      "dari",
      nrow(df),
      "observasi terpilih"
    )
    
  })
  
  observeEvent(input$f_reset, {
    
    updateCheckboxGroupInput(
      session,
      "f_stress",
      selected = levels(df$Stress_Level)
    )
    
    updateSliderInput(
      session,
      "f_gpa",
      value = range(df$GPA)
    )
    
  })
  
  # ----------------------------------------------------------
  # BERANDA
  # ----------------------------------------------------------
  
  output$kpis <- renderUI({
    
    d <- dat()
    
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      
      kpi(
        "Jumlah Observasi",
        fmt(nrow(d), 0),
        mint
      ),
      
      kpi(
        "Rata-rata GPA",
        fmt(mean(d$GPA), 2),
        sky
      ),
      
      kpi(
        "Rata-rata Jam Belajar",
        paste(fmt(mean(d$Study_Hours_Per_Day), 2), "jam"),
        amber
      ),
      
      kpi(
        "Variabel Analisis",
        "6 numerik + 1 kategorik",
        violet
      )
    )
    
  })
  
  output$tbl_def <- renderTable({
    
    data.frame(
      Variabel = c(
        "Study_Hours_Per_Day",
        "Extracurricular_Hours_Per_Day",
        "Sleep_Hours_Per_Day",
        "Social_Hours_Per_Day",
        "Physical_Activity_Hours_Per_Day",
        "GPA",
        "Stress_Level"
      ),
      
      Tipe = c(
        rep("Numerik", 6),
        "Kategorik"
      ),
      
      Definisi = c(
        "Jumlah jam belajar per hari",
        "Jumlah jam kegiatan ekstrakurikuler per hari",
        "Jumlah jam tidur per hari",
        "Jumlah jam bersosialisasi per hari",
        "Jumlah jam aktivitas fisik per hari",
        "Indeks prestasi akademik",
        "Kategori tingkat stres mahasiswa"
      )
    )
    
  })
  
  output$tbl_info <- renderTable({
    
    data.frame(
      Informasi = c(
        "Jumlah observasi awal",
        "Jumlah observasi setelah pembersihan",
        "Jumlah variabel numerik",
        "Jumlah variabel kategorik",
        "Jumlah data hilang awal"
      ),
      
      Nilai = c(
        nrow(raw),
        nrow(df),
        length(num_vars),
        length(cat_vars),
        n_missing
      )
    )
    
  })
  
  # ----------------------------------------------------------
  # UNIVARIAT NUMERIK
  # ----------------------------------------------------------
  
  output$un_plot <- renderPlotly({
    
    x <- dat()[[input$un_var]]
    
    v <- lab(input$un_var)
    
    if (input$un_type == "Histogram") {
      
      plot_ly(
        x = x,
        type = "histogram",
        nbinsx = input$un_bins,
        marker = list(
          color = mint,
          line = list(color = bg, width = 1)
        )
      ) %>%
        style_plotly() %>%
        layout(
          xaxis = list(title = v),
          yaxis = list(title = "Frekuensi")
        )
      
    } else if (input$un_type == "Density Plot") {
      
      validate(
        need(length(unique(x)) > 1, "Data tidak memiliki variasi.")
      )
      
      dn <- density(x)
      
      plot_ly(
        x = dn$x,
        y = dn$y,
        type = "scatter",
        mode = "lines",
        fill = "tozeroy",
        line = list(color = sky, width = 2),
        fillcolor = rgba(sky, 0.3)
      ) %>%
        style_plotly() %>%
        layout(
          xaxis = list(title = v),
          yaxis = list(title = "Densitas")
        )
      
    } else {
      
      plot_ly(
        y = x,
        type = "box",
        boxpoints = "outliers",
        marker = list(color = amber),
        line = list(color = amber)
      ) %>%
        style_plotly() %>%
        layout(
          yaxis = list(title = v)
        )
      
    }
    
  })
  
  output$un_tbl <- renderTable({
    
    ringkas_num(dat()[[input$un_var]])
    
  })
  
  output$un_interp <- renderUI({
    
    x <- dat()[[input$un_var]]
    
    f <- fence(x)
    
    jumlah_outlier <- sum(x < f[1] | x > f[2])
    
    sw <- if (length(x) >= 3 && length(x) <= 5000 && length(unique(x)) > 1) {
      shapiro.test(x)$p.value
    } else {
      NA_real_
    }
    
    interp(
      paste0(
        "Rata-rata sebesar ", fmt(mean(x)),
        " dan median sebesar ", fmt(median(x)),
        ". Nilai skewness sebesar ", fmt(skew(x)),
        " menunjukkan distribusi ",
        if (abs(skew(x)) < 0.5) "relatif simetris."
        else if (skew(x) > 0) "miring ke kanan."
        else "miring ke kiri.",
        " Jumlah kandidat pencilan berdasarkan IQR sebanyak ",
        jumlah_outlier,
        " observasi. Shapiro-Wilk p-value = ",
        pv(sw),
        if (!is.na(sw)) {
          if (sw > 0.05) ", sehingga tidak terdapat bukti yang cukup untuk menolak normalitas."
          else ", sehingga terdapat bukti penyimpangan dari normalitas."
        } else {
          ". Uji Shapiro-Wilk tidak dilakukan."
        }
      )
    )
    
  })
  
  # ----------------------------------------------------------
  # UNIVARIAT KATEGORIK
  # ----------------------------------------------------------
  
  cat_freq <- reactive({
    
    t <- table(dat()$Stress_Level)
    
    hasil <- data.frame(
      Kategori = names(t),
      Frekuensi = as.integer(t)
    )
    
    hasil$Persentase <- 100 * hasil$Frekuensi / sum(hasil$Frekuensi)
    
    hasil
    
  })
  
  output$cat_bar <- renderPlotly({
    
    t <- cat_freq()
    
    plot_ly(
      t,
      x = ~Kategori,
      y = ~Frekuensi,
      type = "bar",
      marker = list(color = cols)
    ) %>%
      style_plotly() %>%
      layout(
        xaxis = list(title = "Tingkat Stres"),
        yaxis = list(title = "Frekuensi")
      )
    
  })
  
  output$cat_pie <- renderPlotly({
    
    t <- cat_freq()
    
    plot_ly(
      t,
      labels = ~Kategori,
      values = ~Frekuensi,
      type = "pie",
      hole = 0.4
    ) %>%
      base_plotly()
    
  })
  
  output$cat_pareto <- renderPlotly({
    t <- cat_freq()
    t <- t[order(-t$Frekuensi), , drop = FALSE]
    t$Kategori <- factor(t$Kategori, levels = t$Kategori)
    t$Kumulatif <- cumsum(t$Frekuensi) / sum(t$Frekuensi) * 100

    plot_ly(t, x = ~Kategori) %>%
      add_bars(y = ~Frekuensi, name = "Frekuensi", marker = list(color = sky)) %>%
      add_lines(y = ~Kumulatif, name = "Persentase kumulatif", yaxis = "y2",
                line = list(color = amber, width = 3), mode = "lines+markers") %>%
      layout(
        yaxis = list(title = "Frekuensi", gridcolor = grid_col),
        yaxis2 = list(title = "Kumulatif (%)", overlaying = "y", side = "right",
                      range = c(0, 100), ticksuffix = "%"),
        xaxis = list(title = "Kategori stres"),
        legend = list(orientation = "h", y = -0.2)
      ) %>% base_plotly()
  })

  output$cat_tbl <- renderTable({
    
    t <- cat_freq()
    
    t$Persentase <- fmt(t$Persentase, 2)
    
    t
    
  })
  
  output$cat_interp <- renderUI({
    
    t <- cat_freq()
    
    i <- which.max(t$Frekuensi)
    
    interp(
      paste0(
        "Kategori tingkat stres terbanyak adalah ",
        t$Kategori[i],
        " dengan jumlah ",
        t$Frekuensi[i],
        " mahasiswa atau ",
        fmt(t$Persentase[i], 2),
        "% dari data yang sedang dianalisis."
      )
    )
    
  })
  
  # ----------------------------------------------------------
  # BIVARIAT
  # ----------------------------------------------------------
  
  output$bi_scatter <- renderPlotly({
    
    req(input$bi_x, input$bi_y)
    
    validate(
      need(
        input$bi_x != input$bi_y,
        "Pilih dua variabel yang berbeda."
      )
    )
    
    d <- dat()
    
    p <- plot_ly(
      d,
      x = d[[input$bi_x]],
      y = d[[input$bi_y]],
      color = ~Stress_Level,
      type = "scatter",
      mode = "markers",
      colors = cols
    )
    
    p %>%
      add_lines(
        x = sort(d[[input$bi_x]]),
        y = fitted(
          lm(d[[input$bi_y]] ~ d[[input$bi_x]])
        )[order(d[[input$bi_x]])],
        inherit = FALSE,
        name = "Garis regresi",
        line = list(color = amber, dash = "dash")
      ) %>%
      style_plotly() %>%
      layout(
        xaxis = list(title = lab(input$bi_x)),
        yaxis = list(title = lab(input$bi_y))
      )
    
  })
  
  output$bi_interp <- renderUI({
    
    d <- dat()
    
    uji_korelasi <- cor.test(
      d[[input$bi_x]],
      d[[input$bi_y]],
      method = "pearson"
    )
    r <- unname(uji_korelasi$estimate)
    p_korelasi <- uji_korelasi$p.value
    
    interp(
      paste0(
        "Hubungan antara ",
        lab(input$bi_x),
        " dan ",
        lab(input$bi_y),
        " memiliki koefisien korelasi Pearson sebesar ",
        fmt(r, 3),
        ". Hubungan tersebut tergolong ",
        kekuatan(r),
        " dan arahnya ",
        if (r > 0) "positif."
        else if (r < 0) "negatif."
        else "tidak menunjukkan arah hubungan linear.",
        " Uji korelasi menghasilkan p-value ", pv(p_korelasi),
        if (p_korelasi < 0.05) "; hubungan signifikan pada taraf 5%."
        else "; belum terdapat bukti hubungan linear yang signifikan pada taraf 5%."
      )
    )
    
  })
  
  cor_mat <- reactive({
    
    cor(
      dat()[, num_vars],
      method = input$cor_method,
      use = "complete.obs"
    )
    
  })
  
  output$bi_heat <- renderPlotly({
    
    cm <- cor_mat()
    
    nm <- lab(colnames(cm))
    
    plot_ly(
      x = nm,
      y = nm,
      z = cm,
      type = "heatmap",
      zmin = -1,
      zmax = 1,
      colorscale = list(
        c(0, pink),
        c(0.5, surface),
        c(1, mint)
      )
    ) %>%
      base_plotly() %>%
      layout(
        xaxis = list(title = "Variabel"),
        yaxis = list(title = "Variabel")
      )
    
  })
  
  output$bi_cor_tbl <- renderTable({
    
    cm <- cor_mat()
    
    hasil <- as.data.frame(as.table(cm))
    
    names(hasil) <- c(
      "Variabel 1",
      "Variabel 2",
      "Korelasi"
    )
    
    hasil$Korelasi <- fmt(hasil$Korelasi, 3)
    
    hasil
    
  })
  
  output$bi_box <- renderPlotly({
    
    d <- dat()
    
    plot_ly(
      d,
      x = ~Stress_Level,
      y = ~GPA,
      color = ~Stress_Level,
      type = "box",
      boxpoints = "all",
      jitter = 0.3,
      colors = cols
    ) %>%
      style_plotly() %>%
      layout(
        xaxis = list(title = "Tingkat Stres"),
        yaxis = list(title = "GPA")
      )
    
  })
  
  output$bi_box_interp <- renderUI({
    
    d <- dat()
    
    med <- tapply(
      d$GPA,
      d$Stress_Level,
      median
    )
    
    interp(
      paste0(
        "Median GPA tertinggi terdapat pada kategori ",
        names(which.max(med)),
        " sebesar ",
        fmt(max(med), 2),
        ", sedangkan median terendah terdapat pada kategori ",
        names(which.min(med)),
        " sebesar ",
        fmt(min(med), 2),
        ". Box plot digunakan untuk membandingkan distribusi GPA ",
        "antar-kategori tingkat stres."
      )
    )
    
  })
  
  # ----------------------------------------------------------
  # MULTIVARIAT: PCA
  # ----------------------------------------------------------
  
  pca_data <- reactive({
    
    d <- dat()

    variasi <- vapply(d[, num_vars, drop = FALSE], sd, numeric(1), na.rm = TRUE)
    validate(
      need(
        all(is.finite(variasi) & variasi > 0),
        "PCA tidak dapat dihitung karena ada variabel yang tidak memiliki variasi pada data terpilih. Longgarkan filter atau periksa dataset."
      )
    )

    prcomp(
      d[, num_vars, drop = FALSE],
      center = TRUE,
      scale. = TRUE
    )
    
  })
  
  output$mv_pca <- renderPlotly({
    
    p <- pca_data()
    
    d <- dat()
    
    skor <- as.data.frame(p$x)
    
    skor$Stress_Level <- d$Stress_Level
    
    plot_ly(
      skor,
      x = ~PC1,
      y = ~PC2,
      color = ~Stress_Level,
      type = "scatter",
      mode = "markers",
      colors = cols
    ) %>%
      style_plotly() %>%
      layout(
        xaxis = list(title = "Komponen Utama 1"),
        yaxis = list(title = "Komponen Utama 2")
      )
    
  })
  
  output$mv_pca_interp <- renderUI({
    
    p <- pca_data()
    
    ve <- 100 * p$sdev^2 / sum(p$sdev^2)
    
    interp(
      paste0(
        "Komponen utama pertama menjelaskan ",
        fmt(ve[1], 2),
        "% variasi data, sedangkan komponen utama kedua menjelaskan ",
        fmt(ve[2], 2),
        "%. Secara kumulatif, kedua komponen menjelaskan ",
        fmt(sum(ve[1:2]), 2),
        "% variasi data."
      )
    )
    
  })
  
  # ----------------------------------------------------------
  # MULTIVARIAT: SCATTER MATRIX
  # ----------------------------------------------------------
  
  output$mv_splom <- renderPlotly({
    
    d <- dat()
    
    plot_ly(
      d,
      type = "splom",
      color = ~Stress_Level,
      colors = cols,
      dimensions = lapply(
        num_vars,
        function(v) {
          list(
            label = lab(v),
            values = d[[v]]
          )
        }
      ),
      marker = list(
        size = 4,
        opacity = 0.7
      )
    ) %>%
      base_plotly()
    
  })
  
  # ----------------------------------------------------------
  # MULTIVARIAT: OUTLIER MAHALANOBIS DAN NORMALITAS
  # ----------------------------------------------------------

  mahal_data <- reactive({
    # Ambil variabel numerik yang digunakan untuk analisis multivariat
    d <- dat()[, num_vars, drop = FALSE]

    # Buang variabel yang tidak memiliki variasi (SD = 0 atau tidak valid)
    variasi <- vapply(d, sd, numeric(1), na.rm = TRUE)
    kolom_valid <- is.finite(variasi) & variasi > 0
    validate(need(sum(kolom_valid) >= 2,
                  "Minimal dua variabel numerik harus memiliki variasi."))
    d <- d[, kolom_valid, drop = FALSE]

    # Identifikasi dan buang variabel yang bergantung secara linear.
    # QR decomposition memilih subset kolom yang bebas linear.
    z <- scale(d)
    qr_d <- qr(z, tol = 1e-8)
    rank_d <- qr_d$rank
    validate(need(rank_d >= 2,
                  "Variabel numerik tidak cukup independen untuk analisis Mahalanobis."))
    kolom_independen <- qr_d$pivot[seq_len(rank_d)]
    d <- d[, kolom_independen, drop = FALSE]

    # Pastikan jumlah observasi memadai untuk mengestimasi kovarians
    validate(need(nrow(d) > ncol(d),
                  "Jumlah observasi harus lebih besar daripada jumlah variabel independen."))

    kov <- cov(d)
    validate(need(all(is.finite(kov)) && qr(kov, tol = 1e-8)$rank == ncol(kov),
                  "Matriks kovarians tetap singular. Periksa data yang dipilih."))

    d2 <- mahalanobis(d, center = colMeans(d), cov = kov)
    derajat_bebas <- ncol(d)
    batas <- qchisq(0.95, df = derajat_bebas)

    data.frame(
      Observasi = seq_len(nrow(d)),
      Jarak = d2,
      Status = ifelse(d2 > batas, "Kandidat outlier", "Normal"),
      Batas = batas,
      DerajatBebas = derajat_bebas
    )
  })

  output$mv_mahal <- renderPlotly({
    h <- mahal_data()
    plot_ly(h, x = ~Observasi, y = ~Jarak, color = ~Status,
            type = "scatter", mode = "markers",
            colors = c("Normal" = mint, "Kandidat outlier" = pink)) %>%
      add_lines(x = range(h$Observasi), y = rep(h$Batas[1], 2),
                inherit = FALSE, name = "Batas Chi-Square 95%",
                line = list(color = amber, dash = "dash")) %>%
      style_plotly() %>%
      layout(xaxis = list(title = "Indeks observasi"),
             yaxis = list(title = "Jarak Mahalanobis kuadrat"))
  })

  output$mv_outlier_tbl <- renderTable({
    h <- mahal_data()
    hasil <- h[h$Status == "Kandidat outlier", c("Observasi", "Jarak", "Status"), drop = FALSE]
    if (nrow(hasil) == 0) {
      data.frame(Keterangan = "Tidak ditemukan kandidat outlier pada batas 95%.")
    } else {
      hasil$Jarak <- fmt(hasil$Jarak, 3)
      hasil
    }
  })

  output$mv_qq <- renderPlotly({
    h <- mahal_data()
    d2 <- sort(h$Jarak)
    n <- length(d2)
    p <- h$DerajatBebas[1]
    chi <- qchisq((seq_len(n) - 0.5) / n, df = p)
    plot_ly(x = chi, y = d2, type = "scatter", mode = "markers",
            marker = list(color = sky)) %>%
      add_lines(x = range(chi), y = range(chi), inherit = FALSE,
                name = "Garis acuan", line = list(color = amber, dash = "dash")) %>%
      style_plotly() %>%
      layout(xaxis = list(title = "Kuantil Chi-Square teoretis"),
             yaxis = list(title = "Jarak Mahalanobis terurut"))
  })

  # ----------------------------------------------------------
  # REGRESI LINEAR BERGANDA
  # ----------------------------------------------------------
  
  model_regresi <- reactive({
    
    req(input$reg_x)
    
    validate(
      need(
        length(input$reg_x) >= 2,
        "Pilih minimal dua variabel prediktor."
      )
    )
    
    d <- dat()
    
    prediktor <- input$reg_x
    
    if (input$reg_stress && nlevels(d$Stress_Level) > 1) {
      prediktor <- c(prediktor, "Stress_Level")
    }
    
    formula_model <- as.formula(
      paste(
        "GPA ~",
        paste(prediktor, collapse = " + ")
      )
    )
    
    lm(
      formula_model,
      data = d
    )
    
  })
  
  output$reg_formula <- renderPrint({
    
    m <- model_regresi()
    
    cat("Persamaan model regresi:\n\n")
    
    print(formula(m))
    
    cat("\nKoefisien estimasi:\n")
    
    print(coef(m))
    
  })
  
  output$reg_kpis <- renderUI({
    
    m <- model_regresi()
    
    s <- summary(m)
    
    f <- s$fstatistic
    
    pf_value <- pf(
      f[1],
      f[2],
      f[3],
      lower.tail = FALSE
    )
    
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      
      kpi(
        "R-Squared",
        fmt(s$r.squared, 3),
        mint
      ),
      
      kpi(
        "Adjusted R²",
        fmt(s$adj.r.squared, 3),
        sky
      ),
      
      kpi(
        "RMSE",
        fmt(s$sigma, 3),
        amber
      ),
      
      kpi(
        "P-value Uji F",
        pv(pf_value),
        pink
      )
    )
    
  })
  
  output$reg_coef <- renderTable({

    m <- model_regresi()

    # Ambil seluruh nama koefisien dari model, termasuk koefisien
    # yang mungkin tidak dapat diestimasi akibat multikolinearitas.
    koef <- stats::coef(m)
    nama_parameter <- names(koef)

    # Ringkasan koefisien dan interval kepercayaan dapat memiliki
    # jumlah baris berbeda jika ada koefisien yang aliased.
    s <- summary(m)$coefficients
    ci <- tryCatch(
      suppressMessages(stats::confint(m, level = 0.95)),
      error = function(e) NULL
    )

    # Selaraskan baris berdasarkan nama parameter, bukan posisi baris.
    idx_s <- match(nama_parameter, rownames(s))
    ambil_s <- function(kolom) {
      hasil <- rep(NA_real_, length(nama_parameter))
      valid <- !is.na(idx_s)
      hasil[valid] <- s[idx_s[valid], kolom]
      hasil
    }

    ci_bawah <- rep(NA_real_, length(nama_parameter))
    ci_atas <- rep(NA_real_, length(nama_parameter))

    if (!is.null(ci) && !is.null(rownames(ci))) {
      idx_ci <- match(nama_parameter, rownames(ci))
      valid_ci <- !is.na(idx_ci)
      ci_bawah[valid_ci] <- ci[idx_ci[valid_ci], 1]
      ci_atas[valid_ci] <- ci[idx_ci[valid_ci], 2]
    }

    p_value <- ambil_s(4)

    data.frame(
      Parameter = nama_parameter,
      Koefisien = fmt(unname(koef), 4),
      Std_Error = fmt(ambil_s(2), 4),
      t_hitung = fmt(ambil_s(3), 3),
      P_value = vapply(p_value, pv, character(1)),
      CI_Bawah = fmt(ci_bawah, 3),
      CI_Atas = fmt(ci_atas, 3),
      Signifikan = ifelse(
        is.na(p_value),
        "Tidak dapat diuji",
        ifelse(p_value < 0.05, "Ya", "Tidak")
      ),
      check.names = FALSE
    )

  })
  
  output$reg_interp <- renderUI({
    
    m <- model_regresi()
    
    s <- summary(m)
    
    f <- s$fstatistic
    
    pF <- pf(
      f[1],
      f[2],
      f[3],
      lower.tail = FALSE
    )
    
    interp(
      paste0(
        "Nilai R-squared sebesar ",
        fmt(s$r.squared, 3),
        " menunjukkan bahwa ",
        fmt(s$r.squared * 100, 2),
        "% variasi GPA dapat dijelaskan oleh prediktor dalam model. ",
        "Uji F menghasilkan p-value sebesar ",
        pv(pF),
        ". ",
        if (pF < 0.05) {
          "Model signifikan secara simultan pada taraf 5%."
        } else {
          "Belum terdapat bukti yang cukup bahwa model signifikan secara simultan pada taraf 5%."
        }
      )
    )
    
  })
  
  # ----------------------------------------------------------
  # VIF
  # ----------------------------------------------------------
  
  vif_manual <- function(m) {
    
    X <- model.matrix(m)
    
    X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
    
    if (ncol(X) == 0) {
      return(data.frame(
        Variabel = character(),
        VIF = numeric()
      ))
    }
    
    if (ncol(X) == 1) {
      return(data.frame(
        Variabel = colnames(X),
        VIF = 1
      ))
    }
    
    hasil <- sapply(
      seq_len(ncol(X)),
      function(j) {
        
        target <- X[, j]
        
        lainnya <- X[, -j, drop = FALSE]
        
        r2 <- summary(
          lm(target ~ lainnya)
        )$r.squared
        
        if (r2 >= 1) Inf else 1 / (1 - r2)
        
      }
    )
    
    data.frame(
      Variabel = colnames(X),
      VIF = hasil
    )
    
  }
  
  output$reg_vif <- renderTable({
    
    v <- vif_manual(model_regresi())
    
    v$VIF <- fmt(v$VIF, 3)
    
    v
    
  })
  
  # ----------------------------------------------------------
  # UJI ASUMSI REGRESI
  # ----------------------------------------------------------
  
  output$reg_assumption <- renderTable({
    m <- model_regresi()
    r <- residuals(m)
    n <- length(r)

    # Shapiro-Wilk
    sw <- if (n >= 3 && n <= 5000 && length(unique(r)) > 1) {
      tryCatch(shapiro.test(r)$p.value, error = function(e) NA_real_)
    } else NA_real_

    # Lilliefors (Kolmogorov-Smirnov dengan parameter normal diestimasi)
    lillie <- if (n >= 5 && length(unique(r)) > 1) {
      tryCatch(nortest::lillie.test(r)$p.value, error = function(e) NA_real_)
    } else NA_real_

    # Jarque-Bera
    jb <- n / 6 * (skew(r)^2 + (kurt(r) - 3)^2 / 4)
    jb_p <- pchisq(jb, df = 2, lower.tail = FALSE)

    # Breusch-Pagan secara manual
    X <- model.matrix(m)[, -1, drop = FALSE]
    bp <- tryCatch({
      bp_model <- lm(I(r^2) ~ X)
      pchisq(n * summary(bp_model)$r.squared, df = ncol(X), lower.tail = FALSE)
    }, error = function(e) NA_real_)

    # Durbin-Watson dari lmtest
    dw <- tryCatch(lmtest::dwtest(m), error = function(e) NULL)
    dw_stat <- if (is.null(dw)) NA_real_ else as.numeric(dw$statistic)
    dw_p <- if (is.null(dw)) NA_real_ else dw$p.value

    data.frame(
      Uji = c("Shapiro-Wilk", "Lilliefors", "Jarque-Bera", "Breusch-Pagan", "Durbin-Watson"),
      Nilai = c(pv(sw), pv(lillie), pv(jb_p), pv(bp),
                if (is.na(dw_stat)) "-" else paste0(fmt(dw_stat, 3), " (p=", pv(dw_p), ")")),
      Kesimpulan = c(
        ifelse(is.na(sw), "Tidak diuji", ifelse(sw > 0.05, "Tidak menolak normalitas", "Menolak normalitas")),
        ifelse(is.na(lillie), "Tidak diuji", ifelse(lillie > 0.05, "Tidak menolak normalitas", "Menolak normalitas")),
        ifelse(jb_p > 0.05, "Tidak menolak normalitas", "Menolak normalitas"),
        ifelse(is.na(bp), "Tidak dapat dihitung", ifelse(bp > 0.05, "Tidak terdapat bukti heteroskedastisitas", "Terdapat indikasi heteroskedastisitas")),
        ifelse(is.na(dw_p), "Tidak dapat dihitung", ifelse(dw_p > 0.05, "Tidak ada bukti autokorelasi", "Terdapat indikasi autokorelasi"))
      )
    )
  })
  
  # ----------------------------------------------------------
  # GRAFIK DIAGNOSTIK REGRESI
  # ----------------------------------------------------------
  
  output$reg_actual <- renderPlotly({
    
    m <- model_regresi()
    
    d <- dat()
    
    hasil <- data.frame(
      Aktual = d$GPA,
      Prediksi = fitted(m),
      Stress_Level = d$Stress_Level
    )
    
    plot_ly(
      hasil,
      x = ~Prediksi,
      y = ~Aktual,
      color = ~Stress_Level,
      type = "scatter",
      mode = "markers",
      colors = cols
    ) %>%
      add_lines(
        x = range(hasil$Aktual),
        y = range(hasil$Aktual),
        inherit = FALSE,
        name = "Garis ideal",
        line = list(color = amber, dash = "dash")
      ) %>%
      style_plotly() %>%
      layout(
        xaxis = list(title = "GPA Prediksi"),
        yaxis = list(title = "GPA Aktual")
      )
    
  })
  
  output$reg_residual <- renderPlotly({
    
    m <- model_regresi()
    
    plot_ly(
      x = fitted(m),
      y = residuals(m),
      type = "scatter",
      mode = "markers",
      marker = list(color = mint)
    ) %>%
      add_lines(
        x = range(fitted(m)),
        y = c(0, 0),
        inherit = FALSE,
        line = list(color = pink, dash = "dash")
      ) %>%
      style_plotly() %>%
      layout(
        xaxis = list(title = "Nilai Prediksi"),
        yaxis = list(title = "Residual")
      )
    
  })
  
  output$reg_qq <- renderPlotly({
    
    r <- residuals(model_regresi())
    
    q <- qqnorm(
      r,
      plot.it = FALSE
    )
    
    plot_ly(
      x = q$x,
      y = q$y,
      type = "scatter",
      mode = "markers",
      marker = list(color = mint)
    ) %>%
      style_plotly() %>%
      layout(
        xaxis = list(title = "Kuantil Teoretis"),
        yaxis = list(title = "Kuantil Residual")
      )
    
  })
  
  output$reg_cook <- renderPlotly({
    
    m <- model_regresi()
    
    cd <- cooks.distance(m)
    
    ambang <- 4 / length(cd)
    
    plot_ly(
      x = seq_along(cd),
      y = cd,
      type = "bar",
      marker = list(
        color = ifelse(cd > ambang, pink, violet)
      )
    ) %>%
      style_plotly() %>%
      layout(
        xaxis = list(title = "Indeks Observasi"),
        yaxis = list(title = "Cook's Distance"),
        shapes = list(
          list(
            type = "line",
            x0 = 0,
            x1 = length(cd),
            y0 = ambang,
            y1 = ambang,
            line = list(color = amber, dash = "dash")
          )
        )
      )
    
  })
  
  # ----------------------------------------------------------
  # KESIMPULAN OTOMATIS
  # ----------------------------------------------------------
  
  output$kesimpulan <- renderUI({
    
    d <- df
    
    m <- lm(
      GPA ~ Study_Hours_Per_Day +
        Sleep_Hours_Per_Day +
        Social_Hours_Per_Day +
        Physical_Activity_Hours_Per_Day +
        Extracurricular_Hours_Per_Day +
        Stress_Level,
      data = d
    )
    
    s <- summary(m)
    
    f <- s$fstatistic
    
    pF <- pf(
      f[1],
      f[2],
      f[3],
      lower.tail = FALSE
    )
    
    korelasi <- cor(
      d$Study_Hours_Per_Day,
      d$GPA
    )
    
    card(
      card_header("Kesimpulan dan Rekomendasi"),
      
      h5("Kesimpulan", class = "section-title"),
      
      tags$ol(
        tags$li(
          paste0(
            "Dataset yang dianalisis terdiri atas ",
            nrow(d),
            " observasi setelah proses pembersihan data."
          )
        ),
        
        tags$li(
          paste0(
            "Rata-rata GPA mahasiswa sebesar ",
            fmt(mean(d$GPA), 2),
            "."
          )
        ),
        
        tags$li(
          paste0(
            "Korelasi Pearson antara jam belajar dan GPA sebesar ",
            fmt(korelasi, 3),
            ", dengan hubungan ",
            kekuatan(korelasi),
            "."
          )
        ),
        
        tags$li(
          paste0(
            "Model regresi menghasilkan R-squared sebesar ",
            fmt(s$r.squared, 3),
            "."
          )
        ),
        
        tags$li(
          paste0(
            "Uji F menghasilkan p-value ",
            pv(pF),
            ". ",
            if (pF < 0.05) {
              "Model signifikan secara simultan."
            } else {
              "Model belum signifikan secara simultan pada taraf 5%."
            }
          )
        )
      ),
      
      h5("Rekomendasi", class = "section-title"),
      
      tags$ul(
        tags$li(
          "Mahasiswa dapat memperhatikan keseimbangan waktu belajar, tidur, dan aktivitas harian."
        ),
        
        tags$li(
          "Hasil analisis dapat digunakan sebagai gambaran hubungan gaya hidup dan prestasi akademik."
        ),
        
        tags$li(
          "Penelitian lanjutan dapat menambahkan variabel lain yang relevan untuk meningkatkan kemampuan model."
        )
      ),
      
      p(
        class = "note",
        "Kesimpulan dihitung berdasarkan data lengkap dan tidak dipengaruhi filter pada sidebar."
      )
    )
    
  })
  
  # ----------------------------------------------------------
  # TABEL DATA DAN DOWNLOAD
  # ----------------------------------------------------------
  
  output$tbl_data <- DT::renderDT({
    
    DT::datatable(
      dat(),
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
  })
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste0(
        "student_lifestyle_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      write.csv(
        dat(),
        file,
        row.names = FALSE
      )
    }
    
  )
  
}

# ============================================================
# BAGIAN 5: MENJALANKAN APLIKASI
# ============================================================

shinyApp(
  ui = ui,
  server = server
)


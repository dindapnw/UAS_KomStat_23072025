library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(sf)
library(leaflet)
library(car)
library(lmtest)
library(MASS)
library(readxl)
library(htmltools)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(webshot2)
library(htmlwidgets) 
library(rsconnect)
rsconnect::writeManifest()

# Load data
data <- read.csv("www/data.csv")
kabkota_lookup <- read_excel("www/data_nama.xlsx")
peta <- st_read("www/indonesia511.geojson", quiet = TRUE)

#merger
data <- merge(data, kabkota_lookup, by = "DISTRICTCODE", all.x = TRUE)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Oswald:wght@700&display=swap');
      
      .shiny-text-output {
          text-align: left !important;
          white-space: pre-wrap;
          font-size: 14px;
      }
      
      body {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        background: linear-gradient(135deg, #ffffff 0%, #fefce8 25%, #fef3c7 50%, #f3f4f6 100%);
        margin: 0;
        padding: 0;
        min-height: 100vh;
        position: relative;
      }
      
      /* Background Pattern */
      body::before {
        content: '';
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-image: 
          radial-gradient(circle at 20% 20%, rgba(255, 215, 0, 0.05) 0%, transparent 50%),
          radial-gradient(circle at 80% 80%, rgba(251, 191, 36, 0.05) 0%, transparent 50%),
          radial-gradient(circle at 40% 60%, rgba(245, 158, 11, 0.03) 0%, transparent 50%);
        pointer-events: none;
        z-index: -1;
      }
      
      /* Header Navigation */
      .main-header {
        background: linear-gradient(135deg, #1f2937 0%, #374151 50%, #4b5563 100%);
        color: white;
        padding: 20px 0;
        box-shadow: 0 4px 25px rgba(0,0,0,0.15), 0 2px 10px rgba(0,0,0,0.1);
        margin-bottom: 20px;
        position: sticky;
        top: 0;
        z-index: 1000;
        backdrop-filter: blur(20px);
        border-bottom: 1px solid rgba(255, 215, 0, 0.3);
      }
      
      .header-content {
        max-width: 1400px;
        margin: 0 auto;
        padding: 0 20px;
        position: relative;
      }
      
      .site-title {
        font-family: 'Oswald', sans-serif;
        font-size: 5em;
        font-weight: 1000;
        background: linear-gradient(135deg, #ffd700 0%, #fbbf24 50%, #f59e0b 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        text-align: center;
        margin-bottom: 25px;
        text-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      /* Tab Panel Styling */
      .tab-navigation {
        transition: all 0.3s ease;
      }
      
      .navbar-nav {
        display: flex;
        justify-content: center;
        flex-wrap: wrap;
        gap: 8px;
        list-style: none;
        padding: 0;
        margin: 0;
        width: 100%;
      }
      
      .navbar-nav .nav-item {
        margin: 0;
      }
      
      .nav-tabs {
        border: none;
        justify-content: center;
        flex-wrap: wrap;
        gap: 8px;
        display: flex;
        width: 100%;
      }
      
      .nav-tabs .nav-link {
        background: rgba(255,255,255,0.15);
        color: #e5e7eb !important;
        border: 2px solid rgba(255,215,0,0.4);
        border-radius: 30px !important;
        padding: 10px 20px;
        margin: 0 4px 12px 0;
        font-weight: 600;
        font-size: 14px;
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
        backdrop-filter: blur(15px);
        cursor: pointer;
        text-decoration: none;
        position: relative;
        overflow: hidden;
      }
      
      .nav-tabs .nav-link::before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(255,215,0,0.2), transparent);
        transition: left 0.5s ease;
      }
      
      .nav-tabs .nav-link:hover::before {
        left: 100%;
      }
      
      .nav-tabs .nav-link:hover {
        background: rgba(255,215,0,0.25) !important;
        color: #ffd700 !important;
        border-color: #fbbf24 !important;
        transform: translateY(-3px) scale(1.02);
        box-shadow: 0 8px 25px rgba(255,215,0,0.4), 0 4px 12px rgba(0,0,0,0.1);
      }
      
      .nav-tabs .nav-link.active {
        background: linear-gradient(135deg, #ffd700 0%, #fbbf24 100%) !important;
        color: #1f2937 !important;
        border-color: #f59e0b !important;
        font-weight: 700;
        box-shadow: 0 6px 20px rgba(255,215,0,0.5), 0 3px 10px rgba(0,0,0,0.2);
        transform: translateY(-2px);
      }
      
      /* Override default tab styles */
      .tabbable > .nav-tabs {
        border-bottom: none;
        display: flex;
        justify-content: center;
        width: 100%;
      }
      
      .tab-content {
        border: none;
        padding: 0;
        text-align: center;
      }
      
      .tab-content > .tab-pane {
        padding: 0;
      }
      
      /* Main Content */
      .main-content {
        max-width: 1400px;
        margin: 0 auto;
        padding: 0 25px;
      }
      
      /* Card Styling */
      .modern-card {
        background: linear-gradient(145deg, rgba(255,255,255,0.9) 0%, rgba(255,255,255,0.7) 100%);
        backdrop-filter: blur(20px);
        border-radius: 20px;
        box-shadow: 
          0 10px 40px rgba(0,0,0,0.1),
          0 4px 15px rgba(0,0,0,0.05),
          inset 0 1px 0 rgba(255,255,255,0.5);
        padding: 35px;
        margin-bottom: 35px;
        border: 1px solid rgba(255,215,0,0.2);
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
        position: relative;
        overflow: hidden;
      }
      
      .modern-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 3px;
        background: linear-gradient(90deg, #ffd700, #fbbf24, #f59e0b);
        opacity: 0.8;
      }
      
      .modern-card.no-hover:hover {
        transform: none !important;
        box-shadow: none !important;
        cursor: default !important;
      }
      
      .modern-card:hover {
        transform: translateY(-3px) scale(1.001);
        box-shadow: 
          0 20px 60px rgba(0,0,0,0.15),
          0 8px 25px rgba(0,0,0,0.1),
          inset 0 1px 0 rgba(255,255,255,0.7);
        border-color: rgba(255,215,0,0.4);
      }
      
      .hero-card {
        background: linear-gradient(135deg, #1f2937 0%, #374151 50%, #4b5563 100%);
        color: white;
        text-align: center;
        padding: 80px 40px;
        border: 2px solid #ffd700;
        position: relative;
        overflow: hidden;
      }
      
      .hero-card::after {
        content: '';
        position: absolute;
        top: -50%;
        right: -50%;
        width: 200%;
        height: 200%;
        background: radial-gradient(circle, rgba(255,215,0,0.1) 0%, transparent 70%);
        animation: float 6s ease-in-out infinite;
      }
      
      .hero-card h1 {
        font-size: 3em;
        font-weight: 800;
        margin-bottom: 25px;
        background: linear-gradient(135deg, #ffd700 0%, #fbbf24 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
        position: relative;
        z-index: 1;
      }
      
      .hero-card p {
        font-size: 1.3em;
        opacity: 0.95;
        max-width: 700px;
        margin: 0 auto;
        line-height: 1.6;
        position: relative;
        z-index: 1;
      }
      
      /* Feature Cards */
      .feature-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
        gap: 30px;
        margin-top: 40px;
      }
      
      .feature-card {
        background: linear-gradient(145deg, rgba(255,255,255,0.95) 0%, rgba(255,255,255,0.8) 100%);
        backdrop-filter: blur(20px);
        border-radius: 18px;
        padding: 35px 30px;
        text-align: center;
        box-shadow: 
          0 8px 30px rgba(0,0,0,0.1),
          0 3px 10px rgba(0,0,0,0.05);
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
        border: 2px solid rgba(255,215,0,0.2);
        position: relative;
        overflow: hidden;
      }
      
      .feature-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(255,215,0,0.1), transparent);
        transition: left 0.6s ease;
      }
      
      .feature-card:hover::before {
        left: 100%;
      }
      
      .feature-card:hover {
        transform: translateY(-8px) scale(1.03);
        box-shadow: 
          0 15px 50px rgba(0,0,0,0.2),
          0 6px 20px rgba(0,0,0,0.1);
        border-color: #ffd700;
      }
      
      .feature-card .icon {
        font-size: 3.5em;
        margin-bottom: 20px;
        background: linear-gradient(135deg, #fbbf24, #000000, #ffd700);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.1);
      }
      
      .feature-card h3 {
        color: #1f2937;
        margin-bottom: 15px;
        font-weight: 700;
        font-size: 1.4em;
      }
      
      .feature-card p {
        color: #4b5563;
        font-size: 15px;
        line-height: 1.6;
      }
      
      /* Content Sections */
      .content-section {
        background: linear-gradient(145deg, rgba(255,255,255,0.95) 0%, rgba(255,255,255,0.85) 100%);
        backdrop-filter: blur(20px);
        border-radius: 20px;
        padding: 40px;
        margin-bottom: 35px;
        box-shadow: 
          0 10px 40px rgba(0,0,0,0.1),
          0 4px 15px rgba(0,0,0,0.05);
        border: 2px solid rgba(255,215,0,0.25);
        position: relative;
        overflow: hidden;
      }
      
      .content-section::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(90deg, #ffd700, #fbbf24, #f59e0b, #d97706);
        opacity: 0.8;
      }
      
      .content-section h2 {
        color: #1f2937;
        font-size: 2.5em;
        padding-bottom: 15px;
        margin-bottom: 20px;
        font-weight: 800;
        text-align: center;
        background: linear-gradient(135deg, #1f2937 0%, #4b5563 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
      }
      
      .content-section p {
        color: #4b5563;
        font-size: 17px;
        line-height: 1.7;
        margin-bottom: 18px;
        text-align: left;
      }
      
      .content-section ul {
        text-align: left;
      }
      
      .content-section li {
        color: #4b5563;
        margin-bottom: 12px;
        font-size: 16px;
        line-height: 1.6;
        position: relative;
        padding-left: 25px;
      }
      
      .content-section li::before {
        content: '✦';
        position: absolute;
        left: 0;
        color: #fbbf24;
        font-weight: bold;
      }
      
      /* Responsive Design */
      @media (max-width: 768px) {
        .hamburger-menu {
          display: flex;
        }
        
        .tab-navigation.mobile-hidden {
          display: none;
        }
        
        .mobile-menu {
          display: none;
          position: fixed;
          top: 0;
          left: 0;
          width: 100vw;
          height: 100vh;
          background: linear-gradient(135deg, rgba(31, 41, 55, 0.98) 0%, rgba(55, 65, 81, 0.98) 100%);
          backdrop-filter: blur(20px);
          z-index: 1002;
          padding: 80px 20px 20px;
          overflow-y: auto;
        }
        
        .mobile-menu.active {
          display: block;
        }
        
        .mobile-nav-item {
          display: block;
          width: 100%;
          padding: 18px 25px;
          margin: 10px 0;
          background: rgba(255,255,255,0.1);
          color: #e5e7eb;
          border: 2px solid rgba(255,215,0,0.3);
          border-radius: 15px;
          text-align: center;
          font-weight: 600;
          font-size: 16px;
          transition: all 0.3s ease;
          cursor: pointer;
          text-decoration: none;
        }
        
        .mobile-nav-item:hover,
        .mobile-nav-item.active {
          background: linear-gradient(135deg, #ffd700, #fbbf24);
          color: #1f2937;
          border-color: #f59e0b;
          transform: scale(1.02);
        }
        
        .main-content {
          padding: 0 15px;
        }
        
        .hero-card {
          padding: 50px 25px;
        }
        
        .hero-card h1 {
          font-size: 2.2em;
        }
        
        .feature-grid {
          grid-template-columns: 1fr;
          gap: 20px;
        }
        
        .feature-grid-four {
          grid-template-columns: 1fr;
          gap: 20px;
        }
        
        .site-title {
          font-size: 3em;
          margin-bottom: 15px;
        }
        
        .content-section {
          padding: 30px 25px;
        }
        
        .content-section h2 {
          font-size: 2em;
        }
        
        .modern-card {
          padding: 25px 20px;
        }
      }
      
      /* Animation */
      @keyframes fadeInUp {
        from {
          opacity: 0;
          transform: translateY(40px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
      
      @keyframes float {
        0%, 100% {
          transform: translateY(0px) rotate(0deg);
        }
        50% {
          transform: translateY(-10px) rotate(5deg);
        }
      }
      
      /* Enhanced Scrollbar */
      ::-webkit-scrollbar {
        width: 10px;
      }
      
      ::-webkit-scrollbar-track {
        background: rgba(243, 244, 246, 0.5);
        border-radius: 10px;
      }
      
      ::-webkit-scrollbar-thumb {
        background: linear-gradient(135deg, #ffd700, #fbbf24, #f59e0b);
        border-radius: 10px;
        border: 1px solid rgba(255,255,255,0.3);
      }
      
      ::-webkit-scrollbar-thumb:hover {
        background: linear-gradient(135deg, #f59e0b, #fbbf24, #ffd700);
        box-shadow: 0 2px 8px rgba(255,215,0,0.3);
      }
    ")),
    
    # JavaScript for mobile menu
    tags$script(HTML("
    $(document).ready(function() {
    
    // Toggle mobile menu
      $('.hamburger-menu').click(function() {
        $(this).toggleClass('active');
        $('.mobile-menu').toggleClass('active');
        $('.tab-navigation').toggleClass('mobile-hidden');
      });
      
      // Close mobile menu when clicking nav item
      $('.mobile-nav-item').click(function() {
        var tabValue = $(this).data('tab');
        $('.hamburger-menu').removeClass('active');
        $('.mobile-menu').removeClass('active');
        $('.tab-navigation').removeClass('mobile-hidden');
        
        // Trigger tab change
        $('a[data-value=\"' + tabValue + '\"]').tab('show');
      });
      
      // Close mobile menu when clicking outside
      $('.mobile-menu').click(function(e) {
        if (e.target === this) {
          $('.hamburger-menu').removeClass('active');
          $(this).removeClass('active');
          $('.tab-navigation').removeClass('mobile-hidden');
        }
      });
    });
  ")),
  ),
  
  # Header Section
  div(class = "main-header",
      div(class = "header-content",
          h1(class = "site-title", "INFRASEKA")
      )
  ),
  
  # Main Content with tabsetPanel
  div(class = "main-content",
      div(class = "tab-navigation",
          tabsetPanel(
            id = "main_tabs",
            type = "tabs",
            
            # Beranda Tab
            tabPanel(
              title = "Beranda",
              value = "beranda",
              div(div(class = "modern-card hero-card no-hover",
                      h1("Selamat Datang!"),
                      p("Sistem visualisasi dan analisis yang mengkaji pengaruh akses infrastruktur dasar terhadap tingkat kemiskinan rumah tangga di tingkat kabupaten/kota di Indonesia.")
              ),
              
              HTML('
                    <div class = "content-section", style="display: flex; text-align:left !important;">
                      <div style="width: 48%; padding: 2px; margin-right: 4%;">
                        <h3>🏘️ Apa Itu <strong>Infraseka</strong>?</h3>
                        <p>
                          <strong>Infraseka</strong> adalah sebuah istilah yang merupakan singkatan dari 
                          <em>Infrastruktur</em> + <em>Sosial-Ekonomi</em> + <em>Kemiskinan</em>.<br><br>
                          Nama ini dipilih untuk merepresentasikan fokus utama dari analisis, yaitu bagaimana akses terhadap infrastruktur dasar 
                          berhubungan dengan kondisi sosial-ekonomi dan kemiskinan rumah tangga di berbagai wilayah.
                        </p><br>
                
                        <h3>Tujuan <strong>Infraseka</strong>?</h3>
                        <p><strong>Infraseka</strong> bertujuan untuk:</p>
                        <ol>
                          <li>Mengukur tingkat kerentanan dasar rumah tangga berdasarkan ketersediaan infrastruktur esensial.</li>
                          <li>Mendeteksi ketimpangan wilayah melalui indikator spasial dan sosial-ekonomi.</li>
                          <li>Mendukung pengambilan kebijakan berbasis data untuk intervensi pembangunan yang lebih tepat sasaran.</li>
                        </ol><br>
                
                        <blockquote style="border-left: 4px solid #ccc; margin: 1em 0; padding-left: 1em;">
                          <strong>Infraseka</strong> adalah indikator gabungan untuk mengukur kerentanan sosial-ekonomi rumah tangga berdasarkan akses terhadap infrastruktur dasar. 
                          Nama ini mencerminkan fokus pada <em>Infrastruktur</em> + <em>Sosial-Ekonomi</em> + <em>Kemiskinan</em>, sehingga menjadi alat penting untuk analisis 
                          dan pemetaan kerentanan wilayah.
                        </blockquote>
                      </div>
                      
                      <div style="width: 50%; padding: 2px;">
                        <h3>🔧 Komponen Utama dan Variabel <strong>Infraseka</strong></h3>
                        <p>Indeks ini menggabungkan beberapa indikator yang berkaitan dengan akses terhadap infrastruktur rumah tangga:</p>
                        
                        <table style="
                          border-collapse: collapse;
                          width: 100%;
                          border: 2px solid black;
                          border-radius: 10px;
                          overflow: hidden;
                        ">
                          <thead>
                            <tr style="background-color: #f2f2f2;">
                              <th style="border: 1px solid black; padding: 10px; width: 30%; text-align: center;">Komponen</th>
                              <th style="border: 1px solid black; padding: 10px; text-align: center;">Keterangan</th>
                            </tr>
                          </thead>
                          <tbody>
                            <tr>
                              <td style="border: 1px solid black; padding: 10px;">🔌 Listrik</td>
                              <td style="border: 1px solid black; padding: 10px;">Rumah tangga tanpa akses listrik atau sumber listrik tidak layak.</td>
                            </tr>
                            <tr>
                              <td style="border: 1px solid black; padding: 10px;">💧 Sumber Air</td>
                              <td style="border: 1px solid black; padding: 10px;">Rumah tangga tanpa akses ke sumber air minum layak (PAM, sumur bor, dll).</td>
                            </tr>
                            <tr>
                              <td style="border: 1px solid black; padding: 10px;">🚽 Sanitasi</td>
                              <td style="border: 1px solid black; padding: 10px;">Rumah tangga tanpa fasilitas sanitasi layak (seperti WC sehat dan saluran pembuangan).</td>
                            </tr>
                            <tr>
                              <td style="border: 1px solid black; padding: 10px;">🏠 Hunian</td>
                              <td style="border: 1px solid black; padding: 10px;">Rumah tangga yang tinggal di rumah kontrakan/sewa sebagai proksi kerentanan tempat tinggal.</td>
                            </tr>
                          </tbody>
                        </table>
                        <p>Indikator-indikator ini kemudian dikuantifikasi dan digunakan pada dashboard untuk mengukur hubungan dengan tingkat kemiskinan.</p><br>
                        
                        <p>Berdasarkan indikator infrastruktur dasar, variabel independen yang digunakan dalam analisis meliputi:</p>
                        <table style="
                          border-collapse: collapse;
                          width: 100%;
                          border: 2px solid black;
                          border-radius: 10px;
                          overflow: hidden;
                        ">
                          <thead>
                            <tr style="background-color: #f2f2f2;">
                              <th style="border: 1px solid black; padding: 10px; text-align: center;">Variabel</th>
                              <th style="border: 1px solid black; padding: 10px; text-align: center;">Keterangan</th>
                            </tr>
                          </thead>
                          <tbody>
                            <tr>
                              <td style="border: 1px solid black; padding: 10px;">NOELECTRIC</td>
                              <td style="border: 1px solid black; padding: 10px;">Persentase rumah tangga yang tidak menggunakan listrik sebagai sumber penerangan</td>
                            </tr>
                            <tr>
                              <td style="border: 1px solid black; padding: 10px;">TAPWATER</td>
                              <td style="border: 1px solid black; padding: 10px;">Persentase rumah tangga yang menggunakan air ledeng (pipa) sebagai sumber air utama</td>
                            </tr>
                            <tr>
                              <td style="border: 1px solid black; padding: 10px;">NOSEWER</td>
                              <td style="border: 1px solid black; padding: 10px;">Persentase rumah tangga yang tidak memiliki saluran pembuangan air limbah.</td>
                            </tr>
                            <tr>
                              <td style="border: 1px solid black; padding: 10px;">RENTED</td>
                              <td style="border: 1px solid black; padding: 10px;">Persentase rumah tangga yang menyewa rumah</td>
                            </tr>
                          </tbody>
                        </table><br>
                      </div>
                    </div>
                  '),
              
              div(class = "feature-grid", style = "margin: 10px;",
                  div(class = "feature-card", class = "mobile-nav-item", `data-tab` = "manajemen", style = "cursor: pointer;",
                      div(class = "icon", "⚡"),
                      h3("Manajemen Data"),
                      p("Mengkategorikan data kontinyu menjadi data kategorik.")
                  ),
                  div(class = "feature-card", class = "mobile-nav-item", `data-tab` = "eksplorasi", style = "cursor: pointer;",
                      div(class = "icon", "⚡"), 
                      h3("Eksplorasi Data"),
                      p("Ringkasan statistik, visualisasi data, tabel, dan peta.")
                  ),
                  div(class = "feature-card", class = "mobile-nav-item", `data-tab` = "rata2", style = "cursor: pointer;",
                      div(class = "icon", "⚡"), 
                      h3("Uji Beda Rata-Rata"),
                      p("Uji perbedaan rata-rata untuk satu dan dua kelompok.")
                  ),
                  div(class = "feature-card", class = "mobile-nav-item", `data-tab` = "proporsi", style = "cursor: pointer;",
                      div(class = "icon", "⚡"),
                      h3("Uji Proporsi & Uji Varians"),
                      p("Analisis proporsi dan kesamaan variabilitas antar kelompok.")
                  ),
                  div(class = "feature-card", class = "mobile-nav-item", `data-tab` = "anova", style = "cursor: pointer;",
                      div(class = "icon", "⚡"),
                      h3("ANOVA"),
                      p("Uji perbedaan rata-rata lebih dari dua kelompok.")
                  ),
                  div(class = "feature-card", class = "mobile-nav-item", `data-tab` = "regresi", style = "cursor: pointer;",
                      div(class = "icon", "⚡"),
                      h3("Analisis Regresi"),
                      p("Identifikasi hubungan antar variabel dan prediksi nilai.")
                  )
              ),
              
              div(class = "content-section", style = "margin-top: 30px;",
                  fluidRow(
                    column(12,
                           selectInput("report_format", "Pilih Format:",
                                       choices = c("HTML" = "html_document",
                                                   "PDF" = "pdf_document",
                                                   "Word" = "word_document"),
                                       selected = "html_document",
                                       width = "100%")
                    )
                  ),
                  downloadButton("downloadReport", "Unduh Laporan Lengkap"),
              ),
              
              HTML('
                    <div class = "modern-card hero-card", style = "padding: 15px !important; margin-top: 30px !important;"> 
                    <h1 style="text-align: center;"><strong>METADATA</strong></h1>
                      <div style="display: flex; text-align:left !important; ">
                          <div style="flex: 1; width: 52%;">
                            <blockquote style="border-left: 4px solid #ccc; margin: 1em 2em;">
                              <h4><strong>Sumber Data</strong></h4>
                              <ul>
                                  <li style="font-size:15px;">Data utama berasal dari <strong>Survei Sosio-Ekonomi Nasional (SUSENAS) 2017</strong> oleh BPS-Statistik Indonesia</li>
                                  <li style="font-size:15px;">Proyeksi populasi dan pertumbuhan penduduk diambil dari <strong>Proyeksi Penduduk Indonesia 2017</strong> oleh BPS</li>
                                  <li style="font-size:15px;">Peta geospasial Indonesia 2013 tingkat kabupaten</li>
                              </ul>
                              
                              <h4><strong>Bidang Subjek Khusus Data</strong></h4>
                              <ul>
                                <li style="font-size:15px;">Manajemen bencana dan pengurangan risiko, kerentanan sosial</li>
                              </ul>
                      
                              <h4><strong>Cakupan Data</strong></h4>
                              <ul>
                                  <li style="font-size:15px;">Tingkat: <strong>Kabupaten/Kota</strong> (511 kabupaten/kota di Indonesia)</li>
                                  <li style="font-size:15px;">Tahun: <strong>2017</strong></li>
                              </ul>
                      
                              <h4><strong>Variabel yang Digunakan</strong></h4>
                              <ul>
                                  <li style="font-size:15px;"><strong>POVERTY (Kemiskinan)</strong>: Persentase penduduk miskin</li>
                                  <li style="font-size:15px;"><strong>NOSEWER (Tanpa Saluran Pembuangan)</strong>: Persentase rumah tangga tanpa sistem pembuangan limbah</li>
                                  <li style="font-size:15px;"><strong>TAPWATER (Air Bersih)</strong>: Persentase rumah tangga menggunakan air ledeng</li>
                                  <li style="font-size:15px;"><strong>NOELECTRIC (Tanpa Listrik)</strong>: Persentase rumah tangga tanpa listrik</li>
                                  <li style="font-size:15px;"><strong>RENTED (Menyewa Rumah)</strong>: Persentase rumah tangga penyewa</li>
                              </ul>
                      
                              <h4><strong>Metode Pengumpulan</strong></h4>
                              <ul>
                                  <li style="font-size:15px;">Metode <strong>multistage sampling</strong> dengan bobot survei</li>
                                  <li style="font-size:15px;">Data disesuaikan dengan peta geografis Indonesia tahun 2013</li>
                              </ul>
                      
                              <h4><strong>Tautan Data</strong></h4>
                              <ul>
                                  <li style="font-size:15px;">Data sosial kerentanan: <a href="https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv" target="_blank">sovi_data.csv</a></li>
                                  <li style="font-size:15px;">Matriks jarak: <a href="https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv" target="_blank">distance.csv</a></li>
                              </ul>
                      
                              <h4><strong>Catatan</strong></h4>
                              <ul>
                                  <li style="font-size:15px;">Data bersifat <strong>open access</strong> (CC BY 4.0)</li>
                                  <li style="font-size:15px;">Wilayah Maluku dan Papua cenderung memiliki nilai lebih tinggi untuk kemiskinan dan ketiadaan listrik</li>
                              </ul>
                            </blockquote>
                          </div>
                          
                          <div style="flex: 1; width: 48%; padding-right: 15px;">
                          <table style="width: 100%; border-collapse: collapse; margin-top: 20px;">
                            <tbody>
                            
                              <!-- POVERTY -->
                              <tr style="border-bottom: 2px solid #aaa;">
                                <td style="padding: 10px; width: 20%; vertical-align: middle; border-right: 2px solid #999;">
                                  <strong>POVERTY<br>(Kemiskinan)</strong>
                                </td>
                                <td style="padding: 10px;">
                                  <table style="width: 100%; border-collapse: collapse;">
                                    <tr style="border-bottom: 1px solid #ccc;">
                                      <td style="width: 30%; padding: 8px 0;"><em>Ukuran:</em></td>
                                      <td style="padding: 8px 0;">Persentase</td>
                                    </tr>
                                    <tr style="border-bottom: 1px solid #ccc;">
                                      <td style="padding: 8px 0;"><em>Satuan:</em></td>
                                      <td style="padding: 8px 0;">% penduduk</td>
                                    </tr>
                                    <tr style="border-bottom: 1px solid #ccc;">
                                      <td style="padding: 8px 0;"><em>Detail Pertanyaan:</em></td>
                                      <td style="padding: 8px 0;">Dihitung berdasarkan garis kemiskinan BPS (bukan dari pertanyaan langsung)</td>
                                    </tr>
                                    <tr>
                                      <td style="padding: 8px 0;"><em>Sumber:</em></td>
                                      <td style="padding: 8px 0;">BPS-Statistics Indonesia (2017)</td>
                                    </tr>
                                  </table>
                                </td>
                              </tr>
                              
                              <!-- TAPWATER -->
                              <tr style="border-bottom: 2px solid #aaa;">
                                <td style="padding: 10px; vertical-align: middle; border-right: 2px solid #999;">
                                  <strong>TAPWATER<br>(Akses Air Bersih)</strong>
                                </td>
                                <td style="padding: 10px;">
                                  <table style="width: 100%; border-collapse: collapse;">
                                    <tr style="border-bottom: 1px solid #ccc;">
                                      <td style="width: 30%; padding: 8px 0;"><em>Ukuran:</em></td>
                                      <td style="padding: 8px 0;">Persentase</td>
                                    </tr>
                                    <tr style="border-bottom: 1px solid #ccc;">
                                      <td style="padding: 8px 0;"><em>Satuan:</em></td>
                                      <td style="padding: 8px 0;">% rumah tangga</td>
                                    </tr>
                                    <tr style="border-bottom: 1px solid #ccc;">
                                      <td style="padding: 8px 0;"><em>Detail Pertanyaan:</em></td>
                                      <td style="padding: 8px 0;">"Apa sumber air minum utama rumah tangga ini?" (R1611A) dan "Apa sumber air untuk mandi/cuci?" (R1616A)</td>
                                    </tr>
                                    <tr>
                                      <td style="padding: 8px 0;"><em>Sumber:</em></td>
                                      <td style="padding: 8px 0;">Kuesioner SUSENAS 2017</td>
                                    </tr>
                                  </table>
                                </td>
                              </tr>
                              
                              <!-- NOELECTRIC -->
                              <tr style="border-bottom: 2px solid #aaa;">
                                <td style="padding: 10px; vertical-align: middle; border-right: 2px solid #999;">
                                  <strong>NOELECTRIC<br>(Tanpa Listrik)</strong>
                                </td>
                                <td style="padding: 10px;">
                                  <table style="width: 100%; border-collapse: collapse;">
                                    <tr style="border-bottom: 1px solid #ccc;">
                                      <td style="width: 30%; padding: 8px 0;"><em>Ukuran:</em></td>
                                      <td style="padding: 8px 0;">Persentase</td>
                                    </tr>
                                    <tr style="border-bottom: 1px solid #ccc;">
                                      <td style="padding: 8px 0;"><em>Satuan:</em></td>
                                      <td style="padding: 8px 0;">% rumah tangga</td>
                                    </tr>
                                    <tr style="border-bottom: 1px solid #ccc;">
                                      <td style="padding: 8px 0;"><em>Detail Pertanyaan:</em></td>
                                      <td style="padding: 8px 0;">"Apa sumber penerangan utama rumah tangga ini?" (R1618)</td>
                                    </tr>
                                    <tr>
                                      <td style="padding: 8px 0;"><em>Sumber:</em></td>
                                      <td style="padding: 8px 0;">Kuesioner SUSENAS 2017</td>
                                    </tr>
                                  </table>
                                </td>
                              </tr>
                              
                              <!-- NOSEWER -->
                              <tr style="border-bottom: 2px solid #aaa;">
                                <td style="padding: 10px; vertical-align: middle; border-right: 2px solid #999;">
                                  <strong>NOSEWER<br>(Tanpa Saluran Pembuangan)</strong>
                                </td>
                                <td style="padding: 10px;">
                                  <table style="width: 100%; border-collapse: collapse;">
                                    <tr style="border-bottom: 1px solid #ccc;">
                                      <td style="width: 30%; padding: 8px 0;"><em>Ukuran:</em></td>
                                      <td style="padding: 8px 0;">Persentase</td>
                                    </tr>
                                    <tr style="border-bottom: 1px solid #ccc;">
                                      <td style="padding: 8px 0;"><em>Satuan:</em></td>
                                      <td style="padding: 8px 0;">% rumah tangga</td>
                                    </tr>
                                    <tr style="border-bottom: 1px solid #ccc;">
                                      <td style="padding: 8px 0;"><em>Detail Pertanyaan:</em></td>
                                      <td style="padding: 8px 0;">"Apakah rumah ini memiliki sistem pembuangan limbah yang baik?" (R1610A)</td>
                                    </tr>
                                    <tr>
                                      <td style="padding: 8px 0;"><em>Sumber:</em></td>
                                      <td style="padding: 8px 0;">Kuesioner SUSENAS 2017</td>
                                    </tr>
                                  </table>
                                </td>
                              </tr>
                              
                              <!-- RENTED -->
                              <tr>
                                <td style="padding: 10px; vertical-align: middle; border-right: 2px solid #999;">
                                  <strong>RENTED<br>(Menyewa Rumah)</strong>
                                </td>
                                <td style="padding: 10px;">
                                  <table style="width: 100%; border-collapse: collapse;">
                                    <tr style="border-bottom: 1px solid #ccc;">
                                      <td style="width: 30%; padding: 8px 0;"><em>Ukuran:</em></td>
                                      <td style="padding: 8px 0;">Persentase</td>
                                    </tr>
                                    <tr style="border-bottom: 1px solid #ccc;">
                                      <td style="padding: 8px 0;"><em>Satuan:</em></td>
                                      <td style="padding: 8px 0;">% rumah tangga</td>
                                    </tr>
                                    <tr style="border-bottom: 1px solid #ccc;">
                                      <td style="padding: 8px 0;"><em>Detail Pertanyaan:</em></td>
                                      <td style="padding: 8px 0;">"Apa status kepemilikan rumah ini?" (R1602)</td>
                                    </tr>
                                    <tr>
                                      <td style="padding: 8px 0;"><em>Sumber:</em></td>
                                      <td style="padding: 8px 0;">Kuesioner SUSENAS 2017</td>
                                    </tr>
                                  </table>
                                </td>
                              </tr>
                            </tbody>
                          </table>
                          </div>
                        </div>
                      </div>
                  '),
              )
            ),
            
            # Manajemen Data Tab
            tabPanel(
              title = "Manajemen Data",
              value = "manajemen",
              div(class = "content-section",
                  h2("Manajemen Data"),
                  p("Manajemen Data merupakan proses penting dalam analisis untuk memastikan bahwa data yang digunakan telah siap dan sesuai untuk dianalisis lebih lanjut. Proses ini mencakup pengelompokan variabel kontinu menjadi kategorik agar lebih mudah diinterpretasikan, identifikasi nilai ekstrem atau missing value, serta penyusunan ringkasan statistik dari setiap variabel. Dengan manajemen data yang baik, interpretasi terhadap hasil analisis menjadi lebih terarah dan informatif."),
                  
                  br(),
                  p(strong(em("📖 Petunjuk Penggunaan Halaman:"))),
                  tags$ul(
                    tags$li("Tabel di bawah menampilkan ringkasan data per Kabupaten/Kota, termasuk kode distrik, nama Kabupaten/Kota, Provinsi, nilai keempat variabel indikator— POVERTY (persentase penduduk miskin), TAPWATER (persentase rumah tangga yang mendapat akses air bersih), NOSEWER (persentase rumah tangga yang tidak memiliki saluran pembuangan air limbah), NOELECTRIC (persentase rumah tangga yang tidak menggunakan listrik sebagai sumber penerangan), RENTED (persentase rumah tangga yang menyewa rumah)—dan Total Populasi."),
                    tags$li("Gunakan kolom pencarian di bagian atas setiap kolom untuk memfilter data berdasarkan kriteria spesifik."),
                    tags$li("Gunakan kotak 'Search' global di kanan atas tabel untuk mencari di semua kolom."),
                    tags$li("Klik header kolom untuk mengurutkan data secara ascending atau descending.")
                  ),
                  br(),
              ),
              
              div(class = "content-section",
                  h3("TABEL DINAMIS", style = "font-weight:800;"),
                  DT::dataTableOutput("data_tabel_dinamis")
              ),
              
              div(class = "content-section",
                  h3("TABEL DINAMIS KATEGORIK", style = "font-weight:800;"),
                  DT::dataTableOutput("categorized_kabkota_table")
              ),
              
              div(class = "content-section",
                  h3("DATA KATEGORIK: POVERTY", style = "font-weight:800;"),
                  br(),
                  p("Kategori pada variabel ini dibentuk berdasarkan pembagian kuartil dari distribusi data."),
                  br(),
                  uiOutput("poverty_slider"),
                  DT::dataTableOutput("poverty_categorical_table"),
                  br(),
                  div(
                    uiOutput("poverty_interpretation"),
                  ),
                  br()
              ),
              
              div(class = "content-section",
                  h3("DATA KATEGORIK: NOSEWER", style = "font-weight:800;"),
                  br(),
                  p("Kategori pada variabel ini dibentuk berdasarkan pembagian kuartil dari distribusi data."),
                  br(),
                  uiOutput("nosewer_slider"),
                  DT::dataTableOutput("nosewer_categorical_table"),
                  br(),
                  div(
                    uiOutput("nosewer_interpretation"),
                    style = "font-size = 12px;"
                  ),
                  br()
              ),
              
              div(class = "content-section",
                  h3("DATA KATEGORIK: TAPWATER", style = "font-weight:800;"),
                  br(),
                  p("Kategori pada variabel ini dibentuk berdasarkan pembagian kuartil dari distribusi data."),
                  br(),
                  uiOutput("tapwater_slider"),
                  DT::dataTableOutput("tapwater_categorical_table"),
                  br(),
                  div(
                    uiOutput("tapwater_interpretation"),
                    style = "font-size = 12px;"
                  ),
                  br()
              ),
              
              div(class = "content-section",
                  h3("DATA KATEGORIK: NOELECTRIC", style = "font-weight:800;"),
                  br(),
                  p("Kategori pada variabel ini dibentuk berdasarkan pembagian kuartil dari distribusi data."),
                  br(),
                  uiOutput("noelectric_slider"),
                  DT::dataTableOutput("noelectric_categorical_table"),
                  br(),
                  div(
                    uiOutput("noelectric_interpretation"),
                    style = "font-size = 12px;"
                  ),
                  br()
              ),
              
              div(class = "content-section",
                  h3("DATA KATEGORIK: RENTED", style = "font-weight:800;"),
                  br(),
                  p("Kategori pada variabel ini dibentuk berdasarkan pembagian kuartil dari distribusi data."),
                  br(),
                  uiOutput("rented_slider"),
                  DT::dataTableOutput("rented_categorical_table"),
                  br(),
                  div(
                    uiOutput("rented_interpretation"),
                    style = "font-size = 12px;"
                  )
              ),
            ),
            
            # Eksplorasi Data Tab
            tabPanel(
              title = "Eksplorasi Data",
              value = "eksplorasi",
              div(class = "content-section",
                  h2("Eksplorasi Data"),
                  p("Analisis eksplorasi data (EDA) yang mendalam untuk memahami karakteristik dan pola tersembunyi dalam dataset kerentanan sosial-ekonomi dengan visualisasi. Tujuan utamanya adalah memahami pola, pencilan, hubungan antara variabel, dan karakteristik distribusi data."),
                  br(),
                  p(strong(em("📖 Petunjuk Penggunaan Halaman:"))),
                  tags$ul(
                    tags$li("Gunakan dropdown di bawah untuk memilih salah satu variabel yang ingin dieksplorasi."),
                    tags$li("Visualisasi dan deskripsi statistik akan muncul secara otomatis berdasarkan variabel yang dipilih.")
                  )
              ),
              
              fluidRow(
                box(
                  style = "margin-top: 0px; padding: -1px;",
                  width = 12, 
                  status = "primary",
                  fluidRow(
                    box(
                      width = 12,
                      div(
                        style = "margin-bottom: 50px;",
                        selectInput(
                          inputId = "indikator",
                          label = div("📂 Pilih Variabel", style = "font-weight: bold; color: #2c3e50; font-size: 20px;"),
                          choices = c("POVERTY (Persentase Penduduk Miskin)" = "Penduduk Miskin",
                                      "TAPWATER (Persentase Rumah Tangga yang Mendapat Akses Air Bersih)" = "Rumah Tangga yang Mendapat Akses Air Bersih",
                                      "NOSEWER (Persentase Rumah Tangga yang Tidak Memiliki Saluran Pembuangan Air Limbah)" = "Rumah Tangga yang Tidak Memiliki Saluran Pembuangan Air Limbah",
                                      "NOELECTRIC (Persentase Rumah Tangga yang Tidak Menggunakan Listrik sebagai Sumber Penerangan)" = "Rumah Tangga yang Tidak Menggunakan Listrik sebagai Sumber Penerangan",
                                      "RENTED (Persentase Rumah Tangga yang Menyewa Rumah)" = "Rumah Tangga yang Menyewa Rumah"),
                          selected = "POVERTY",
                          width = "100%"
                        )
                      )
                    )
                  ),
                  
                  fluidRow(
                    div(
                      valueBoxOutput("medianBox"),
                      valueBoxOutput("maxBox"),
                      valueBoxOutput("meanBox")
                    )
                  ),
                  
                  fluidRow( 
                    style = "margin: 30px 0px;",
                    uiOutput("zeroBoxContainer")
                  ),
                  
                  fluidRow(
                    box(plotOutput("plot_eksplorasi"), width = 12)
                  ),
                  
                  br(),
                  
                  div(class = "content-section", style = "padding: 25px;",
                      fluidRow(
                        box(uiOutput("inter_plot"), width = 12)
                      )
                  ),
                  
                  fluidRow(
                    box(plotOutput("boxplot_eksplorasi"), width = 12)
                  ),
                  
                  br(),
                  
                  div(class = "content-section", style = "padding: 25px;",
                      fluidRow(
                        box(uiOutput("inter_boxplot"), width = 12)
                      )
                  ),
                  
                  div(class = "content-section",
                      h2("Peta Distribusi"),
                      leafletOutput("distribusi_peta", height = 600),
                      br(),
                      
                      downloadButton("downloadMapPNG", "Unduh Peta (PNG)", class = "btn-success", icon = icon("download")),
                      br(),
                      br(),
                      uiOutput("interpretasi_distribusi_peta"),
                  )
                )
              )
            ),
            
            # Uji Beda Rata-Rata
            tabPanel(
              title = "Uji Beda Rata-Rata",
              value = "rata2",
              div(class = "content-section",
                  h2("Uji Beda Rata-Rata"),
                  p("Bagian ini memungkinkan Anda untuk melakukan uji beda rata-rata (t-test) untuk membandingkan rata-rata tingkat kemiskinan (POVERTY) antara dua kelompok yang dibuat berdasarkan variabel independen yang dipilih."),
                  p("Untuk tujuan uji ini, variabel independen (RENTED, NOSEWER, NOELECTRIC, TAPWATER) akan dikategorikan menjadi dua kelompok (misalnya, di bawah median dan di atas median) untuk memfasilitasi perbandingan rata-rata POVERTY."),
              ),
              
              div(class = "content-section",
                  fluidRow(
                    column(8,
                           selectInput("mean_diff_var", "Pilih Variabel Independen:",
                                       choices = c("RENTED", "NOSEWER", "NOELECTRIC", "TAPWATER"))
                    ),
                    column(4,
                           br(),
                           actionButton("run_mean_diff", "Jalankan Uji Beda Rata-Rata")
                    )
                  )
              ),
              
              div(class = "content-section",
                  h3("Hasil Uji Beda Rata-Rata", style = "font-weight: 700 !important;"),
                  verbatimTextOutput("mean_diff_output"),
                  plotOutput("mean_diff_plot")
              ),
              
              div(class = "content-section",
                  uiOutput("inter_uji_rata_rata")
              )
            ),
            
            # Uji Proporsi dan Uji Varians
            tabPanel(
              title = "Uji Proporsi & Uji Varians",
              value = "proporsi",
              div(class = "content-section",
                  h2("Uji Proporsi & Uji Varians"),
                  p("Bagian ini memungkinkan Anda untuk melakukan uji proporsi dan uji varians."),
                  p("Untuk uji proporsi, Anda dapat membandingkan proporsi distrik dengan karakteristik tertentu (misalnya, proporsi distrik dengan tingkat rumah sewaan tinggi) antara kelompok kemiskinan tinggi dan rendah. Untuk uji varians, Anda dapat membandingkan apakah varians POVERTY berbeda antara dua kelompok atau varians dari variabel independen yang dipilih berbeda antara dua kelompok."),
              ),
              
              div(class = "content-section",
                  h3("Uji Proporsi", style = "font-weight: 700;"),
                  br(),
                  fluidRow(
                    column(8,
                           numericInput("prop_threshold", "Ambang Batas untuk Proporsi (misal: 50 untuk 50%):", value = 50, min = 0, max = 100),
                           p("Catatan: Tingkat kemiskinan (POVERTY) akan dikategorikan menjadi 'Rendah' (<= median) dan 'Tinggi' (> median) untuk uji proporsi ini.")
                    ),
                    
                    column(4,
                           selectInput("prop_var", "Pilih Variabel Independen untuk Uji Proporsi:",
                                       choices = c("RENTED", "NOSEWER", "NOELECTRIC", "TAPWATER")),
                           actionButton("run_prop_test", "Jalankan Uji Proporsi")
                    )
                  ),
              ),
              
              div(class = "content-section",
                  h3("Hasil Uji Proporsi", style = "font-weight: 700 !important;"),
                  verbatimTextOutput("prop_test_output"),
              ),
              
              div(class = "content-section",
                  uiOutput("inter_uji_proporsi")
              ),
              
              div(class = "content-section",
                  h3("Uji Varians (F-test)", style = "font-weight: 700;"),
                  br(),
                  fluidRow(
                    column(8,
                           selectInput("var_test_var", "Pilih Variabel untuk Uji Varians:",
                                       choices = c("POVERTY", "RENTED", "NOSEWER", "NOELECTRIC", "TAPWATER")),
                           p("Catatan: Jika POVERTY dipilih, pengelompokan akan berdasarkan median dari variabel independen yang dipilih. Jika variabel independen dipilih, pengelompokan akan berdasarkan median POVERTY.")
                    ),
                    column(4,
                           selectInput("var_test_group_var", "Pilih Variabel Pengelompokan (untuk dikategorikan menjadi dua grup):",
                                       choices = c("RENTED", "NOSEWER", "NOELECTRIC", "TAPWATER", "POVERTY")),
                           actionButton("run_var_test", "Jalankan Uji Varians")
                    )
                  ),
              ),
              
              div(class = "content-section",
                  h3("Hasil Uji Varians", style = "font-weight: 700 !important;"),
                  verbatimTextOutput("var_test_output")
              ),
              
              div(class = "content-section",
                  uiOutput("inter_uji_varians")
              )
            ),
            
            # ANOVA
            tabPanel(
              title = "ANOVA",
              value = "anova",
              div(class = "content-section",
                  h2("Uji ANOVA"),
                  p("Bagian ini memungkinkan Anda untuk melakukan Analisis Varians (ANOVA) untuk menguji apakah ada perbedaan yang signifikan pada rata-rata tingkat kemiskinan (POVERTY) di antara kelompok-kelompok yang dibuat berdasarkan kategori variabel independen."),
                  p("Variabel independen yang dipilih akan dikategorikan menjadi beberapa level (misalnya, kuartil) untuk analisis ANOVA."),
              ),
              
              div(class = "content-section",
                  fluidRow(
                    column(4,
                           selectInput("anova_var", "Pilih Variabel Independen untuk ANOVA:",
                                       choices = c("RENTED", "NOSEWER", "NOELECTRIC", "TAPWATER"))
                    ),
                    column(4,
                           numericInput("anova_groups", "Jumlah Kelompok (misal: 3 untuk tertile, 4 untuk kuartil):", value = 3, min = 2, step = 1)
                    ),
                    column(4,
                           br(),
                           actionButton("run_anova", "Jalankan Uji ANOVA")
                    )
                  ),
              ),
              
              div(class = "content-section",
                  h3("Hasil Uji ANOVA", style = "font-weight: 700 !important;"),
                  verbatimTextOutput("anova_output"),
                  plotOutput("anova_plot")
              ),
              
              div(class = "content-section",
                  uiOutput("inter_uji_anova")
              )
            ),
            
            # Analisis Regresi
            tabPanel(
              title = "Analisis Regresi",
              value = "regresi",
              div(class = "content-section",
                  h2("Analisis Regresi dan Uji Asumsi"),
                  p("Bagian ini didedikasikan untuk melakukan analisis regresi, sebuah metode statistik yang kuat untuk memodelkan hubungan antara tingkat kemiskinan (POVERTY) sebagai variabel dependen dan faktor-faktor sosial-ekonomi seperti persentase rumah tangga yang menyewa rumah (RENTED), persentase rumah tangga yang tidak memiliki saluran pembuangan air limbah (NOSEWER), persentase rumah tangga yang tidak menggunakan listrik sebagai sumber penerangan (NOELECTRIC), dan persentase rumah tangga yang mendapat akses air bersih (TAPWATER) sebagai variabel independen."),
                  p("Selain itu, bagian ini juga mencakup berbagai uji asumsi klasik yang penting untuk memastikan validitas dan reliabilitas model regresi. Uji asumsi akan membantu kita memahami apakah data memenuhi prasyarat seperti normalitas residual, homoskedastisitas, non-multikolinearitas, dan tidak adanya autokorelasi, sehingga hasil regresi dapat diinterpretasikan dengan benar."),
                  
              ),
              
              fluidRow(
                box(
                  div(class = "content-section",
                      h2("Ringkasan Hasil Regresi", style = "margin-top: 5px;"),
                      verbatimTextOutput("summary_model")
                  )
                ),
                
                box(
                  div(class = "content-section",
                      h2("Interpretasi Regresi", style = "margin-top: 5px;"),
                      uiOutput("inter_regresi")
                  )
                )
              ),
              
              div(class = "content-section",
                  h2("Ringkasan Pemeriksaan Uji Asumsi Klasik Regresi", style = "margin-top: 5px;"),
                  verbatimTextOutput("overall_check")
              ),
              
              fluidRow(
                box(
                  div(class = "content-section",
                      h2("Normalitas Error", style = "margin-top: 5px;"),
                      verbatimTextOutput("assumption1")
                  )
                ),
                
                box(
                  div(class = "content-section",
                      h2("Homoskedastisitas", style = "margin-top: 5px;"),
                      verbatimTextOutput("assumption2")
                  )
                )
              ),
              
              fluidRow(
                box(
                  div(class = "content-section",
                      h2("Multikolinearitas", style = "margin-top: 5px;"),
                      verbatimTextOutput("assumption3")
                  )
                ),
                
                box(
                  div(class = "content-section",
                      h2("Autokorelasi", style = "margin-top: 5px;"),
                      verbatimTextOutput("assumption4")
                  )
                )
              )
            )
          )
      )
  )
)

# Global reactiveValues untuk menyimpan semua data yang akan dimasukkan ke laporan RMD
global_report_data <- reactiveValues(
  mean_diff_result = NULL,
  chi_sq_result = NULL,
  var_test_result = NULL,
  anova_result = NULL,
  regression_result = NULL, # Untuk hasil regresi
  current_inputs = list() # Untuk menyimpan nilai input UI saat ini
)

server <- function(input, output, session) {
  
  # Manajemen Data
  library(DT)
  
  output$data_tabel_dinamis <- DT::renderDataTable({
    data_tampil <- data %>%
      select(
        `Kode Distrik` = DISTRICTCODE,
        `Kab/Kota` = KABKOTA,
        `Provinsi` = PROV,
        Kemiskinan = POVERTY,
        `% RT dgn Akses Air Bersih` = TAPWATER,
        `% RT Tanpa Saluran Limbah` = NOSEWER,
        `% RT Tanpa Listrik` = NOELECTRIC,
        `% RT Menyewa Rumah` = RENTED,
        `Populasi` = POPULATION
      )
    
    DT::datatable(
      data_tampil,
      options = list(
        pageLength = 10, # Jumlah baris default per halaman
        lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')), # Pilihan jumlah baris per halaman
        scrollX = TRUE, # Aktifkan scroll horizontal jika tabel lebar
        dom = 'lfrtipB', # Urutan elemen: Length, Filtering, Row info, Table, Paging
        buttons = list(
          'copy',
          'csv',
          'excel',
          'pdf',
          'print'
        )
      ),
      filter = 'top', # Menampilkan kotak pencarian di bagian atas setiap kolom
      rownames = FALSE, # Jangan tampilkan nomor baris
      class = 'display compact' # Kelas CSS untuk tampilan tabel
    )
  })
  
  library(tidyr)
  
  # === FUNGSI BANTU UNTUK KATEGORISASI DENGAN BATAS TETAP (TANPA POPULASI) ===
  # Parameter total_pop_vec dihapus
  categorize_data_fixed <- function(data_vec, breaks, labels) {
    
    # Hilangkan NA hanya dari data_vec
    data_vec_clean <- data_vec[!is.na(data_vec)]
    
    # --- DEBUGGING: Periksa data setelah NA dihilangkan ---
    print(paste("DEBUG: Running categorize_data_fixed for variable with breaks starting:", head(breaks, 1)))
    print(paste("DEBUG: Length of data_vec_clean (after NA removal):", length(data_vec_clean)))
    
    if (length(data_vec_clean) == 0) {
      print("DEBUG: CRITICAL ERROR: data_vec_clean is EMPTY after NA removal. Returning NULL.")
      return(NULL) # Tidak bisa kategorisasi tanpa data
    }
    
    if (length(breaks) != length(labels) + 1) {
      print(paste("DEBUG: Mismatch between breaks and labels. Breaks length:", length(breaks), "Labels length:", length(labels)))
      print("DEBUG: Returning NULL from categorize_data_fixed due to breaks/labels mismatch.")
      return(NULL) 
    }
    
    sorted_breaks <- sort(breaks)
    print(paste("DEBUG: Sorted breaks used for cut:", paste(sorted_breaks, collapse = ", ")))
    
    if (min(data_vec_clean, na.rm=TRUE) < min(sorted_breaks) || max(data_vec_clean, na.rm=TRUE) > max(sorted_breaks)) {
      print("DEBUG WARNING: Data values are outside the specified breaks range. This might lead to NA categories.")
      print(paste("Min clean data:", min(data_vec_clean, na.rm=TRUE)))
      print(paste("Max clean data:", max(data_vec_clean, na.rm=TRUE)))
    }
    
    kategori_vector <- tryCatch({
      cut(data_vec_clean,
          breaks = sorted_breaks,
          labels = labels,
          include.lowest = TRUE,
          right = FALSE
      )
    }, error = function(e) {
      print(paste("DEBUG ERROR in cut():", e$message))
      return(NULL)
    })
    
    if (is.null(kategori_vector)) {
      print("DEBUG: Kategori vector is NULL after cut() operation. Returning NULL.")
      return(NULL)
    }
    
    # Membuat dataframe sementara untuk grouping (hanya berdasarkan kategori)
    temp_df <- data.frame(kategori = kategori_vector, stringsAsFactors = FALSE)
    
    # Menghitung frekuensi dan persentase (hanya berdasarkan Jumlah Kab/Kota)
    df_kategorik <- temp_df %>%
      group_by(kategori) %>%
      summarise(
        `Jumlah Kab/Kota` = n()
      ) %>%
      ungroup()
    
    # Hitung persentase secara keseluruhan
    total_kabkota <- sum(df_kategorik$`Jumlah Kab/Kota`, na.rm = TRUE)
    
    df_kategorik <- df_kategorik %>%
      mutate(
        `Persentase Kab/Kota` = if (total_kabkota > 0) (`Jumlah Kab/Kota` / total_kabkota) * 100 else 0
        # `Total Populasi` dan `Persentase Populasi` dihapus
      ) %>%
      arrange(factor(kategori, levels = labels))
    
    print("DEBUG: categorize_data_fixed returning successful df_kategorik.")
    return(df_kategorik)
  }
  
  # === FUNGSI BANTU UNTUK INTERPRETASI OTOMATIS (TANPA POPULASI) ===
  # Parameter total_pop_vec dan segala referensi populasi dihapus
  generate_interpretation_fixed <- function(df_kategorik, variable_name, category_labels) {
    # Hanya cek Jumlah Kab/Kota, tidak perlu cek Total Populasi
    if (is.null(df_kategorik) || nrow(df_kategorik) == 0 || sum(df_kategorik$`Jumlah Kab/Kota`) == 0) {
      return(HTML(paste0("<p><i>Tidak ada data untuk interpretasi ", variable_name, ".</i></p>")))
    }
    
    narasi <- paste0("<h3> </h3>")
    narasi <- paste0(narasi, "<p>Berdasarkan kategorisasi ke dalam level Rendah, Moderat, Tinggi, dan Sangat Tinggi:</p>")
    
    # Pastikan semua kategori ada, jika tidak ada data di kategori tertentu, set 0
    full_df_kategorik <- data.frame(kategori = factor(category_labels, levels = category_labels)) %>%
      left_join(df_kategorik, by = "kategori") %>%
      mutate(
        `Jumlah Kab/Kota` = replace_na(`Jumlah Kab/Kota`, 0),
        `Persentase Kab/Kota` = replace_na(`Persentase Kab/Kota`, 0)
        # `Total Populasi` dan `Persentase Populasi` dihapus
      )
    
    # Menemukan kategori dengan persentase kab/kota tertinggi
    highest_kabkota_cat <- full_df_kategorik %>%
      filter(`Jumlah Kab/Kota` > 0) %>% 
      arrange(desc(`Persentase Kab/Kota`)) %>%
      slice(1)
    
    # Total kab/kota (tetap diperlukan untuk persentase)
    total_kabkota_all <- sum(full_df_kategorik$`Jumlah Kab/Kota`)
    
    narasi <- paste0(narasi, "<ul>")
    
    for (i in 1:nrow(full_df_kategorik)) {
      kategori_nama <- full_df_kategorik$kategori[i]
      persen_kabkota <- round(full_df_kategorik$`Persentase Kab/Kota`[i], 1)
      jumlah_kabkota <- full_df_kategorik$`Jumlah Kab/Kota`[i]
      
      narasi <- paste0(narasi, "<li>Untuk kategori <b>", kategori_nama, "</b>, terdapat <b>",
                       jumlah_kabkota, "</b> kabupaten/kota (", persen_kabkota, "%) yang berada pada level ini.</li>")
    }
    narasi <- paste0(narasi, "</ul>")
    
    # Kesimpulan Umum
    if (nrow(highest_kabkota_cat) > 0) {
      narasi <- paste0(narasi, "<p>Secara keseluruhan, mayoritas kabupaten/kota (", round(highest_kabkota_cat$`Persentase Kab/Kota`, 1), "%) berada pada kategori <b>", highest_kabkota_cat$kategori, "</b> untuk ", tolower(variable_name), ".</p>")
    } else {
      narasi <- paste0(narasi, "<p><i>(Tidak ada kategori mayoritas kabupaten/kota yang ditemukan.)</i></p>")
    }
    
    # Bagian populasi dihapus sepenuhnya
    # narasi <- paste0(narasi, "</p>") # Jika tidak ada populasi, baris ini mungkin tidak diperlukan
    
    return(HTML(narasi))
  }
  
  # === DEFINISI BATAS DAN LABEL UNTUK SETIAP VARIABEL ===
  # Gunakan konfigurasi 5 breaks, 4 labels seperti yang sudah benar secara struktural
  # POVERTY
  poverty_breaks <- c(0, 7.3, 11.1, 16, Inf)
  poverty_labels <- c("Rendah (0—7.3)", "Moderat (>7.3—11.1)", "Tinggi (>11.1—16)", "Sangat Tinggi (>16)")
  
  # TAPWATER
  tapwater_breaks <- c(0, 5.3, 13, 25.7, Inf)
  tapwater_labels <- c("Rendah (0—5.3)", "Moderat (>5.3—13)", "Tinggi (>13—25.7)", "Sangat Tinggi (>25.7)")
  
  # NOSEWER
  nosewer_breaks <- c(0, 6.5, 14.4, 25.7, Inf)
  nosewer_labels <- c("Rendah (0—6.5)", "Moderat (>6.5—14.4)", "Tinggi (>14.4—25.7)", "Sangat Tinggi (>25.7)")
  
  # NOELECTRIC
  noelectric_breaks <- c(0, 0.1, 0.9, 4.4, Inf)
  noelectric_labels <- c("Rendah (0—0.1)", "Moderat (>0.1—0.9)", "Tinggi (>0.9—4.4)", "Sangat Tinggi (>4.4)")
  
  # RENTED
  rented_breaks <- c(0, 1.7, 4, 9.3, Inf)
  rented_labels <- c("Rendah (0—1.7)", "Moderat (>1.7—4)", "Tinggi (>4—9.3)", "Sangat Tinggi (>9.3)")
  
  
  # === OUTPUT TABEL DAN INTERPRETASI UNTUK SETIAP VARIABEL ===
  
  # POVERTY
  output$poverty_categorical_table <- DT::renderDataTable({
    poverty_data_col <- data$POVERTY # Cukup data$POVERTY
    
    df_kategorik <- categorize_data_fixed(poverty_data_col, poverty_breaks, poverty_labels) # Parameter populasi dihapus
    
    if (is.null(df_kategorik)) {
      return(datatable(data.frame(Pesan = "Tidak ada data yang valid untuk variabel Kemiskinan atau batas kategori tidak sesuai."), 
                       options = list(dom = 'tB', paging = FALSE, searching = FALSE), rownames = FALSE))
    }
    DT::datatable(df_kategorik, options = list(dom = 'tB', paging = FALSE, searching = FALSE, buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), extensions = 'Buttons', rownames = FALSE)
  })
  
  output$poverty_interpretation <- renderUI({
    poverty_data_col <- data$POVERTY # Cukup data$POVERTY
    
    df_kategorik <- categorize_data_fixed(poverty_data_col, poverty_breaks, poverty_labels) # Parameter populasi dihapus
    generate_interpretation_fixed(df_kategorik, "Kemiskinan", poverty_labels)
  })
  
  # TAPWATER
  output$tapwater_categorical_table <- DT::renderDataTable({
    tapwater_data_col <- data$TAPWATER # Cukup data$TAPWATER
    
    df_kategorik <- categorize_data_fixed(tapwater_data_col, tapwater_breaks, tapwater_labels) # Parameter populasi dihapus
    if (is.null(df_kategorik)) {
      return(datatable(data.frame(Pesan = "Tidak ada data yang valid untuk variabel Akses Air Bersih atau batas kategori tidak sesuai."), 
                       options = list(dom = 'tB', paging = FALSE, searching = FALSE), rownames = FALSE))
    }
    DT::datatable(df_kategorik, options = list(dom = 'tB', paging = FALSE, searching = FALSE, buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), extensions = 'Buttons', rownames = FALSE)
  })
  
  output$tapwater_interpretation <- renderUI({
    tapwater_data_col <- data$TAPWATER # Cukup data$TAPWATER
    
    df_kategorik <- categorize_data_fixed(tapwater_data_col, tapwater_breaks, tapwater_labels) # Parameter populasi dihapus
    generate_interpretation_fixed(df_kategorik, "Akses Air Bersih", tapwater_labels)
  })
  
  # NOSEWER
  output$nosewer_categorical_table <- DT::renderDataTable({
    nosewer_data_col <- data$NOSEWER
    
    df_kategorik <- categorize_data_fixed(nosewer_data_col, nosewer_breaks, nosewer_labels)
    if (is.null(df_kategorik)) {
      return(datatable(data.frame(Pesan = "Tidak ada data yang valid untuk variabel Tanpa Saluran Limbah atau batas kategori tidak sesuai."), 
                       options = list(dom = 'tB', paging = FALSE, searching = FALSE), rownames = FALSE))
    }
    DT::datatable(df_kategorik, options = list(dom = 'tB', paging = FALSE, searching = FALSE, buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), extensions = 'Buttons', rownames = FALSE)
  })
  
  output$nosewer_interpretation <- renderUI({
    nosewer_data_col <- data$NOSEWER
    
    df_kategorik <- categorize_data_fixed(nosewer_data_col, nosewer_breaks, nosewer_labels)
    generate_interpretation_fixed(df_kategorik, "Tanpa Saluran Limbah", nosewer_labels)
  })
  
  # NOELECTRIC
  output$noelectric_categorical_table <- DT::renderDataTable({
    noelectric_data_col <- data$NOELECTRIC
    
    df_kategorik <- categorize_data_fixed(noelectric_data_col, noelectric_breaks, noelectric_labels)
    if (is.null(df_kategorik)) {
      return(datatable(data.frame(Pesan = "Tidak ada data yang valid untuk variabel Tanpa Listrik atau batas kategori tidak sesuai."), 
                       options = list(dom = 'tB', paging = FALSE, searching = FALSE), rownames = FALSE))
    }
    DT::datatable(df_kategorik, options = list(dom = 'tB', paging = FALSE, searching = FALSE, buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), extensions = 'Buttons', rownames = FALSE)
  })
  
  output$noelectric_interpretation <- renderUI({
    noelectric_data_col <- data$NOELECTRIC
    
    df_kategorik <- categorize_data_fixed(noelectric_data_col, noelectric_breaks, noelectric_labels)
    generate_interpretation_fixed(df_kategorik, "Tanpa Listrik", noelectric_labels)
  })
  
  # RENTED
  output$rented_categorical_table <- DT::renderDataTable({
    rented_data_col <- data$RENTED
    
    df_kategorik <- categorize_data_fixed(rented_data_col, rented_breaks, rented_labels)
    if (is.null(df_kategorik)) {
      return(datatable(data.frame(Pesan = "Tidak ada data yang valid untuk variabel Menyewa Rumah atau batas kategori tidak sesuai."), 
                       options = list(dom = 'tB', paging = FALSE, searching = FALSE), rownames = FALSE))
    }
    DT::datatable(df_kategorik, options = list(dom = 'tB', paging = FALSE, searching = FALSE, buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), extensions = 'Buttons', rownames = FALSE)
  })
  
  output$rented_interpretation <- renderUI({
    rented_data_col <- data$RENTED
    
    df_kategorik <- categorize_data_fixed(rented_data_col, rented_breaks, rented_labels)
    generate_interpretation_fixed(df_kategorik, "Menyewa Rumah", rented_labels)
  })
  
  # --- DEFINISI BATAS DAN LABEL UNTUK SETIAP VARIABEL (HARUS SAMA DENGAN YG DIGUNAKAN UNTUK TABEL KATEGORIK DI BAWAH) ---
  # POVERTY
  poverty_breaks <- c(0, 7.3, 11.1, 16, Inf)
  poverty_labels <- c("Rendah", "Moderat", "Tinggi", "Sangat Tinggi")
  
  # TAPWATER
  tapwater_breaks <- c(0, 5.3, 13, 25.7, Inf)
  tapwater_labels <- c("Rendah", "Moderat", "Tinggi", "Sangat Tinggi")
  
  # NOSEWER
  nosewer_breaks <- c(0, 6.5, 14.4, 25.7, Inf)
  nosewer_labels <- c("Rendah", "Moderat", "Tinggi", "Sangat Tinggi")
  
  # NOELECTRIC
  noelectric_breaks <- c(0, 0.1, 0.9, 4.4, Inf)
  noelectric_labels <- c("Rendah", "Moderat", "Tinggi", "Sangat Tinggi")
  
  # RENTED
  rented_breaks <- c(0, 1.7, 4, 9.3, Inf)
  rented_labels <- c("Rendah", "Moderat", "Tinggi", "Sangat Tinggi")
  
  # Mapping untuk akses mudah
  variable_categorization_specs <- list(
    "POVERTY" = list(breaks = poverty_breaks, labels = poverty_labels),
    "TAPWATER" = list(breaks = tapwater_breaks, labels = tapwater_labels),
    "NOSEWER" = list(breaks = nosewer_breaks, labels = nosewer_labels),
    "NOELECTRIC" = list(breaks = noelectric_breaks, labels = noelectric_labels),
    "RENTED" = list(breaks = rented_breaks, labels = rented_labels)
  )
  
  reactive_categorized_data_table <- reactive({
    df_temp <- data %>%
      select(DISTRICTCODE, KABKOTA, PROV, POPULATION, POVERTY, TAPWATER, NOSEWER, NOELECTRIC, RENTED) 
    
    # Debugging: Periksa apakah ada NA di kolom numerik sebelum cut()
    print("NA counts before categorization:")
    print(sapply(df_temp[, c("POVERTY", "TAPWATER", "NOSEWER", "NOELECTRIC", "RENTED")], function(x) sum(is.na(x))))
    
    # Debugging: Periksa tipe data
    print("Class of columns before categorization:")
    print(sapply(df_temp[, c("POVERTY", "TAPWATER", "NOSEWER", "NOELECTRIC", "RENTED")], class))
    
    df_result <- df_temp %>%
      mutate(
        POVERTY_CAT = cut(POVERTY, breaks = poverty_breaks, labels = poverty_labels, include.lowest = TRUE, right = FALSE, ordered_result = TRUE),
        TAPWATER_CAT = cut(TAPWATER, breaks = tapwater_breaks, labels = tapwater_labels, include.lowest = TRUE, right = FALSE, ordered_result = TRUE),
        NOSEWER_CAT = cut(NOSEWER, breaks = nosewer_breaks, labels = nosewer_labels, include.lowest = TRUE, right = FALSE, ordered_result = TRUE),
        NOELECTRIC_CAT = cut(NOELECTRIC, breaks = noelectric_breaks, labels = noelectric_labels, include.lowest = TRUE, right = FALSE, ordered_result = TRUE),
        RENTED_CAT = cut(RENTED, breaks = rented_breaks, labels = rented_labels, include.lowest = TRUE, right = FALSE, ordered_result = TRUE)
      ) %>%
      
      select(
        `Kode Distrik` = DISTRICTCODE,
        `Kab/Kota` = KABKOTA,
        `Provinsi` = PROV,
        `Kategori Kemiskinan` = POVERTY_CAT,
        `Kategori Akses Air Bersih` = TAPWATER_CAT,
        `Kategori Tanpa Saluran Limbah` = NOSEWER_CAT,
        `Kategori Tanpa Listrik` = NOELECTRIC_CAT,
        `Kategori Menyewa Rumah` = RENTED_CAT
      )
    
    # Debugging: Periksa hasil akhir dataframe
    print("Head of categorized data_table:")
    print(head(df_result))
    print("Summary of categorized data_table:")
    print(summary(df_result)) # Lihat apakah ada banyak NA di kolom _CAT
    print(paste("Number of rows in categorized data_table:", nrow(df_result)))
    
    return(df_result)
  })
  
  # === OUTPUT TABEL DINAMIS KATEGORIK PER KABUPATEN/KOTA (BARU) ===
  output$categorized_kabkota_table <- DT::renderDataTable({
    df_display_categorized <- reactive_categorized_data_table()
    
    # Pastikan df_display_categorized tidak NULL atau kosong
    if (is.null(df_display_categorized) || nrow(df_display_categorized) == 0) {
      return(datatable(data.frame(Pesan = "Tidak ada data kategorik untuk ditampilkan."), 
                       options = list(dom = 'tB', paging = FALSE, searching = FALSE), rownames = FALSE))
    }
    
    DT::datatable(df_display_categorized,
                  filter = 'top', # Filter di bagian atas kolom
                  options = list(
                    scrollX = TRUE, # Memungkinkan scroll horizontal jika tabel lebar
                    pageLength = 10,
                    lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
                    dom = 'lfrtipB', # Menampilkan tombol salin/cetak/excel
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    columnDefs = list(list(className = 'dt-center', targets = '_all'))
                  ),
                  extensions = 'Buttons',
                  rownames = FALSE
    )
  })
  
  # EKSPLORASI DATA
  library(dplyr)
  
  # Fungsi reaktif untuk mendapatkan data yang bersih dan informasi lengkap
  get_cleaned_data_info <- reactive({
    label_terpilih <- input$indikator
    nama_kolom_data <- switch(label_terpilih,
                              "Penduduk Miskin" = "POVERTY",
                              "Rumah Tangga yang Mendapat Akses Air Bersih" = "TAPWATER",
                              "Rumah Tangga yang Tidak Memiliki Saluran Pembuangan Air Limbah"  = "NOSEWER",
                              "Rumah Tangga yang Tidak Menggunakan Listrik sebagai Sumber Penerangan" = "NOELECTRIC",
                              "Rumah Tangga yang Menyewa Rumah" = "RENTED"
    )
    
    df_variabel_full <- data[!is.na(data[[nama_kolom_data]]), 
                             c("DISTRICTCODE", "KABKOTA", "PROV", nama_kolom_data)]
    
    # Mengubah nama kolom indikator agar mudah diakses
    names(df_variabel_full)[which(names(df_variabel_full) == nama_kolom_data)] <- "nilai_indikator"
    
    # Pisahkan data untuk zero-box, tapi main data tetap full
    df_zero <- df_variabel_full %>% filter(nilai_indikator == 0)
    
    return(list(
      nilai_bersih = df_variabel_full$nilai_indikator, # Ini akan mencakup nilai nol
      df_for_extremes = df_variabel_full, # Ini juga akan mencakup nilai nol
      df_zero_values = df_zero, # Ini khusus untuk box nilai nol
      nama_kolom = nama_kolom_data,
      label_indikator = label_terpilih
    ))
  })
  
  output$maxBox <- renderValueBox({
    data_info <- get_cleaned_data_info()
    nilai <- data_info$nilai_bersih
    label_indikator <- data_info$label_indikator
    
    if (length(nilai) > 0) {
      max_val <- max(nilai, na.rm = TRUE)
      
      df_max <- data_info$df_for_extremes[data_info$df_for_extremes$nilai_indikator == max_val,]
      
      kabkot_max_names <- if(nrow(df_max) > 0) {
        paste(unique(df_max$KABKOTA), collapse = ", ")
      } else {""}
      
      prov_max_names <- if(nrow(df_max) > 0) {
        paste(unique(df_max$PROV), collapse = ", ")
      } else {""}
      
      subtitle_text <- paste0("Persentase ", label_indikator, " Tertinggi")
      if (kabkot_max_names != "") {
        subtitle_text <- paste0(subtitle_text, "<br>Kab/Kota: ", kabkot_max_names)
        if (prov_max_names != "") {
          subtitle_text <- paste0(subtitle_text, " (Provinsi: ", prov_max_names, ")")
        }
      }
      
      valueBox(
        value = tags$p(paste0(round(max_val, 2), "%"), style = "font-size: 200%;"),
        subtitle = HTML(subtitle_text),
        icon = icon("arrow-up"),
        color = "yellow"
      )
    } else {
      valueBox(
        value = tags$p("N/A", style = "font-size: 200%;"),
        subtitle = "Tidak Ada Data",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    }
  })
  
  output$meanBox <- renderValueBox({
    data_info <- get_cleaned_data_info()
    nilai <- data_info$nilai_bersih
    label_indikator <- data_info$label_indikator
    
    if (length(nilai) > 0) {
      mean_val <- mean(nilai, na.rm = TRUE)
      valueBox(
        value = tags$p(paste0(round(mean_val, 2), "%"), style = "font-size: 200%;"),
        subtitle = HTML(paste0("Rata-rata Persentase ", label_indikator, "<br>Antar Kab/Kota")),
        icon = icon("calculator"),
        color = "green"
      )
    } else {
      valueBox(
        value = tags$p("N/A", style = "font-size: 200%;"),
        subtitle = "Tidak Ada Data",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    }
  })
  
  output$medianBox <- renderValueBox({
    data_info <- get_cleaned_data_info()
    nilai <- data_info$nilai_bersih
    label_indikator <- data_info$label_indikator
    
    if (length(nilai) > 0) {
      median_val <- median(nilai, na.rm = TRUE)
      valueBox(
        value = tags$p(paste0(round(median_val, 2), "%"), style = "font-size: 200%;"),
        subtitle = HTML(paste0("Median Persentase ", label_indikator, "<br>Antar Kab/Kota")),
        icon = icon("chart-line"),
        color = "blue"
      )
    } else {
      valueBox(
        value = tags$p("N/A", style = "font-size: 200%;"),
        subtitle = "Tidak Ada Data",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    }
  })
  
  output$zeroBoxContainer <- renderUI({
    data_info <- get_cleaned_data_info()
    df_zero <- data_info$df_zero_values
    label_indikator <- data_info$label_indikator
    
    if (nrow(df_zero) > 0) {
      list_kabkot_zero <- paste(unique(df_zero$KABKOTA), collapse = ", ")
      
      # Membangun struktur HTML secara manual
      tags$div(
        class = "zero-box-content",
        style = "padding: 10px; background-color: #FF4500; color: white;",
        tags$h3(paste0(nrow(df_zero), " Kab/Kota Memiliki Persentase 0%"), style = "text-align: center;"),
        tags$p(
          HTML(paste0("Untuk ", label_indikator, ":")), 
          style = "text-align: center; margin-top: 10px; font-weight: bold;"
        ),
        tags$p(
          HTML(list_kabkot_zero), 
          style = "font-size: 0.9em; text-align: center; padding: 0 10px;"
        ),
        tags$p(
          HTML("<i>Kab/Kota dengan persentase bernilai nol mengindikasikan dua kemungkinan: (1) kondisi aktual, atau (2) kesalahan entri data atau potensi data tidak tersedia</i>"),
          style = "font-size: 0.9em; text-align: center; font-weight: 800; color: #FFD700; margin-top: 15px;"
        ),
        tags$div(
          class = "zero-box-icon",
          icon("exclamation-circle"),
          style = "font-size: 2em; color: #FFD700; text-align: center; margin-bottom: 20px;"
        )
      )
    } else {
      tags$div(
        class = "zero-box-content-empty", style = "margin: 20px 0px;",
        tags$h3("0 Kab/Kota", style = "text-align: center;"),
        tags$p(
          HTML(paste0("Tidak Ada Kab/Kota Bernilai 0%<br>Untuk ", label_indikator)),
          style = "text-align: center; margin-top: 10px; font-weight: 800;"
        ),
      )
    }
  })
  
  output$plot_eksplorasi <- renderPlot({
    data_info <- get_cleaned_data_info()
    nilai <- data_info$nilai_bersih
    label_indikator <- data_info$label_indikator
    plot_label_x <- switch(label_indikator,
                           "Kemiskinan" = "Persentase Kemiskinan (%)",
                           "Akses Air Bersih" = "Persentase Akses Air Bersih (%)",
                           "Tanpa Saluran Limbah" = "Persentase Rumah Tangga Tanpa Saluran Limbah (%)",
                           "Tanpa Listrik" = "Persentase Rumah Tangga Tanpa Listrik (%)",
                           "Menyewa Rumah" = "Persentase Rumah Tangga Menyewa Rumah (%)"
    )
    
    print(paste0("Panjang data setelah NA dihapus (plot): ", length(nilai)))
    print(paste0("Jumlah nilai unik setelah NA dihapus (plot): ", length(unique(nilai))))
    print("Data aktual setelah na.omit():")
    print(nilai)
    
    if (length(unique(nilai)) > 1) {
      hist(nilai,
           main = paste("Distribusi", label_indikator, "per Kab/Kota"),
           xlab = plot_label_x,                       
           col = "red", border = "white")
    } else {
      plot.new()
      if (length(nilai) == 0) {
        text(0.5, 0.5, "Tidak ada data yang tersedia untuk menampilkan histogram.", cex = 1.2)
      } else {
        text(0.5, 0.6, paste("Semua nilai data adalah:", unique(nilai), "%"), cex = 1.2)
        text(0.5, 0.4, "Histogram tidak dapat digambar karena tidak ada variasi data.", cex = 1)
      }
    }
  })
  
  output$inter_plot <- renderUI({
    data_info <- get_cleaned_data_info()
    nilai <- data_info$nilai_bersih
    label_indikator <- data_info$label_indikator
    
    # Cek jika tidak ada data atau hanya satu nilai unik
    if (length(nilai) == 0) {
      return(HTML("<p><i>Tidak ada data yang cukup untuk menganalisis distribusi histogram.</i></p>"))
    }
    if (length(unique(nilai)) <= 1) {
      return(HTML(paste0("<p><i>Histogram tidak dapat diinterpretasikan karena semua nilai ", label_indikator, " adalah sama (", unique(nilai), "%).</i></p>")))
    }
    
    # Hitung statistik deskriptif
    min_val <- min(nilai, na.rm = TRUE)
    max_val <- max(nilai, na.rm = TRUE)
    mean_val <- mean(nilai, na.rm = TRUE)
    median_val <- median(nilai, na.rm = TRUE)
    sd_val <- sd(nilai, na.rm = TRUE)
    
    # Bentuk distribusi (sangat sederhana, bisa lebih kompleks dengan skewness/kurtosis package)
    # Jika mean > median, cenderung skewed ke kanan (positive skew)
    # Jika mean < median, cenderung skewed ke kiri (negative skew)
    # Jika mean ~ median, cenderung simetris
    dist_shape <- ""
    if (mean_val > median_val + sd_val/4) { # Tambahkan sedikit toleransi
      dist_shape <- "cenderung menceng ke kanan (positif), menunjukkan bahwa mayoritas kabupaten/kota memiliki persentase yang lebih rendah, dengan beberapa kabupaten/kota memiliki persentase yang sangat tinggi."
    } else if (mean_val < median_val - sd_val/4) { # Tambahkan sedikit toleransi
      dist_shape <- "cenderung menceng ke kiri (negatif), menunjukkan bahwa mayoritas kabupaten/kota memiliki persentase yang lebih tinggi, dengan beberapa kabupaten/kota memiliki persentase yang sangat rendah."
    } else {
      dist_shape <- "relatif simetris, menunjukkan distribusi nilai yang merata di sekitar rata-rata."
    }
    
    # Modalitas (sangat sederhana, hanya perkiraan visual)
    # Untuk analisis yang lebih akurat, perlu deteksi puncak atau density plot
    modal_info <- ""
    if (length(nilai) > 50) { # Hanya relevan jika ada cukup data
      # Ini adalah estimasi sangat kasar tanpa menghitung mode sebenarnya dari histogram bins
      # Untuk akurasi, Anda bisa menghitung density:
      # d <- density(nilai)
      # peak_count <- sum(diff(sign(diff(d$y))) == -2) # Deteksi puncak
      # if (peak_count == 1) modal_info <- "memiliki satu puncak (unimodal)"
      # else if (peak_count > 1) modal_info <- "memiliki beberapa puncak (multimodal)"
      # else modal_info <- "tidak menunjukkan puncak yang jelas"
      
      # Untuk tujuan ini, kita akan asumsikan unimodal kecuali ada indikasi kuat lain.
      modal_info <- "umumnya memiliki satu puncak utama"
    } else {
      modal_info <- "dengan jumlah data yang terbatas, pola puncak mungkin kurang jelas"
    }
    
    
    narasi <- paste0("<div style=\"text-align: left;\">",
                     "<h3 style=\"text-align: center; font-weight: 700\">Interpretasi Histogram ", label_indikator, "</h3>",
                     "<p>Histogram di atas memvisualisasikan distribusi persentase ", tolower(label_indikator), " di berbagai kabupaten/kota. Berikut adalah analisis singkat dari distribusi data:</p>",
                     "<ul>",
                     "<li>Nilai terendah yang tercatat adalah <b>", round(min_val, 2), "%</b> dan nilai tertinggi adalah <b>", round(max_val, 2), "%</b>.</li>",
                     "<li>Rata-rata persentase ", tolower(label_indikator), " adalah sekitar <b>", round(mean_val, 2), "%</b>, sedangkan nilai tengah (median) adalah <b>", round(median_val, 2), "%</b>.</li>",
                     "<li>Distribusi data ini ", dist_shape, "</li>",
                     "<li>Secara visual, distribusi ini ", modal_info, ", menunjukkan satu konsentrasi utama dari kabupaten/kota pada rentang nilai tertentu.</li>",
                     "</ul>",
                     "<p>Pola distribusi ini memberikan gambaran tentang bagaimana ", tolower(label_indikator), " tersebar di seluruh wilayah, mengidentifikasi apakah sebagian besar kabupaten/kota memiliki nilai rendah, tinggi, atau merata.</p>",
                     "</div>"
    )
    
    return(HTML(narasi))
  })
  
  output$boxplot_eksplorasi <- renderPlot({
    data_info <- get_cleaned_data_info()
    nilai <- data_info$nilai_bersih
    label_indikator <- data_info$label_indikator
    
    if (length(unique(nilai)) > 1) {
      boxplot(nilai,
              main = paste("Boxplot Distribusi", label_indikator),
              ylab = switch(label_indikator,
                            "Kemiskinan" = "Persentase Kemiskinan (%)",
                            "Akses Air Bersih" = "Persentase Akses Air Bersih (%)",
                            "Tanpa Saluran Limbah" = "Persentase Rumah Tangga Tanpa Saluran Limbah (%)",
                            "Tanpa Listrik" = "Persentase Rumah Tangga Tanpa Listrik (%)",
                            "Menyewa Rumah" = "Persentase Rumah Tangga Menyewa Rumah (%)"),
              col = "#0073C2FF", border = "black")
    } else {
      plot.new()
      if (length(nilai) == 0) {
        text(0.5, 0.5, "Tidak ada data yang tersedia untuk menampilkan boxplot.", cex = 1.2)
      } else {
        text(0.5, 0.6, paste("Semua nilai data adalah:", unique(nilai), "%"), cex = 1.2)
        text(0.5, 0.4, "Boxplot tidak dapat digambar karena tidak ada variasi data.", cex = 1)
      }
    }
  })
  
  # === INTERPRETASI BOXPLOT ===
  output$inter_boxplot <- renderUI({
    data_info <- get_cleaned_data_info()
    nilai <- data_info$nilai_bersih
    label_indikator <- data_info$label_indikator
    
    if (length(nilai) == 0 || length(unique(nilai)) <= 1) {
      return(HTML("<p><i>Tidak ada data yang cukup untuk menganalisis distribusi boxplot.</i></p>"))
    }
    
    q1 <- quantile(nilai, 0.25, na.rm = TRUE)
    median_val <- median(nilai, na.rm = TRUE)
    q3 <- quantile(nilai, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_whisker <- max(min(nilai, na.rm = TRUE), q1 - 1.5 * iqr)
    upper_whisker <- min(max(nilai, na.rm = TRUE), q3 + 1.5 * iqr)
    outliers <- nilai[nilai < lower_whisker | nilai > upper_whisker]
    num_outliers <- length(outliers)
    
    outlier_text <- if (num_outliers > 0) {
      paste0("Terdapat ", num_outliers, " outlier (data pencilan) yang berada di luar batas normal distribusi.")
    } else {
      "Tidak terdeteksi adanya outlier (data pencilan) yang signifikan."
    }
    
    narasi_boxplot <- paste0(
      "<h3 style = \"font-weight: 700;\">Interpretasi Boxplot ", label_indikator, "</h3>",
      "<p>Boxplot di atas menampilkan ringkasan distribusi data ", tolower(label_indikator), " dengan fokus pada kuartil dan potensi outlier:</p>",
      "<ul>",
      "<li>Garis tengah kotak (median) berada pada <b>", round(median_val, 2), "%</b>, yang membagi data menjadi dua bagian sama banyak.</li>",
      "<li>Kotak tersebut mewakili rentang interkuartil (IQR), yang mencakup 50% data di tengah. Batas bawah kotak (Q1) adalah <b>", round(q1, 2), "%</b> dan batas atas kotak (Q3) adalah <b>", round(q3, 2), "%</b>.</li>",
      "<li>Jangkauan (whisker) menunjukkan sebaran data di luar IQR, tidak termasuk outlier. Nilai minimum data (tidak termasuk outlier) adalah sekitar <b>", round(lower_whisker, 2), "%</b> dan nilai maksimumnya adalah <b>", round(upper_whisker, 2), "%</b>.</li>",
      "<li>", outlier_text, "</li>",
      "</ul>",
      "<p>Visualisasi ini membantu mengidentifikasi sebaran data, nilai tengah, dan keberadaan data ekstrem.</p>"
    )
    return(HTML(narasi_boxplot))
  })
  
  reactive_map_info <- reactiveVal(NULL)
  
  output$distribusi_peta <- renderLeaflet({
    # Dapatkan indikator yang dipilih dari UI
    indikator_terpilih_dari_ui <- input$indikator
    
    nama_kolom_peta_mapping <- list(
      "Penduduk Miskin" = "POVERTY",
      "Rumah Tangga yang Mendapat Akses Air Bersih" = "TAPWATER",
      "Rumah Tangga yang Tidak Memiliki Saluran Pembuangan Air Limbah" = "NOSEWER",
      "Rumah Tangga yang Tidak Menggunakan Listrik sebagai Sumber Penerangan" = "NOELECTRIC",
      "Rumah Tangga yang Menyewa Rumah" = "RENTED"
    )
    
    # Dapatkan nama kolom yang sebenarnya untuk data peta
    indikator_kolom_sebenarnya <- nama_kolom_peta_mapping[[indikator_terpilih_dari_ui]]
    
    # Asumsi 'peta' adalah objek sf atau SpatialPolygonsDataFrame Anda yang sudah dimuat
    peta_data <- peta 
    
    # Pastikan indikator_kolom_sebenarnya tidak NULL dan ada di data peta_data
    req(indikator_kolom_sebenarnya)
    if (!indikator_kolom_sebenarnya %in% names(peta_data)) {
      # Simpan info error untuk interpretasi
      reactive_map_info(list(status = "error", message = "Kolom indikator tidak ditemukan dalam data peta. Pastikan GeoJSON memiliki kolom tersebut."))
      return(leaflet() %>% addTiles() %>% setView(lng = 118, lat = -2, zoom = 4) %>%
               addPopups(118, -2, "Kolom indikator tidak ditemukan dalam data peta. Pastikan GeoJSON memiliki kolom tersebut."))
    }
    
    # Ambil data untuk pewarnaan menggunakan nama kolom yang sudah diterjemahkan
    data_untuk_plot_peta <- peta_data[[indikator_kolom_sebenarnya]]
    
    # Hapus NA untuk perhitungan palet warna
    data_untuk_plot_clean_peta <- data_untuk_plot_peta[!is.na(data_untuk_plot_peta)]
    
    if (length(data_untuk_plot_clean_peta) == 0 || all(is.na(data_untuk_plot_peta))) {
      # Simpan info error untuk interpretasi
      reactive_map_info(list(status = "error", message = "Tidak ada data yang valid atau semua data NA untuk indikator ini."))
      return(leaflet() %>% addTiles() %>% setView(lng = 118, lat = -2, zoom = 4) %>%
               addPopups(118, -2, "Tidak ada data yang valid atau semua data NA untuk indikator ini."))
    }
    
    # Buat palet warna berdasarkan data
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = range(data_untuk_plot_clean_peta, na.rm = TRUE)
    )
    
    # Dapatkan label untuk legenda dan pop-up
    # Mengambil label dari UI berdasarkan nama kolom sebenarnya
    label_indikator_display <- names(nama_kolom_peta_mapping[nama_kolom_peta_mapping == indikator_kolom_sebenarnya])
    
    labels <- sprintf(
      "%s<br/>%s: %g%%",
      peta_data$nmkab,
      label_indikator_display,
      round(data_untuk_plot_peta, 2)
    ) %>% lapply(htmltools::HTML)
    
    # Buat peta Leaflet
    peta_objek <- leaflet(peta_data) %>%
      addTiles() %>%
      setView(lng = 118, lat = -2, zoom = 4) %>%
      addPolygons(
        fillColor = ~pal(data_untuk_plot_peta),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addLegend(pal = pal, values = ~data_untuk_plot_peta, opacity = 0.7,
                title = label_indikator_display, # Menggunakan label yang sudah diterjemahkan
                position = "bottomright")
    
    # Simpan informasi peta ke reactive_map_info
    reactive_map_info(list(
      status = "success",
      indikator_nama = indikator_terpilih_dari_ui, # Nama indikator dari UI
      indikator_kolom = indikator_kolom_sebenarnya, # Nama kolom di data
      data_rentang = range(data_untuk_plot_clean_peta, na.rm = TRUE),
      median_data = median(data_untuk_plot_clean_peta, na.rm = TRUE),
      mean_data = mean(data_untuk_plot_clean_peta, na.rm = TRUE),
      sd_data = sd(data_untuk_plot_clean_peta, na.rm = TRUE)
    ))
    
    return(peta_objek)
  })
  
  output$downloadMapPNG <- downloadHandler(
    filename = function() {
      # Nama file akan menyertakan indikator yang sedang ditampilkan
      indikator_terpilih_dari_ui <- input$indikator
      paste0("peta_", gsub(" ", "_", indikator_terpilih_dari_ui), "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Kita perlu membuat ulang logika peta di sini agar webshot2 bisa merendernya
      # Ambil kembali semua input dan data yang diperlukan untuk membuat peta
      
      indikator_terpilih_dari_ui <- input$indikator # Dapatkan lagi indikator dari UI
      
      nama_kolom_peta_mapping <- list(
        "Penduduk Miskin" = "POVERTY",
        "Rumah Tangga yang Mendapat Akses Air Bersih" = "TAPWATER",
        "Rumah Tangga yang Tidak Memiliki Saluran Pembuangan Air Limbah" = "NOSEWER",
        "Rumah Tangga yang Tidak Menggunakan Listrik sebagai Sumber Penerangan" = "NOELECTRIC",
        "Rumah Tangga yang Menyewa Rumah" = "RENTED"
      )
      
      indikator_kolom_sebenarnya <- nama_kolom_peta_mapping[[indikator_terpilih_dari_ui]]
      
      peta_data <- peta # Objek sf Anda
      req(indikator_kolom_sebenarnya)
      
      if (!indikator_kolom_sebenarnya %in% names(peta_data)) {
        stop("Kolom indikator tidak ditemukan dalam data peta saat mencoba mengunduh.")
      }
      
      data_untuk_plot_peta <- peta_data[[indikator_kolom_sebenarnya]]
      data_untuk_plot_clean_peta <- data_untuk_plot_peta[!is.na(data_untuk_plot_peta)]
      
      if (length(data_untuk_plot_clean_peta) == 0 || all(is.na(data_untuk_plot_peta))) {
        stop("Tidak ada data yang valid untuk indikator ini saat mencoba mengunduh peta.")
      }
      
      pal <- colorNumeric(
        palette = "YlOrRd",
        domain = range(data_untuk_plot_clean_peta, na.rm = TRUE)
      )
      
      label_indikator_display <- names(nama_kolom_peta_mapping[nama_kolom_peta_mapping == indikator_kolom_sebenarnya])
      
      labels <- sprintf(
        "%s<br/>%s: %g%%",
        peta_data$nmkab,
        label_indikator_display,
        round(data_untuk_plot_peta, 2)
      ) %>% lapply(htmltools::HTML)
      
      # Re-create the leaflet object for screenshotting
      map_to_screenshot <- leaflet(peta_data) %>%
        addTiles() %>%
        setView(lng = 118, lat = -2, zoom = 4) %>% # Pastikan setView konsisten
        addPolygons(
          fillColor = ~pal(data_untuk_plot_peta),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.9,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
        ) %>%
        addLegend(pal = pal, values = ~data_untuk_plot_peta, opacity = 0.7,
                  title = label_indikator_display,
                  position = "bottomright")
      
      # Simpan objek leaflet ke file HTML sementara
      temp_html_file <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(map_to_screenshot, file = temp_html_file, selfcontained = TRUE)
      
      # Gunakan webshot2 untuk mengambil screenshot dari file HTML
      webshot2::webshot(
        url = temp_html_file,
        file = file,
        delay = 0.5, # Beri sedikit waktu untuk peta dirender di headless browser
        vwidth = 1000, # Lebar viewport virtual (sesuaikan untuk kualitas)
        vheight = 800  # Tinggi viewport virtual (sesuaikan untuk kualitas)
      )
    }
  )
  
  output$interpretasi_distribusi_peta <- renderUI({
    map_info <- reactive_map_info()
    req(map_info) # Pastikan informasi peta tersedia
    
    if (map_info$status == "error") {
      return(HTML(paste0("<p style='color: red;'><strong>Gagal membuat interpretasi:</strong> ", map_info$message, "</p>")))
    }
    
    indikator_nama <- map_info$indikator_nama
    data_rentang <- map_info$data_rentang
    median_data <- map_info$median_data
    mean_data <- map_info$mean_data
    sd_data <- map_info$sd_data
    
    # Logika interpretasi
    narasi <- paste0(
      "<h3 style = \"font-weight: 700;\">Interpretasi Peta Distribusi ", indikator_nama, "</h3>",
      "<p>Peta di atas menampilkan distribusi spasial dari indikator <b>'", indikator_nama, "'</b> di berbagai wilayah. Pewarnaan pada peta menggunakan palet warna kuning-oranye-merah ('YlOrRd'), di mana warna yang lebih gelap (lebih merah) mengindikasikan nilai <b>'", indikator_nama, "'</b> yang lebih tinggi, sedangkan warna yang lebih terang (lebih kuning) mengindikasikan nilai yang lebih rendah.</p>",
      "<ul>",
      "<li><b>Rentang Nilai:</b> Nilai indikator '", indikator_nama, "' bervariasi dari sekitar <b>", round(data_rentang[1], 2), "%</b> (terendah) hingga <b>", round(data_rentang[2], 2), "%</b> (tertinggi).</li>",
      "<li><b>Rata-rata:</b> Rata-rata nilai '", indikator_nama, "' di seluruh wilayah adalah sekitar <b>", round(mean_data, 2), "%</b>.</li>",
      "<li><b>Median:</b> Nilai median '", indikator_nama, "' adalah sekitar <b>", round(median_data, 2), "%</b>. Ini berarti 50% wilayah memiliki nilai di bawah median ini dan 50% di atasnya.</li>"
    )
    
    # Tambahkan observasi pola spasial umum
    if (data_rentang[2] - data_rentang[1] > 0) { # Pastikan ada variasi data
      if (sd_data / mean_data > 0.3) { # Contoh heuristik untuk variasi tinggi (koefisien variasi > 30%)
        narasi <- paste0(narasi,
                         "<li><b>Pola Spasial:</b> Dari visualisasi, terlihat adanya <b>heterogenitas</b> dalam distribusi '", indikator_nama, "'. Beberapa wilayah menunjukkan konsentrasi nilai yang tinggi (area lebih merah), sementara wilayah lain memiliki nilai yang relatif rendah (area lebih kuning). Ini mengindikasikan adanya perbedaan yang jelas dalam '", indikator_nama, "' antar wilayah.</li>"
        )
      } else {
        narasi <- paste0(narasi,
                         "<li><b>Pola Spasial:</b> Distribusi '", indikator_nama, "' cenderung <b>lebih homogen</b> di seluruh wilayah, meskipun mungkin ada sedikit variasi. Ini menunjukkan bahwa nilai '", indikator_nama, "' relatif serupa di sebagian besar wilayah yang diamati.</li>"
        )
      }
    } else {
      narasi <- paste0(narasi, "<li><b>Pola Spasial:</b> Semua wilayah memiliki nilai yang sama untuk indikator ini.</li>")
    }
    
    narasi <- paste0(narasi,
                     "</ul>",
                     "<p>Pola spasial ini dapat memberikan wawasan awal tentang area mana yang mungkin memerlukan perhatian lebih lanjut atau intervensi spesifik terkait indikator ini.</p>"
    )
    
    HTML(narasi)
  })
  
  # Create reactive data frame - Langsung membaca dari file yang sudah ada
  df_react <- reactive({
    # Baca file CSV dari direktori www
    df <- read.csv("www/data.csv")
    
    # Ensure all required columns exist
    required_cols <- c("POVERTY", "RENTED", "NOSEWER", "NOELECTRIC", "TAPWATER")
    missing_cols <- required_cols[!required_cols %in% names(df)]
    
    if (length(missing_cols) > 0) {
      stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }
    
    return(df)
  })
  
  # Display data summary
  output$data_summary <- renderPrint({
    req(df_react())
    cat("Jumlah Observasi:", nrow(df_react()), "\n")
    cat("Variabel dalam Dataset:", paste(names(df_react()), collapse = ", "), "\n\n")
    cat("Ringkasan Statistik:\n")
    summary(df_react()[, c("POVERTY", "RENTED", "NOSEWER", "NOELECTRIC", "TAPWATER")])
  })
  
  # --- Uji Beda Rata-Rata (Mean Difference Test) ---
  reactive_mean_diff_result <- reactiveVal(NULL)
  
  observeEvent(input$run_mean_diff, {
    req(df_react()) # Pastikan df_react sudah tersedia
    selected_var <- input$mean_diff_var # Variabel yang digunakan untuk mengelompokkan
    
    df_temp <- df_react()
    
    # Create a dichotomous group based on the median of the selected variable
    median_val <- median(df_temp[[selected_var]], na.rm = TRUE)
    # Ubah nama grup ke Bahasa Indonesia agar konsisten dengan interpretasi
    group1_name_internal <- paste0("Rendah ", selected_var)
    group2_name_internal <- paste0("Tinggi ", selected_var)
    df_temp$Group <- ifelse(df_temp[[selected_var]] <= median_val, group1_name_internal, group2_name_internal)
    
    # Perform t-test (POVERTY adalah variabel dependen)
    ttest_result <- t.test(POVERTY ~ Group, data = df_temp)
    
    # Dapatkan deskripsi kelompok
    summary_groups_poverty <- df_temp %>%
      group_by(Group) %>%
      summarise(
        N = n(),
        Mean_Poverty = mean(POVERTY, na.rm = TRUE),
        SD_Poverty = sd(POVERTY, na.rm = TRUE),
        Mean_Var = mean(get(selected_var), na.rm = TRUE) # Rata-rata variabel pengelompok di setiap grup
      ) %>%
      ungroup()
    
    # Simpan hasil uji dan ringkasan ke reactiveVal
    reactive_mean_diff_result(list(
      test_object = ttest_result,
      summary_groups = summary_groups_poverty,
      selected_var = selected_var, # Variabel yang digunakan untuk pengelompokan
      median_val = median_val
    ))
    
    # === SIMPAN KE GLOBAL REPORT DATA ===
    global_report_data$mean_diff_result <- result_to_save
    # Tambahkan juga input UI yang relevan
    global_report_data$current_inputs$mean_diff_var <- input$mean_diff_var
    
    output$mean_diff_output <- renderPrint({
      print(ttest_result)
      cat("\nDeskripsi Kelompok:\n")
      print(summary_groups_poverty)
    })
    
    output$mean_diff_plot <- renderPlot({
      ggplot(df_temp, aes_string(x = "Group", y = "POVERTY", fill = "Group")) +
        geom_boxplot() +
        labs(title = paste("Distribusi Tingkat Kemiskinan (POVERTY) Berdasarkan Kategori ", selected_var), # Ubah judul
             x = paste0("Kategori ", selected_var), y = "Tingkat Kemiskinan (POVERTY)") + # Ubah label sumbu
        theme_minimal() +
        theme(legend.position = "none") # Sembunyikan legenda
    })
  })
  
  output$inter_uji_rata_rata <- renderUI({
    result_list <- reactive_mean_diff_result()
    req(result_list) # Pastikan hasil uji tersedia
    
    ttest_result <- result_list$test_object
    summary_groups <- result_list$summary_groups
    selected_var <- result_list$selected_var
    median_val <- result_list$median_val
    
    p_value_t <- ttest_result$p.value
    test_statistic_t <- ttest_result$statistic
    df_t <- ttest_result$parameter
    alpha <- 0.05 # Tingkat signifikansi
    
    # Ambil nama kelompok dan rata-rata POVERTY mereka
    group1_name_display <- paste0("Kelompok Rendah ", selected_var, " (nilai ≤ ", round(median_val, 2), ")")
    group2_name_display <- paste0("Kelompok Tinggi ", selected_var, " (nilai > ", round(median_val, 2), ")")
    
    mean_poverty_group1 <- summary_groups$Mean_Poverty[summary_groups$Group == paste0("Rendah ", selected_var)]
    mean_poverty_group2 <- summary_groups$Mean_Poverty[summary_groups$Group == paste0("Tinggi ", selected_var)]
    
    kesimpulan_t <- ""
    makna_t <- ""
    warna_t <- ""
    
    if (p_value_t < alpha) {
      kesimpulan_t <- "Tolak Hipotesis Nol (H₀)"
      makna_t <- paste0("Ini berarti ada <b>perbedaan rata-rata tingkat kemiskinan (POVERTY) yang signifikan secara statistik</b> antara ", group1_name_display, " (rata-rata kemiskinan: ", round(mean_poverty_group1, 2), "%) dan ", group2_name_display, " (rata-rata kemiskinan: ", round(mean_poverty_group2, 2), "%).")
      warna_t <- "red"
    } else {
      kesimpulan_t <- "Gagal Tolak Hipotesis Nol (H₀)"
      makna_t <- paste0("Ini berarti <b>tidak ada perbedaan rata-rata tingkat kemiskinan (POVERTY) yang signifikan secara statistik</b> antara ", group1_name_display, " (rata-rata kemiskinan: ", round(mean_poverty_group1, 2), "%) dan ", group2_name_display, " (rata-rata kemiskinan: ", round(mean_poverty_group2, 2), "%).")
      warna_t <- "green"
    }
    
    narasi <- paste0(
      "<h3>Interpretasi Uji Beda Rata-rata (Uji-t)</h3>",
      "<p>Uji-t digunakan untuk mengevaluasi apakah ada perbedaan signifikan dalam rata-rata tingkat kemiskinan (POVERTY) antara dua kelompok yang dibentuk berdasarkan median variabel '", selected_var, "'.</p>",
      "<ul>",
      "<li><b>Variabel Dependen:</b> Tingkat Kemiskinan (POVERTY)</li>",
      "<li><b>Variabel Independen (Kategorisasi):</b> '", selected_var, "' dibagi menjadi kelompok Rendah (≤ ", round(median_val, 2), ") dan Tinggi (> ", round(median_val, 2), ").</li>",
      "<li><b>Hipotesis Nol (H₀):</b> Tidak ada perbedaan rata-rata tingkat kemiskinan yang signifikan antara kelompok rendah dan tinggi '", selected_var, "'.</li>",
      "<li><b>Hipotesis Alternatif (H₁):</b> Ada perbedaan rata-rata tingkat kemiskinan yang signifikan antara kelompok rendah dan tinggi '", selected_var, "'.</li>",
      "<li><b>Nilai Statistik Uji (t):</b> ", round(test_statistic_t, 3), "</li>",
      "<li><b>Derajat Kebebasan (df):</b> ", df_t, "</li>",
      "<li><b>p-value:</b> ", round(p_value_t, 4), "</li>",
      "</ul>",
      "<p style='color:", warna_t, ";'><b>Keputusan:</b> ", kesimpulan_t, "</p>",
      "<p>", makna_t, "</p>",
      "<p>Hasil ini membantu memahami apakah tingkat kemiskinan berbeda secara signifikan antara daerah dengan nilai '", selected_var, "' yang rendah dan tinggi.</p>"
    )
    HTML(narasi)
  })
  
  # --- Uji Proporsi ---
  reactive_chi_sq_result <- reactiveVal(NULL)
  
  observeEvent(input$run_prop_test, {
    req(df_react()) # Pastikan df_react sudah tersedia
    selected_var <- input$prop_var
    threshold <- input$prop_threshold
    
    df_temp <- df_react()
    
    # Categorize POVERTY into high/low
    median_poverty <- median(df_temp$POVERTY, na.rm = TRUE)
    df_temp$PovertyGroup <- ifelse(df_temp$POVERTY <= median_poverty, "Rendah", "Tinggi")
    
    # Create binary variable for the selected independent variable based on threshold
    df_temp$VarAboveThreshold <- ifelse(df_temp[[selected_var]] > threshold, "Di Atas Ambang Batas", "Di Bawah Ambang Batas") # Ubah ke Bahasa Indonesia
    
    # Create a contingency table
    contingency_table <- table(df_temp$PovertyGroup, df_temp$VarAboveThreshold)
    
    # Perform chi-squared test for independence
    chi_sq_test <- chisq.test(contingency_table)
    
    # Simpan hasil uji ke reactiveVal
    result_to_save <- list(
      test_object = chi_sq_test,
      contingency_table = contingency_table,
      selected_var = selected_var,
      threshold = threshold
    )
    reactive_chi_sq_result(result_to_save)
    
    # === SIMPAN KE GLOBAL REPORT DATA ===
    global_report_data$chi_sq_result <- result_to_save
    global_report_data$current_inputs$prop_var <- input$prop_var
    global_report_data$current_inputs$prop_threshold <- input$prop_threshold
    
    output$prop_test_output <- renderPrint({
      cat("Tabel Kontingensi (Kelompok Kemiskinan vs. Variabel di Atas/Bawah Ambang Batas):\n")
      print(contingency_table)
      cat("\n")
      cat("Uji Chi-squared untuk Independensi:\n")
      print(chi_sq_test)
      cat("\nInterpretasi: Uji ini melihat apakah ada hubungan signifikan antara kelompok kemiskinan (Rendah/Tinggi) dengan kategori variabel '", selected_var, "' (Di Atas/Di Bawah Ambang Batas ", threshold, "%).\n", sep="")
    })
  })
  
  output$inter_uji_proporsi <- renderUI({
    result_list <- reactive_chi_sq_result()
    req(result_list) # Pastikan hasil uji tersedia
    
    chi_sq_test <- result_list$test_object
    contingency_table <- result_list$contingency_table
    selected_var <- result_list$selected_var
    threshold <- result_list$threshold
    
    p_value_chi <- chi_sq_test$p.value
    test_statistic_chi <- chi_sq_test$statistic
    df_chi <- chi_sq_test$parameter
    alpha <- 0.05 # Tingkat signifikansi
    
    kesimpulan_chi <- ""
    makna_chi <- ""
    warna_chi <- ""
    
    if (p_value_chi < alpha) {
      kesimpulan_chi <- "Tolak Hipotesis Nol (H₀)"
      makna_chi <- paste0("Ini berarti ada <b>hubungan yang signifikan secara statistik</b> antara kelompok kemiskinan (Rendah/Tinggi) dan kategori variabel '", selected_var, "' (Di Atas/Di Bawah Ambang Batas ", threshold, "%). Dengan kata lain, proporsi distrik yang berada di atas atau di bawah ambang batas ", selected_var, " **berbeda secara signifikan** antara kelompok kemiskinan rendah dan tinggi.")
      warna_chi <- "red"
    } else {
      kesimpulan_chi <- "Gagal Tolak Hipotesis Nol (H₀)"
      makna_chi <- paste0("Ini berarti <b>tidak ada hubungan yang signifikan secara statistik</b> antara kelompok kemiskinan (Rendah/Tinggi) dan kategori variabel '", selected_var, "' (Di Atas/Di Bawah Ambang Batas ", threshold, "%). Dengan kata lain, proporsi distrik yang berada di atas atau di bawah ambang batas ", selected_var, " **tidak berbeda secara signifikan** antara kelompok kemiskinan rendah dan tinggi.")
      warna_chi <- "green"
    }
    
    narasi <- paste0(
      "<h3>Interpretasi Uji Chi-squared untuk Independensi</h3>",
      "<p>Uji Chi-squared digunakan untuk mengevaluasi apakah ada hubungan yang signifikan antara dua variabel kategori.</p>",
      "<ul>",
      "<li><b>Variabel yang Diuji:</b> Kelompok Kemiskinan (Rendah/Tinggi) dan Kategori '", selected_var, "' (Di Atas/Di Bawah Ambang Batas ", threshold, "%)</li>",
      "<li><b>Hipotesis Nol (H₀):</b> Kedua variabel bersifat independen (tidak ada hubungan).</li>",
      "<li><b>Hipotesis Alternatif (H₁):</b> Kedua variabel tidak independen (ada hubungan).</li>",
      "<li><b>Nilai Statistik Uji (Chi-squared):</b> ", round(test_statistic_chi, 3), "</li>",
      "<li><b>Derajat Kebebasan (df):</b> ", df_chi, "</li>",
      "<li><b>p-value:</b> ", round(p_value_chi, 4), "</li>",
      "</ul>",
      "<p style='color:", warna_chi, ";'><b>Keputusan:</b> ", kesimpulan_chi, "</p>",
      "<p>", makna_chi, "</p>",
      "<p>Hasil ini membantu memahami apakah status kemiskinan suatu distrik berhubungan dengan proporsi variabel lain yang berada di atas atau di bawah ambang batas tertentu.</p>"
    )
    HTML(narasi)
  })
  
  # --- Uji Varians (F-test) ---
  reactive_var_test_result <- reactiveVal(NULL)
  
  observeEvent(input$run_var_test, {
    req(df_react()) # Pastikan df_react sudah tersedia
    selected_var_for_test <- input$var_test_var
    grouping_var <- input$var_test_group_var
    
    df_temp <- df_react()
    
    # Create dichotomous group based on the median of the grouping variable
    median_grouping_val <- median(df_temp[[grouping_var]], na.rm = TRUE)
   
    df_temp$Group <- ifelse(df_temp[[grouping_var]] <= median_grouping_val, 
                            paste0("Rendah ", grouping_var), 
                            paste0("Tinggi ", grouping_var))
    
    # Perform F-test for variances
    f_test_result <- var.test(df_temp[[selected_var_for_test]] ~ Group, data = df_temp)
    
    # Dapatkan ringkasan varians kelompok
    summary_varians_groups <- df_temp %>%
      group_by(Group) %>%
      summarise(
        N = n(),
        Mean_Var = mean(get(selected_var_for_test), na.rm = TRUE),
        SD_Var = sd(get(selected_var_for_test), na.rm = TRUE),
        Variance_Var = var(get(selected_var_for_test), na.rm = TRUE)
      ) %>%
      ungroup() # Penting untuk ungroup setelah summarise
    
    # Simpan hasil uji dan ringkasan ke reactiveVal
    result_to_save <- list(
      test_object = f_test_result,
      summary_groups = summary_varians_groups,
      selected_var_for_test = selected_var_for_test,
      grouping_var = grouping_var
    )
    reactive_var_test_result(result_to_save)
    
    # === SIMPAN KE GLOBAL REPORT DATA ===
    global_report_data$var_test_result <- result_to_save
    global_report_data$current_inputs$var_test_var <- input$var_test_var
    global_report_data$current_inputs$var_test_group_var <- input$var_test_group_var
    
    output$var_test_output <- renderPrint({
      print(f_test_result)
      cat("\nDeskripsi Varians Kelompok:\n")
      print(summary_varians_groups)
    })
  })
  
  output$inter_uji_varians <- renderUI({
    result_list <- reactive_var_test_result()
    req(result_list) # Pastikan hasil uji tersedia
    
    f_test_result <- result_list$test_object
    summary_groups <- result_list$summary_groups
    selected_var_for_test <- result_list$selected_var_for_test
    grouping_var <- result_list$grouping_var
    
    p_value_var <- f_test_result$p.value
    test_statistic_var <- f_test_result$statistic
    df1_var <- f_test_result$parameter[1]
    df2_var <- f_test_result$parameter[2]
    alpha <- 0.05 # Tingkat signifikansi
    
    # Ambil nama kelompok dan variansnya
    group1_name <- summary_groups$Group[1]
    group2_name <- summary_groups$Group[2]
    var_group1 <- summary_groups$Variance_Var[1]
    var_group2 <- summary_groups$Variance_Var[2]
    
    kesimpulan_var <- ""
    makna_var <- ""
    warna_var <- ""
    
    if (p_value_var < alpha) {
      kesimpulan_var <- "Tolak Hipotesis Nol (H₀)"
      makna_var <- paste0("Ini berarti ada <b>perbedaan varians yang signifikan secara statistik</b> untuk variabel '", selected_var_for_test, "' antara kelompok '", group1_name, "' (Varians: ", round(var_group1, 2), ") dan kelompok '", group2_name, "' (Varians: ", round(var_group2, 2), ")." )
      warna_var <- "red"
    } else {
      kesimpulan_var <- "Gagal Tolak Hipotesis Nol (H₀)"
      makna_var <- paste0("Ini berarti <b>tidak ada perbedaan varians yang signifikan secara statistik</b> untuk variabel '", selected_var_for_test, "' antara kelompok '", group1_name, "' (Varians: ", round(var_group1, 2), ") dan kelompok '", group2_name, "' (Varians: ", round(var_group2, 2), ")." )
      warna_var <- "green"
    }
    
    narasi <- paste0(
      "<h3>Interpretasi Uji Perbedaan Varians (Uji F)</h3>",
      "<p>Uji F varians digunakan untuk mengevaluasi apakah ada perbedaan signifikan dalam keragaman (varians) variabel '", selected_var_for_test, "' antara dua kelompok yang dibentuk berdasarkan '", grouping_var, "'.</p>",
      "<ul>",
      "<li><b>Hipotesis Nol (H₀):</b> Varians kedua kelompok adalah sama.</li>",
      "<li><b>Hipotesis Alternatif (H₁):</b> Varians kedua kelompok tidak sama.</li>",
      "<li><b>Nilai Statistik Uji (F):</b> ", round(test_statistic_var, 3), "</li>",
      "<li><b>Derajat Kebebasan (df1, df2):</b> (", df1_var, ", ", df2_var, ")</li>",
      "<li><b>p-value:</b> ", round(p_value_var, 4), "</li>",
      "</ul>",
      "<p style='color:", warna_var, ";'><b>Keputusan:</b> ", kesimpulan_var, "</p>",
      "<p>", makna_var, "</p>",
      "<p>Memahami apakah varians antar kelompok berbeda penting untuk validitas uji statistik lainnya (misalnya, asumsi homoskedastisitas pada uji-t dan ANOVA).</p>"
    )
    HTML(narasi)
  })
  
  # --- ANOVA ---
  reactive_anova_result <- reactiveVal(NULL)
  
  observeEvent(input$run_anova, {
    req(df_react()) # Pastikan df_react sudah tersedia
    selected_var <- input$anova_var # Variabel independen yang dikategorikan
    num_groups <- input$anova_groups # Jumlah kelompok yang diinginkan
    
    df_temp <- df_react()
    
    # Create categorized variable based on quantiles
    # Pastikan quantile menghasilkan batas unik untuk menghindari error cut()
    breaks_anova <- unique(quantile(df_temp[[selected_var]], probs = seq(0, 1, by = 1/num_groups), na.rm = TRUE))
    
    # Pastikan ada cukup batas untuk jumlah kelompok yang diminta
    if (length(breaks_anova) < (num_groups + 1)) {
      output$anova_output <- renderPrint({
        cat("Peringatan: Tidak dapat membuat", num_groups, "kelompok unik dari variabel yang dipilih. Coba kurangi jumlah kelompok atau pilih variabel lain.\n")
        cat("Jumlah batas unik yang ditemukan: ", length(breaks_anova), "\n")
      })
      output$anova_plot <- renderPlot({})
      reactive_anova_result(NULL) # Reset hasil jika tidak valid
      return()
    }
    
    df_temp$CategorizedVar <- cut(df_temp[[selected_var]],
                                  breaks = breaks_anova,
                                  include.lowest = TRUE,
                                  labels = paste0("Kelompok ", 1:num_groups)) # Ubah ke Bahasa Indonesia
    
    # Pastikan ada cukup kelompok yang dibuat
    if (nlevels(df_temp$CategorizedVar) < num_groups) {
      output$anova_output <- renderPrint({
        cat("Peringatan: Tidak dapat membuat", num_groups, "kelompok unik dari variabel yang dipilih. Coba kurangi jumlah kelompok atau pilih variabel lain.\n")
        cat("Jumlah kelompok yang dibuat: ", nlevels(df_temp$CategorizedVar), "\n")
      })
      output$anova_plot <- renderPlot({})
      reactive_anova_result(NULL) # Reset hasil jika tidak valid
      return()
    }
    
    # Perform ANOVA
    # Variabel dependen adalah POVERTY, variabel independen adalah CategorizedVar
    anova_result <- aov(POVERTY ~ CategorizedVar, data = df_temp)
    summary_anova <- summary(anova_result)
    
    # Simpan hasil uji ke reactiveVal
    result_to_save <- list(
      test_object = anova_result,
      summary_anova = summary_anova,
      selected_var = selected_var, # Variabel yang dikategorikan
      num_groups = num_groups,
      group_names = levels(df_temp$CategorizedVar) # Nama-nama kelompok yang benar-benar dibuat
    )
    reactive_anova_result(result_to_save)
    
    # === SIMPAN KE GLOBAL REPORT DATA ===
    global_report_data$anova_result <- result_to_save
    global_report_data$current_inputs$anova_var <- input$anova_var
    global_report_data$current_inputs$anova_groups <- input$anova_groups
    
    output$anova_output <- renderPrint({
      print(summary_anova)
    })
    
    output$anova_plot <- renderPlot({
      ggplot(df_temp, aes_string(x = "CategorizedVar", y = "POVERTY", fill = "CategorizedVar")) +
        geom_boxplot() +
        labs(title = paste("Distribusi Kemiskinan Berdasarkan Kategori", selected_var), # Ubah judul
             x = paste0("Kategori ", selected_var), y = "Tingkat Kemiskinan (POVERTY)") + # Ubah label sumbu
        theme_minimal() +
        theme(legend.position = "none") # Sembunyikan legenda jika tidak diperlukan
    })
  })
  
  output$inter_uji_anova <- renderUI({
    result_list <- reactive_anova_result()
    req(result_list) # Pastikan hasil uji tersedia
    
    anova_result <- result_list$test_object
    summary_anova <- result_list$summary_anova
    selected_var <- result_list$selected_var
    num_groups <- result_list$num_groups
    group_names <- result_list$group_names
    
    # Ambil p-value, statistik F, dan df dari summary anova
    p_value_anova <- summary_anova[[1]]$`Pr(>F)`[1]
    test_statistic_anova <- summary_anova[[1]]$`F value`[1]
    df1_anova <- summary_anova[[1]]$Df[1] # Derajat kebebasan antar kelompok
    df2_anova <- summary_anova[[1]]$Df[2] # Derajat kebebasan dalam kelompok
    alpha <- 0.05 # Tingkat signifikansi
    
    kesimpulan_anova <- ""
    makna_anova <- ""
    warna_anova <- ""
    
    if (p_value_anova < alpha) {
      kesimpulan_anova <- "Tolak Hipotesis Nol (H₀)"
      makna_anova <- paste0("Ini berarti setidaknya ada <b>satu pasang kelompok</b> di antara ", paste(group_names, collapse = ", "), " yang memiliki <b>perbedaan rata-rata tingkat kemiskinan (POVERTY) yang signifikan secara statistik</b>. Diperlukan uji post-hoc (misalnya uji Tukey HSD) untuk menentukan pasangan kelompok mana yang berbeda secara spesifik.")
      warna_anova <- "red"
    } else {
      kesimpulan_anova <- "Gagal Tolak Hipotesis Nol (H₀)"
      makna_anova <- paste0("Ini berarti <b>tidak ada perbedaan rata-rata tingkat kemiskinan (POVERTY) yang signifikan secara statistik</b> di antara semua kelompok (", paste(group_names, collapse = ", "), ").")
      warna_anova <- "green"
    }
    
    narasi <- paste0(
      "<h3>Interpretasi Uji ANOVA</h3>",
      "<p>Analisis Varians (ANOVA) digunakan untuk mengevaluasi apakah ada perbedaan signifikan dalam rata-rata tingkat kemiskinan (POVERTY) antara ", num_groups, " kelompok yang dibentuk berdasarkan kategori variabel '", selected_var, "'.</p>",
      "<ul>",
      "<li><b>Hipotesis Nol (H₀):</b> Rata-rata tingkat kemiskinan dari semua kelompok (", paste(group_names, collapse = ", "), ") adalah sama.</li>",
      "<li><b>Hipotesis Alternatif (H₁):</b> Setidaknya ada satu pasang kelompok yang memiliki rata-rata tingkat kemiskinan yang berbeda.</li>",
      "<li><b>Nilai Statistik Uji (F):</b> ", round(test_statistic_anova, 3), "</li>",
      "<li><b>Derajat Kebebasan (df1, df2):</b> (", df1_anova, ", ", df2_anova, ")</li>",
      "<li><b>p-value:</b> ", round(p_value_anova, 4), "</li>",
      "</ul>",
      "<p style='color:", warna_anova, ";'><b>Keputusan:</b> ", kesimpulan_anova, "</p>",
      "<p>", makna_anova, "</p>",
      "<p>ANOVA adalah alat yang ampuh untuk membandingkan beberapa kelompok sekaligus, menghindari masalah uji-t berulang dan inflasi Tipe I Error.</p>"
    )
    HTML(narasi)
  })
  
  observeEvent(input$run_regression, {
    req(data())
    df_model <- data()
    required_cols <- c("POVERTY", "RENTED", "NOSEWER", "TAPWATER", "NOELECTRIC")
    missing_cols <- setdiff(required_cols, names(df_model))
    if (length(missing_cols) > 0) {
      showNotification(paste("Kolom yang dibutuhkan tidak ditemukan:", paste(missing_cols, collapse = ", ")), type = "error")
      reactive_model_fit(NULL)
      global_report_data$regression_result <- NULL # Reset di global data
      return()
    }
    
    df_model_clean <- df_model %>%
      select(POVERTY, RENTED, NOSEWER, TAPWATER, NOELECTRIC) %>%
      na.omit()
    
    if (nrow(df_model_clean) == 0) {
      showNotification("Tidak ada data yang cukup setelah penghapusan NA untuk membuat model.", type = "error")
      reactive_model_fit(NULL)
      global_report_data$regression_result <- NULL # Reset di global data
      return()
    }
    
    if (any(df_model_clean$POVERTY <= 0)) {
      df_model_clean$POVERTY_log <- log(df_model_clean$POVERTY + 1)
    } else {
      df_model_clean$POVERTY_log <- log(df_model_clean$POVERTY)
    }
    
    tryCatch({
      model_fit <- lm(POVERTY_log ~ RENTED + NOSEWER + TAPWATER + NOELECTRIC, data = df_model_clean)
      reactive_model_fit(model_fit)
      
      # === SIMPAN KE GLOBAL REPORT DATA ===
      global_report_data$regression_result <- model_fit
      global_report_data$current_inputs$regression_vars <- "log(POVERTY) ~ RENTED + NOSEWER + TAPWATER + NOELECTRIC"
      
    }, error = function(e) {
      showNotification(paste("Terjadi error saat membuat model regresi:", e$message), type = "error")
      reactive_model_fit(NULL)
      global_report_data$regression_result <- NULL # Reset di global data
    })
  })
  
  model_fit <- lm(log(POVERTY) ~ RENTED+NOSEWER+TAPWATER+NOELECTRIC, data = data)
  
  output$summary_model <- renderPrint({
    summary(model_fit)
  })
  
  output$inter_regresi <- renderUI({
    model_summary <- summary(model_fit)
    coefs <- model_summary$coefficients
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    f_stat <- model_summary$fstatistic
    p_val_f <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    
    # 1. Formula
    formula_text <- paste("Model regresi yang digunakan adalah:", deparse(model_fit$call$formula))
    
    # 2. R-Squared
    r2_text <- paste("Nilai R-squared sebesar", round(r_squared, 3),
                     "menunjukkan bahwa sekitar", round(r_squared * 100, 1),
                     "% variasi pada variabel dependen dapat dijelaskan oleh model.")
    
    adj_r2_text <- paste("Nilai Adjusted R-squared sebesar", round(adj_r_squared, 3),
                         "memperhitungkan jumlah variabel prediktor dalam model.")
    
    # 3. F-test
    f_test_text <- ifelse(p_val_f < 0.05,
                          paste("Uji F menghasilkan p-value =", signif(p_val_f, 3),
                                "yang < 0.05, sehingga model secara keseluruhan signifikan."),
                          paste("Uji F menghasilkan p-value =", signif(p_val_f, 3),
                                "yang > 0.05, sehingga model secara keseluruhan tidak signifikan.")
    )
    
    # 4. Koefisien
    coef_texts <- c()
    for (i in 1:nrow(coefs)) {
      var_name <- rownames(coefs)[i]
      estimate <- round(coefs[i, 1], 3)
      p_value <- coefs[i, 4]
      signif_text <- ifelse(p_value < 0.05,
                            "berpengaruh signifikan terhadap variabel dependen.",
                            "tidak berpengaruh signifikan.")
      direction <- ifelse(estimate > 0, "positif", "negatif")
      
      if (var_name == "(Intercept)") {
        coef_texts[i] <- paste0("Intercept model bernilai ", estimate, ".")
      } else {
        coef_texts[i] <- paste0("Variabel ", var_name, " memiliki koefisien ", estimate,
                                " yang berarti hubungan ", direction, " dan ", signif_text)
      }
    }
    
    # Gabungkan semua penjelasan
    HTML(paste(
      formula_text, "<br><br>",
      r2_text, "<br>",
      adj_r2_text, "<br><br>",
      f_test_text, "<br><br>",
      paste(coef_texts, collapse = "<br>"),
      sep = ""
    ))
  })
  
  output$overall_check <- renderPrint({
    model <- model_fit
    
    # Variabel untuk menyimpan hasil uji
    assumption_results <- list()
    
    # 1. UJI HOMOSKEDASTISITAS
    tryCatch({
      library(lmtest)
      bp_test <- bptest(model)
      homoskedastis_pass <- bp_test$p.value > 0.05
      assumption_results$homoskedastis <- list(
        test = "Breusch-Pagan",
        p_value = bp_test$p.value,
        pass = homoskedastis_pass,
        status = if(homoskedastis_pass) "✓ TERPENUHI" else "✗ TIDAK TERPENUHI"
      )
    }, error = function(e) {
      assumption_results$homoskedastis <- list(
        test = "Breusch-Pagan",
        p_value = NA,
        pass = FALSE,
        status = "✗ ERROR"
      )
    })
    
    # 2. UJI NORMALITAS RESIDUAL
    residuals <- residuals(model)
    
    if(length(residuals) < 5000) {
      # Shapiro-Wilk Test
      sw_test <- shapiro.test(residuals)
      normalitas_pass <- sw_test$p.value > 0.05
      assumption_results$normalitas <- list(
        test = "Shapiro-Wilk",
        p_value = sw_test$p.value,
        pass = normalitas_pass,
        status = if(normalitas_pass) "✓ TERPENUHI" else "✗ TIDAK TERPENUHI"
      )
    } else {
      # Kolmogorov-Smirnov Test
      ks_test <- ks.test(residuals, "pnorm", mean(residuals), sd(residuals))
      normalitas_pass <- ks_test$p.value > 0.05
      assumption_results$normalitas <- list(
        test = "Kolmogorov-Smirnov",
        p_value = ks_test$p.value,
        pass = normalitas_pass,
        status = if(normalitas_pass) "✓ TERPENUHI" else "✗ TIDAK TERPENUHI"
      )
    }
    
    # 3. UJI MULTIKOLINEARITAS
    if(length(attr(terms(model), "term.labels")) > 1) {
      tryCatch({
        library(car)
        vif_values <- vif(model)
        max_vif <- max(vif_values)
        multikolinearitas_pass <- max_vif < 5
        assumption_results$multikolinearitas <- list(
          test = "VIF",
          max_vif = max_vif,
          pass = multikolinearitas_pass,
          status = if(multikolinearitas_pass) "✓ TERPENUHI" else 
            if(max_vif <= 10) "⚠ SEDANG" else "✗ TINGGI"
        )
      }, error = function(e) {
        assumption_results$multikolinearitas <- list(
          test = "VIF",
          max_vif = NA,
          pass = FALSE,
          status = "✗ ERROR"
        )
      })
    } else {
      assumption_results$multikolinearitas <- list(
        test = "VIF",
        max_vif = NA,
        pass = TRUE,
        status = "✓ TIDAK DIPERLUKAN"
      )
    }
    
    # 4. Nonautokorelasi
    tryCatch({
      model_residuals <- residuals(model)
      
      # Cek validitas geometri (disarankan)
      peta_data_valid <- st_make_valid(peta) 
      
      # Gunakan kolom ID unik di peta_data Anda jika diperlukan untuk poly2nb
      nb_queen <- poly2nb(peta_data_valid, queen = TRUE) 
      
      # Membuat matriks bobot spasial (row-standardized)
      lw_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)
      
      # Lakukan Uji Moran's I pada residu model
      moran_test <- moran.test(model_residuals, lw_queen, alternative = "two.sided")
      
      moran_i_value <- moran_test$estimate[1]
      moran_p_value <- moran_test$p.value
      
      # Hipotesis nol Moran's I adalah tidak ada autokorelasi (terpenuhi jika p-value >= 0.05)
      autokorelasi_terpenuhi <- moran_p_value >= 0.05 
      
      moran_status_text <- if(autokorelasi_terpenuhi) {
        "✓ TERPENUHI"
      } else {
        "✗ TIDAK TERPENUHI"
      }
      
      assumption_results$autokorelasi <- list( # Menggunakan 'autokorelasi' sesuai permintaan Anda
        test = "Moran's I",
        moran_i = moran_i_value,
        p_value = moran_p_value,
        pass = autokorelasi_terpenuhi,
        status = moran_status_text
      )
      
    }, error = function(e) {
      assumption_results$autokorelasi <- list( # Menggunakan 'autokorelasi' sesuai permintaan Anda
        test = "Moran's I",
        moran_i = NA,
        p_value = NA,
        pass = FALSE,
        status = paste0("ERROR: ", e$message) # Tetap tampilkan error jika ada
      )
    })
    
    cat("1. Homoskedastisitas: ", assumption_results$homoskedastis$status)
    cat("\n2. Normalitas Residual: ", assumption_results$normalitas$status)
    cat("\n3. Multikolinearitas: ", assumption_results$multikolinearitas$status)
    cat("\n4. Autokorelasi: ", assumption_results$autokorelasi$status)
    
    # KESIMPULAN KESELURUHAN
    total_tests <- length(assumption_results)
    passed_tests <- sum(sapply(assumption_results, function(x) x$pass))
    
    cat("\n\n")
    cat(sprintf("Jumlah Uji Asumsi: %d\n", total_tests))
    cat(sprintf("Uji Terpenuhi: %d\n", passed_tests))
    cat(sprintf("Uji Tidak Terpenuhi: %d\n", total_tests - passed_tests))
    cat(sprintf("Persentase Terpenuhi: %.1f%%\n\n", (passed_tests/total_tests)*100))
    
    # REKOMENDASI
    if(passed_tests == total_tests) {
      cat("✅ Model regresi memenuhi semua asumsi klasik.\n")
      cat("Model dapat digunakan untuk prediksi dan inferensi statistik.\n")
    } else if(passed_tests >= total_tests * 0.75) {
      cat("✅ GOOD! Model regresi memenuhi sebagian besar asumsi klasik.\n")
      cat("Model dapat digunakan dengan sedikit catatan pada asumsi yang tidak terpenuhi.\n")
    } else {
      cat("❌ POOR! Model regresi tidak memenuhi sebagian besar asumsi klasik.\n")
    }
  })
  
  output$assumption1 <- renderPrint({
    model <- model_fit
    
    # 1. UJI HOMOSKEDASTISITAS (Breusch-Pagan Test)
    cat("1. UJI HOMOSKEDASTISITAS\n")
    cat("   H₀: Varians error konstan (terdapat homoskedastis)\n")
    cat("   H₁: Varians error tidak konstan (terdapat heteroskedastis)\n")
    
    tryCatch({
      library(lmtest)
      bp_test <- bptest(model)
      cat("   Breusch-Pagan Test:\n")
      cat("   - Statistics BP =", round(bp_test$statistic, 4), "\n")
      cat("   - p-value =", round(bp_test$p.value, 4), "\n")
      if(bp_test$p.value > 0.05) {
        cat("   - Keputusan = GAGAL TOLAK H₀ (p > 0.05)\n\n")
        cat("Berdasarkan tingkat signifikansi 5%, p-value uji Breusch-Pagan lebih besar dari α, sehingga H₀ diterima.\n")
        cat("Dengan demikian, model regresi memenuhi asumsi homoskedastisitas atau variansi residual dianggap konstan.\n")
      } else {
        cat("   - Keputusan = TOLAK H₀ (p ≤ 0.05)\n\n")
        cat("Berdasarkan tingkat signifikansi 5%, p-value uji Breusch-Pagan kurang dari atau sama dengan α, sehingga H₀ ditolak.\n")
        cat("Dengan demikian, model regresi belum memenuhi asumsi homoskedastisitas atau terdapat heteroskedastisitas pada model.\n")
      }
    }, error = function(e) {
      cat("   Error dalam uji Breusch-Pagan:", e$message, "\n")
    })
  })
  
  output$assumption2 <- renderPrint({
    model <- model_fit
    
    # 2. UJI NORMALITAS RESIDUAL
    cat("2. UJI NORMALITAS RESIDUAL\n")
    cat("   H₀: Residual berdistribusi normal\n")
    cat("   H₁: Residual tidak berdistribusi normal\n")
    
    residuals <- residuals(model)
    
    # Shapiro-Wilk Test (untuk n < 5000)
    if(length(residuals) < 5000) {
      sw_test <- shapiro.test(residuals)
      cat("   Shapiro-Wilk Test:\n")
      cat("   - Statistics SW =", round(sw_test$statistic, 4), "\n")
      cat("   - p-value =", round(sw_test$p.value, 4), "\n")
      if(sw_test$p.value > 0.05) {
        cat("   - Keputusan = GAGAL TOLAK H₀ (p > 0.05)\n\n")
        cat("Berdasarkan tingkat signifikansi 5%, p-value uji Shapiro-Wilk lebih besar dari α, sehingga H₀ diterima.\n")
        cat("Dengan demikian, model regresi memenuhi asumsi normalitas error atau residual berdistribusi normal.\n")
      } else {
        cat("   - Keputusan = TOLAK H₀ (p ≤ 0.05)\n\n")
        cat("Berdasarkan tingkat signifikansi 5%, p-value uji Shapiro-Wilk kurang dari atau sama dengan α, sehingga H₀ ditolak.\n")
        cat("Dengan demikian, model regresi belum memenuhi asumsi normalitas error atau residual tidak berdistribusi normal.\n")
      }
    } else {
      # Kolmogorov-Smirnov Test untuk sampel besar
      ks_test <- ks.test(residuals, "pnorm", mean(residuals), sd(residuals))
      cat("   Kolmogorov-Smirnov Test:\n")
      cat("   - Statistics KS =", round(ks_test$statistic, 4), "\n")
      cat("   - p-value =", round(ks_test$p.value, 4), "\n")
      if(ks_test$p.value > 0.05) {
        cat("   - Keputusan = GAGAL TOLAK H₀ (p > 0.05)\n\n")
        cat("Berdasarkan tingkat signifikansi 5%, p-value uji Kolmogorov-Smirnov lebih besar dari α, sehingga H₀ diterima.\n")
        cat("Dengan demikian, model regresi memenuhi asumsi normalitas error atau residual berdistribusi normal.\n")
      } else {
        cat("   - Keputusan = TOLAK H₀ (p ≤ 0.05)\n\n")
        cat("Berdasarkan tingkat signifikansi 5%, p-value uji Kolmogorov-Smirnov kurang dari atau sama dengan α, sehingga H₀ ditolak.\n")
        cat("Dengan demikian, model regresi belum memenuhi asumsi normalitas error atau residual tidak berdistribusi normal.\n")
      }
    }
  })
  
  output$assumption3 <- renderPrint({
    cat("3. UJI MULTIKOLINEARITAS\n")
    cat("   Variance Inflation Factor (VIF)\n")
    cat("   - VIF < 5   : Tidak ada multikolinearitas\n")
    cat("   - VIF 5–10  : Multikolinearitas sedang\n")
    cat("   - VIF > 10  : Multikolinearitas tinggi\n\n")
    
    tryCatch({
      # Cek apakah model sudah tersedia
      if (!exists("model_fit")) {
        stop("Model belum dibuat.")
      }
      
      model <- model_fit  # pastikan ini adalah objek model, bukan reactive()
      
      # Cek apakah model punya lebih dari 1 variabel independen
      if (length(attr(terms(model), "term.labels")) > 1) {
        library(car)
        vif_values <- vif(model)
        
        cat("   Nilai VIF untuk masing-masing variabel:\n")
        for (i in seq_along(vif_values)) {
          var_name <- names(vif_values)[i]
          vif_val <- vif_values[i]
          status <- if (vif_val < 5) "BAIK" else if (vif_val <= 10) "SEDANG" else "TINGGI"
          cat(sprintf("   - %s: %.3f (%s)\n", var_name, vif_val, status))
        }
        
        max_vif <- max(vif_values)
        
        if (max_vif < 5) {
          cat("\n   Keputusan = TIDAK ADA MULTIKOLINEARITAS (semua VIF < 5)\n")
          cat("   Model memenuhi asumsi non-multikolinearitas\n")
        } else if (max_vif <= 10) {
          cat("\n   Keputusan = TERDAPAT MULTIKOLINEARITAS SEDANG (VIF 5–10)\n")
          cat("   Model belum sepenuhnya memenuhi asumsi non-multikolinearitas\n")
        } else {
          cat("\n   Keputusan = TERDAPAT MULTIKOLINEARITAS TINGGI (VIF > 10)\n")
          cat("   Model tidak memenuhi asumsi non-multikolinearitas\n")
        }
      } else {
        cat("   Model hanya memiliki satu variabel independen\n")
        cat("   Uji multikolinearitas tidak relevan\n")
      }
    }, error = function(e) {
      cat("   ❌ Error saat menghitung VIF:\n")
      cat("   ", e$message, "\n")
    })
  })
  
  output$assumption4 <- renderPrint({
    library(spdep)  # Pastikan paket ini sudah terpasang
    
    # 1. Model OLS awal
    model_ols <- lm(log(POVERTY) ~ NOELECTRIC + TAPWATER + NOSEWER + RENTED, data = data)
    resid_ols <- residuals(model_ols)
    
    # 2. Spatial Weights Matrix (misalnya antar distrik bersebelahan)
    n <- nrow(data)
    mat <- matrix(0, n, n)
    for (i in 1:(n-1)) {
      mat[i, i+1] <- 1
      mat[i+1, i] <- 1
    }
    lw <- mat2listw(mat)
    
    # 3. Uji Moran's I
    moran_result <- moran.test(resid_ols, lw)
    
    # 4. Output
    cat("4. UJI AUTOKORELASI SPASIAL (Moran's I)\n")
    cat("   H₀: Tidak ada autokorelasi spasial\n")
    cat("   H₁: Terdapat autokorelasi spasial\n\n")
    cat("   Moran's I =", round(moran_result$estimate[[1]], 4), "\n")
    cat("   p-value   =", round(moran_result$p.value, 4), "\n\n")
    
    if (moran_result$p.value < 0.05) {
      cat("   ➤ Keputusan: Tolak H₀ (Terdapat autokorelasi spasial)\n")
    } else {
      cat("   ➤ Keputusan: Gagal tolak H₀ (Tidak terbukti ada autokorelasi spasial)\n")
    }
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("Laporan_aINFRASEKA", Sys.Date(), switch(
        input$report_format,
        "html" = "html",
        "word" = "docx",
        "pdf" = "pdf"
      ))
    },
    
    content = function(file) {
      # Buat direktori sementara untuk menyimpan file Rmd dan outputnya
      temp_dir <- tempdir()
      file.copy("report.Rmd", temp_dir, overwrite = TRUE) # Pastikan Rmd berada di root aplikasi Shiny atau tentukan path lengkap
      
      # Tentukan output format berdasarkan input dari UI
      output_format <- switch(
        input$report_format,
        "html" = "html_document",
        "word" = "word_document",
        "pdf" = "pdf_document"
      )
      
      # Set parameter yang akan dilewatkan ke Rmd
      params <- list(
        report_data = reactiveValuesToList(global_report_data) # Konversi reactiveValues ke list biasa
      )
      
      # Render Rmd ke HTML
      rmarkdown::render(
        "www/report.Rmd", # Path ke file RMD Anda
        output_format = output_format,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv()) # Lingkungan baru untuk render
      )
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
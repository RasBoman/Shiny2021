
#### TAB1 import #### 

tab1_import_ohje <- p(paste("You can also drag & drop the table to the field above. Please note, that in a LajiGIS-compatible table",
                          "there should be 5 empty rows, headers are on row 6 and data starts from row 7."))






tab1_instructions_for_use <- wellPanel(p(paste("This application is built to help to ensure the quality of VELMU-data.",
                                               "There is a reference table loaded into the application until you upload your own survey data.",
                                               "After you upload your own data, the tabs should look about the same as now.",
                                               "The tabs are in order, in which it is recommended to go through the data:")),
                                       p("1. General overview of the table and locations"),
                                       p("2. Graphs to easily spot outliers in numerical columns"),
                                       p("3. Technical inspection, including duplicates and possible errors in excel-cells"),
                                       p("4. Species data, typos and outliers"),
                                       p("5. Map, to check that i.e. depth and dive line directions match the location"),
                                       p("6. Dive line tab. For examining specific lines, otherwise empty"),
                                       p("7. The whole table. For quickly filtering and examing the possible errors."))

#Tämä sovellus on tarkoitettu linjamuotoisen VELMU-kartoitusaineiston tarkistamiseen. Sovellukseen on esiladattu esimerkkitaulukko, ja ladattuasi oman taulukon välilehtien tulisi näyttää suunnilleen samalta. Välilehdet on asetettu järjestykseen, jossa aineistoa kannattaa käydä läpi:
 # 1. Yleiskatsaus aineistoon ja linjojen sijainteihin

#. Poikkeavuuksien ja selkeiden virheiden etsintä kuvaajien avulla

#3. Lajihavaintojen läpikäynti

#4. Linjojen sijaintien tarkempi läpikäynti, syvyyksien ja suuntien tarkastus



tab2_graphs_txt <- "LOREM IPSUM"


tab3_sidepanel_txt <- wellPanel(p("LajiGIS-tietokannan kannalta taulukon suhteen olennaista on sarake A eli kohteen numero."),
                                p("Kullakin kartoituspisteellä TULEE OLLA uniikki kohteen numero."),
                                p("Kohteen numerolla lajikohtaisia tietoja lukuunottamatta kaikkien muiden tietojen tulee olla identtisiä."),
                                p("Jos taulukot on täytetty oikein, tällä sivulla ei tule näkyä taulukoita, vaan kuittauksia että kaikki on kunnossa."),
                                p("Älä etene tältä välilehdeltä eteenpäin ennen kuin olet tehnyt vaaditut korjaukset!"))





#### SUOMEKSI ####

tab1_ohjeet <- wellPanel(p(paste("Tämä sovellus on rakennettu Metsähallituksen meriluonnonsuojelun toimesta helpottamaan VELMU-muotoisen kartoitusaineiston tarkastamista.",
                                 "Sovellukseen on ladattu esimerkkitaulukko,",
                                 "ja ladattuasi oman excel-taulukkosi sovellukseen välilehtien tulisi näyttää suurin piirtein samalta.",
                                 "Välilehdet ovat järjestyksessä, jossa aineistoa kannattaa käydä läpi:")),
                         p("1. Yleiskatsaus taulukkoon ja sijainteihin"),
                         p("2. Kuvaajat, joista on helppo huomata poikkeavuudet numeerisissa sarakkeissa"),
                         p("3. Tekniset tarkastukset, duplikaatit ja virheet taulukon muodossa"),
                         p("4. Lajiaineisto, typot ja poikkeavuudet"),
                         p("5. Kartta, mm. syvyyksien ja linjojen suunnan tarkastamiseen"),
                         p("6. Välilehti sukelluslinjojen yksittäiseen tarkasteluun. Pistemäisissä taulukoissa tyhjä."),
                         p("7. Koko taulukko, aineiston nopeaan tutkailuun ja suodattamiseen."))

tab1_upload_ohje <- p(paste("Voit myös tiputtaa taulukon yllä olevaan kenttään.",
                            "Huomioithan, että LajiGIS-yhteensopivassa taulukossa on 5 tyhjää riviä, otsikot ovat rivillä 6,",
                            "ja aineisto alkaa riviltä 7."))

tab1_periaate <- wellPanel(p(paste("Sovellusta kannattaa hyödyntää jo sisäänsyötön aikana, esimerkiksi aina 'näpyttelypäivän' päätteeksi.", 
                                   "Tällöin korjauksia pystyy tekemään ketterästi sitä mukaa kuin taulukko kasvaa, eikä lopputarkastus muodostu liian raskaaksi.",
                                   "Ehdotettu työskentelytapa:")),
                           br(),
                           p("1. Tiputa Excel-taulukko sovellukseen"),
                           p("2. Käy välilehtiä läpi"),
                           p("3. Tee samalla muutoksia Excel-taulukkoon"),
                           p("4. Tallenna muokattu Excel-taulukko sopivassa vaiheessa"),
                           p("5. Tiputa päivitetty Excel-taulukko sovellukseen. Toista kohtia 2-5, kunnes kaikki on kunnossa."))

tab1_ongelmat <- wellPanel(p(paste("Ongelmatapauksissa laita viestiä rasmus.boman@metsa.fi")))

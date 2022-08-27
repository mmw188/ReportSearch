############################################################################
##Project: RAND Report Search Tool
##Code: Define UI
##Date: 10 Mar 2021
##Author: Matt Walsh (mmw188@rand.org)
############################################################################

# Top band
header <- dashboardHeader(
    titleWidth = 300,
    title = "PAF Knowledge Map"
)

# Left band
sidebar <- dashboardSidebar(
    width = 300,
    # Viewable tabs
    sidebarMenu(
        id = "tabs",
        menuItem("Search", tabName = "tab_table",   icon = icon("chart-bar")),
        menuItem("Map",    tabName = "tab_network", icon = icon("chart-bar")),
        menuItem("About",  tabName = "intro",       icon = icon("info"))
    ),
    
    fluidRow(
        conditionalPanel(
            condition = "['tab_network', 'tab_table'].includes(input.tabs)",
            hr(),
            column(
                width = 12, align = "left", style = "width: 285px; margin: 0px 30px 0px 0px; padding: 0px",
                tags$div(
                    style = "width: 300px; padding: 0px 0px 0px 15px;",
                    searchInput(
                        inputId = "search_entry",
                        label = "Enter term(s) to view related reports", 
                        placeholder = NA_character_,
                        btnSearch = icon("search"),
                        width = "100%"
                    ) %>%
                        helper(type = "inline",
                               colour = 'white',
                               content = c('Type one or more words or phrases and hit enter. The search engine returns the reports that are most related to the words or phrases in order of relevance.'),
                               size = 's'),
                    textOutput('missing_term')
                )
            )
        ),
        conditionalPanel(
            condition = "['tab_network', 'tab_table'].includes(input.tabs)",
            hr(),
            column(
                width = 12, align = "left", style = "width: 285px; margin: 0px 30px 0px 0px; padding: 0px",
                tags$div(
                    style = "width: 300px; padding: 0px 0px 0px 15px;",
                    selectizeInput(
                        inputId = "search_topic",
                        label = "Select title(s) to view related reports",
                        multiple = TRUE,
                        choices = c('Select one' = '', topic.list)
                    ) %>% helper(type = "inline",
                                 colour = 'white',
                                 content = c('Select one or more RAND PAF reports by title. The search engine returns other reports that are most related to the selected work(s) in order of relevance.'),
                                 size = 's')
                )
            )
        ),
        conditionalPanel(
            condition = "['tab_network', 'tab_table'].includes(input.tabs)",
            hr(),
            column(
                width = 12, align = "left", style = "width: 285px; margin: 0px 30px 0px 0px; padding: 0px",
                tags$div(
                    style = "width: 300px; padding: 0px 0px 0px 15px;",
                    fileInput('file_input',
                              'Upload file to view related reports',
                              accept = c('.pdf', '.txt', '.html', '.rtf', '.docx', '.doc')) %>%
                        helper(type = "inline",
                               colour = 'white',
                               content = c('Select and upload a document from your computer. The search engine returns the reports that are most related to the uploaded document in order of relevance.'),
                               size = 's')
                )
            )
        ),
        conditionalPanel(
            condition = "['tab_table'].includes(input.tabs)",
            hr(),
            column(
                width = 12, align = "left", style = "width: 285px; margin: 0px 30px 0px 0px; padding: 0px",
                hr(),
                tags$div(
                    style = "width: 300px; padding: 0px 0px 0px 15px;",
                    selectizeInput(
                        inputId = "program", label = "Filter results by RAND PAF program", multiple = TRUE,
                        choices = c('Select one' = '', program.list)
                    )
                )
            )
        ),
        conditionalPanel(
            condition = "['tab_table'].includes(input.tabs)",
            hr(),
            column(
                width = 12, align = "left", style = "width: 285px; margin: 0px 30px 0px 0px; padding: 0px",
                tags$div(
                    style = "width: 300px; padding: 0px 0px 0px 15px;",
                    sliderInput("years", "Filter results by year",
                                min = 2000, max = 2022,
                                value = c(2000, 2022),
                                sep = '',
                                ticks = FALSE
                    ),
                    hr()
                ),
            )
        ),
    )
)

# Main display
body <- dashboardBody(
    # Formatting options
    useShinyjs(),
    
    # Tabs list
    tabItems(
        # Intro Tab
        tabItem(
            tabName = "intro",
            fluidRow(
                column(
                    # Theme settings
                    width = 12, align = "left",
                    # Display
                    h4(strong('PAF Knowledge Map Overview')),
                    p("The PAF Knowledge Map is a tool for exploring and retrieving reports about issues pertinent to 
                      the Department of the Air Force (DAF). The tool contains all unclassified, releasable RAND PAF 
                      reports published since 2000.", 
                      style = "font-size:14px"),
                    h4(strong('Technical Approach')),
                    p(HTML("The PAF Knowledge Map reads all the words contained in each RAND PAF report. It applies a technique 
                    called latent semantic analysis (LSA) to the collections of words contained in reports. LSA discovers a 
                    set of underlying topics present across the complete set of reports. After LSA is applied, each document 
                    is represented as a mixture of topics, rather than as a collection of words.<br><br>
                    By representing reports in this way, it becomes possible to compute similarity metrics. This underlies three common uses of LSA:"), style = "font-size:14px"),
                    tags$ol(tags$li(p(HTML("Return documents that are semantically related to a search phrase even if they do not contain the exact terms (Search tab)"), style = "margin-bottom: 0px;")),
                            tags$li(p(HTML("Return documents that are semantically related to another document (Search tab)"), style = "margin-bottom: 0px;")),
                            tags$li(p(HTML("Discover clusters of semantically related documents (Map tab)"), style = "margin-bottom: 0px;"))
                    ),
                    h4(strong('Use Cases')),
                    p("The Knowledge Map can be used for both targeted searches and exploratory searches. Example use cases include", 
                      style = "font-size:14px"),
                    tags$ul(tags$li(p(HTML("Finding RAND PAF reports on a particular topic in a specific time frame. For example, search for 'command and control' to find reports over the last five years."), style = "margin-bottom: 0px;")),
                            tags$li(p(HTML("Seeking out reports related to a document of interest. For example, upload a document, such as a white paper, news article, or academic article, to find related PAF reports."), style = "margin-bottom: 0px;")),
                            tags$li(p(HTML("Discovering other reports related to one or a series of RAND PAF reports. For example, select one or multiple PAF reports to find other related PAF reports."), style = "margin-bottom: 0px;")),
                            tags$li(p(HTML("Exploring clusters of related work. For example, use the Search tab to find relevant PAF reports based on an input, and use the Map tab 
                                           to explore clusters of related work that do not appear in the table of results. For instance, some of the reports related to recruiting 
                                    might appear adjacent to a cluster of reports on diversity, and others might appear adjacent to a cluster of reports on cyber career fields."), style = "margin-bottom: 0px;"))
                    )
                )
            )
        ),
        
        tabItem(
            tabName = "tab_table",
            fluidRow(
                column(
                    width = 12,
                    p(HTML('The <b> Search Table </b> displays RAND PAF reports sorted from the most to least relevant based on your search inputs. You may use the Search Table in three ways:'), style = "margin-bottom: 6px;"),
                    tags$ol(tags$li(p(HTML("<i>Enter term(s)</i>. This option allows you to enter key words. The search engine returns the reports that are most related to the key words."), style = "margin-bottom: 0px;")),
                            tags$li(p(HTML("<i>Select title(s)</i>. This option allows you to select RAND PAF reports by titles, either from the results of a key word search or from the full RAND PAF database. The search engine returns other reports that are most related to the selected work(s)."), style = "margin-bottom: 0px;")),
                            tags$li(p(HTML("<i>Upload file</i>. This option allows you to upload a document from your computer. The search engine returns the reports that are most related to the uploaded document."), style = "margin-bottom: 0px;"))
                    )
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    DTOutput("table_results", height = "800px")
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    p(),
                    downloadButton('download_table', label = "Download Reults")
                )
            )
        ),
        
        tabItem(
            tabName = "tab_network",
            fluidRow(
                column(
                    width = 10,
                    p(HTML('The <b> Knowledge Map </b> displays PAF reports clustered by similarity. You can use it to <b> explore clusters of reports that are related
                    to topics that you are interested in</b>. On the left, enter search term(s), select titles of RAND PAF reports in the drop-down menu, or upload your own document to discover 
                    related reports and related clusters of research. Hover over individual reports for more information. To help explore the map, click and hold to reposition it within the window, or 
                    zoom in on particular areas.<br><br>'))
                )
            ),
            fluidRow(
                column(
                    width = 3,
                    plotOutput("plot_legend", height = "800px")
                ),
                column(
                    width = 7,
                    visNetworkOutput("plot_network", height = "800px")
                )
            )
        )
    )
)

dashboardPage(
    # Color theme for officer
    #   if this changes the HTML for output$data_theme will need to change as well. Should just need to replace .skin-blue
    #   with .skin-NEWCOLOR
    skin = "blue",
    header,
    sidebar,
    body,
    tags$head(
        tags$style(HTML(
            ".sidebar { height: 2000px; overflow-y: auto; overflow-x: hidden }"
        ))            # close tags$style
    )
)


#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
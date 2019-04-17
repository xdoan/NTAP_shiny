shinyServer(function(input, output, session) {
  session$sendCustomMessage(type = "readCookie", message = list())
  setBookmarkExclude(c("cookie", "authorized"))

  ## Show message if user is not logged in to synapse
  unauthorized <- observeEvent(input$authorized, {
    showModal(modalDialog(
      title = "Not logged in",
      HTML("You must log in to <a href=\"https://www.synapse.org/\">Synapse</a> to use this application. Please log in, and then refresh this page.")
    ))
  })
  
  foo <- observeEvent(input$cookie, {
    
    synLogin(sessionToken=input$cookie)
      
    withProgress(message = 'Loading data...',
               {source("getDataNTAP.R")})
  
    output$centersBox <- renderInfoBox({
      centers <- n_distinct(ntap_center_study_summary_df$projectId)
      infoBox(
        "Studies", centers, icon = icon("university"), ### changed to studies
        color = "light-blue", fill = TRUE
      )
    })

    output$filesBox <- renderInfoBox({
      files <- sum(ntap_center_study_summary_df$fileId)
      infoBox(
        "Files", files, icon = icon("file"),
        color = "light-blue", fill = TRUE
      )
    })

    output$samplesBox <- renderInfoBox({
      samples <- sum(
        sum(ntap_center_study_summary_df$individualID),
        # sum(ntap_center_study_summary_df$cellLine),
        sum(ntap_center_study_summary_df$specimenID)
      )
      infoBox(
        "Samples", samples, icon = icon("tag"),
        color = "light-blue", fill = TRUE
      )
    })

    output$pubsBox <- renderInfoBox({
      pubs <- ntap_summary_df %>% 
        group_by(projectId) %>% 
        summarise(value = unique(publication_count)) %>% 
        pull(value) %>%
        sum()
      infoBox(
        "Publications", pubs, icon = icon("pencil"),
        color = "light-blue", fill = TRUE
      )
    })

    output$consortium_files <- renderpier({
      consortium_donut(consortium_counts, "fileId", "Files")
    })

    output$consortium_studies <- renderpier({
      consortium_donut(consortium_counts, "study", "Studies")
    })

    output$consortium_cancers <- renderpier({
      consortium_donut(consortium_counts, "tumorType", "Tumor types")
    })

    output$consortium_data <- renderpier({
      consortium_donut(consortium_counts, "assay", "Assay")
    })

    output$kp_activity <- plotly::renderPlotly({
      ggplotly(p_ntap, tooltip = "text") %>%
        layout(font = list(family = "Lato"),
                legend = list(orientation = 'h',
                              y = 1, x = 0, yanchor = "bottom"),
                margin = list(l = 100, b = 60, r = 60)) %>% 
        plotly::config(displayModeBar = F)  
    })

    output$consortium_summary <- plotly::renderPlotly({
      plot_keys <- list(assay = "Assay", tumorType = "Tumor Type",
                        diagnosis = "Diagnosis", species = "Species",
                        resourceType = "Resource Type", dataType = "Data Type",
                        study = "Study", nf1Genotype = "NF1 Genotype",
                        nf2Genotype = "NF2 Genotype") #, cellLine = "Cell Lines")
      
      chart <- ntap_summary_df %>%
        plot_file_counts_by_annotationkey(plot_keys, chart_height = 300) %>% 
        plotly::layout(font = list(family = "Lato"))
      
      chart
    })

    output$annotationkey_counts <- plotly::renderPlotly({
      plot_keys <- config$annotationkey_value[c(
        input$sample_fill_select,
        input$sample_group_select
      )]
      ntap_summary_df %>%
        filter(!is.na(assay), 
                !(assay %in% c("null", "Not Applicable"))) %>% 
        filter(!is.na(tumorType), 
                !(tumorType %in% c("null", "Not Applicable"))) %>% 
        # filter(!is.na(resourceType),
        #        !(resourceType %in% c("null", "Not Applicable"))) %>%
        plot_sample_counts_by_annotationkey_2d_NTAP(
          sample_key = input$sample_type,
          annotation_keys = plot_keys,
          filter_missing = FALSE
        ) %>% 
        plotly::layout(font = list(family = "Lato"),
                        showlegend = FALSE)
    })

    output$files_per_month <- plotly::renderPlotly({
      sg_facet_chr <- input$sg_facet
      sg_facet <- as.name(input$sg_facet)
      
      plot_df <- ntap_summary_df %>% 
        select(createdOn_file, rlang::UQ(sg_facet)) %>% 
        mutate(month = as.Date(cut(createdOn_file, breaks = "month"))) %>% 
        filter(!is.na(month)) %>% 
        group_by(month, rlang::UQ(sg_facet)) %>% 
        tally() %>% 
        ungroup() %>%
        complete(month = seq.Date(min(month), max(month), by="month"), 
                  rlang::UQ(sg_facet)) %>% 
        replace_na(list(0L, "Not Annotated") %>% 
                      set_names(c("n", sg_facet_chr))) 
      
      if (input$sg_cumulative) {
        plot_df <- plot_df %>% 
          group_by(rlang::UQ(sg_facet)) %>%
          mutate(n = cumsum(n)) %>% 
          ungroup()
      } 
      plot_df <- plot_df %>% 
        mutate(
          month_pretty = str_c(
            lubridate::month(month, label = TRUE),
            lubridate::year(month),
            sep = ". "
          ),
          label = glue::glue(
            "<b>{value}:</b>\n{month}: {count} files uploaded",
            value = rlang::UQ(sg_facet),
            month = month_pretty,
            count = n),
          n = log10(n + 1)
        ) %>% 
        rename(sg_facet = rlang::UQ(sg_facet))
      
      p <- plot_df %>% 
        ggplot(aes(x = month, y = n, text = label)) + 
        geom_col(aes(fill = sg_facet), 
                  colour = "slategray", size = 0.3, alpha = 1) + 
        scale_fill_viridis_d() +
        theme_bw() + 
        xlab(" ") +
        scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y") + 
        scale_y_continuous(expand = c(0, 0)) +
        ylab("log10(Files)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p, tooltip = "text") %>%
        layout(font = list(family = "Lato"),
                showlegend = FALSE) %>% 
        plotly::config(displayModeBar = F)
    })

    output$center_summary <- DT::renderDT({
      ntap_center_summary_df %>% 
        select(
          `Study Name` = name_project,
          Studies = study,
          Files = fileId,
          Assays = assay,
          Individuals = individualID,
          Specimens = specimenID,
          Tools = tool
        ) %>% 
        datatable(
          selection = list(
            mode = 'single'
          ),
          options = list(
            scrollX = TRUE,
            autoWidth = F,
            dom = "tip"
          ),
          rownames = FALSE
        ) %>%
        formatStyle(
          'Files',
          backgroundColor = styleEqual(0.0, 'lightpink')
        )
    }, server = FALSE
    )

    observeEvent(input$center_summary_rows_selected, {
      output$center_name <- renderText({
        center_row <- input$center_summary_rows_selected
        ntap_center_summary_df[[center_row, "name_project"]]
      })
    })

    observeEvent(input$center_summary_rows_selected, {
      output$center_metadata <- function() {
        center_row <- input$center_summary_rows_selected
        selected_center <- ntap_center_summary_df[[center_row, "projectId"]]
        center_data_df <- ntap_center_summary_df %>%
          filter(projectId == selected_center) %>%
          select(project_vars) %>%
          select(-dplyr::matches("(On_project)")) %>%
          mutate(`Synapse Project` = projectId) %>%
          create_synapse_links(list(`Synapse Project` = "projectId") )%>%
          select(-projectId, -name_project) %>%
          gather(field, val) %>%
          mutate(field = str_to_title(field),
                  field = case_when(
                    # field == "Grantnumber" ~ "Grant Number",
                    # field == "Granttype" ~ "Grant Type",
                    field == "Publication_count" ~ "Publications",
                    field == "Fundingagency" ~ "Funding Agengy",
                    # field == "Publication_geodata_produced" ~ "GEO Datasets",
                    TRUE ~ field
                  ),
                  field = str_c("<b>", field, "</b>")
          ) 
        
        knitr::kable(center_data_df, "html", escape = FALSE, col.names = NULL,
                      align = c('r', 'l')) %>% 
          kable_styling("striped", full_width = T)
      }
    })

    observeEvent(input$center_summary_rows_selected, {
      output$center_details <- plotly::renderPlotly({
        if (length(input$center_summary_rows_selected)) {
          center_row <- input$center_summary_rows_selected
          selected_center <- ntap_center_summary_df[[center_row, "projectId"]]
          center_df <- ntap_summary_df %>% 
            filter(projectId == selected_center)
          
          p <- center_df %>%
            mutate(tool = ifelse(!is.na(summary_y), study, NA)) %>%
            select(
              # Studies = study,
              `Tumor Types` = tumorType,
              Species = species,
              Assays = assay,
              `Resource Type` = resourceType
            ) %>%
            gather(attr, val) %>%
            group_by(attr, val) %>%
            tally() %>%
            replace_na(list(val = "Not Annotated")) %>%
            group_by(attr) %>%
            mutate(files = sum(n)) %>%
            ungroup() %>%
            dplyr::mutate(label = glue::glue(
              "<b>{value}:</b>\n{count} of {total} files",
              value = val,
              count = n,
              total = files)
            ) %>%
            ggplot(aes(x = 1, y = n, text = label)) +
            geom_col(aes(fill = val), 
                      position = position_stack(reverse = FALSE),
                      colour = "slategray", size = 0.3, alpha = 1) +
            guides(fill = FALSE) +
            scale_fill_viridis_d() +
            scale_y_continuous(expand = c(0, 0)) +
            xlab("") +
            ylab("Number of Files") +
            facet_grid(. ~ attr, scales = "free_y") +
            theme_bw() +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank()) %>%
            I
          ggplotly(p, tooltip = "text") %>%
            layout(font = list(family = "Lato"),
                    showlegend = FALSE, margin = list(l = 80)) %>%
            plotly::config(displayModeBar = F)
        }
      })
      # })
    })
    observeEvent(input$center_summary_rows_selected, {
      output$center_files_per_month <- plotly::renderPlotly({
        if (length(input$center_summary_rows_selected)) {
          center_row <- input$center_summary_rows_selected
          selected_center <- ntap_center_summary_df[[center_row, "projectId"]]
          center_df <- ntap_summary_df %>% 
            filter(projectId == selected_center)
          
          sb_facet_chr <- input$sb_facet
          sb_facet <- as.name(input$sb_facet)
          
          plot_df <- center_df %>%
            select(createdOn_file, rlang::UQ(sb_facet)) %>%
            mutate(month = as.Date(cut(createdOn_file, breaks = "month"))) %>%
            filter(!is.na(month)) %>%
            group_by(month, rlang::UQ(sb_facet)) %>%
            tally() %>%
            ungroup() %>%
            complete(month = seq.Date(min(month), max(month), by="month"),
                      rlang::UQ(sb_facet)) %>%
            replace_na(list(0L, "Not Annotated") %>%
                          set_names(c("n", sb_facet_chr)))
          
          if (input$sb_cumulative) {
            plot_df <- plot_df %>%
              group_by(rlang::UQ(sb_facet)) %>%
              mutate(n = cumsum(n)) %>%
              ungroup()
          }
          plot_df <- plot_df %>%
            mutate(
              month_pretty = str_c(
                lubridate::month(month, label = TRUE),
                lubridate::year(month),
                sep = ". "
              ),
              label = glue::glue(
                "<b>{value}:</b>\n{month}: {count} files uploaded",
                value = rlang::UQ(sb_facet),
                month = month_pretty,
                count = n),
              n = log10(n + 1)
            ) %>%
            rename(sb_facet = rlang::UQ(sb_facet))
          
          p <- plot_df %>%
            ggplot(aes(x = month, y = n, text = label)) +
            geom_col(aes(fill = sb_facet),
                      colour = "slategray", size = 0.3, alpha = 1) +
            scale_fill_viridis_d() +
            theme_bw() +
            xlab(" ") +
            scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y") +
            scale_y_continuous(expand = c(0, 0)) +
            ylab("log10(Files)") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          ggplotly(p, tooltip = "text") %>%
            layout(font = list(family = "Lato"),
                    showlegend = FALSE) %>%
            plotly::config(displayModeBar = F)

        }
        
      })
    })
  
  })
})   

  
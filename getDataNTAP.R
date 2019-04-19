cat(file=stderr(), "reading data...", "\n")

### syndccutils functions 
get_table_df <- function(table_id, cache = FALSE) {
  if (cache) {
    viewcache_dir <- "data/viewcache"
    if (!fs::dir_exists(viewcache_dir)) {
      fs::dir_create(viewcache_dir, recursive = TRUE)
    }
    view_file <- fs::path(viewcache_dir,
                          stringr::str_c(table_id, ".feather"))
    if (!fs::file_exists(view_file)) {
      syn_table_data <- synapser::synTableQuery(
        sprintf("select * from %s", table_id),
        includeRowIdAndRowVersion = FALSE
      )
      feather::write_feather(syn_table_data$asDataFrame(), view_file)
      return(syn_table_data$asDataFrame())
    } else {
      return(feather::read_feather(view_file))
    }
  } else {
    syn_table_data <- synapser::synTableQuery(
      sprintf("select * from %s", table_id),
      includeRowIdAndRowVersion = FALSE
    )
    syn_table_data$asDataFrame()
  }
}

create_synapse_links <- function(
    df, link_keys
) {
    base_url <- "https://www.synapse.org/#!Synapse:"
    link_template <- glue::glue(
        "<a href='{base}{{id}}' target='_blank'>{{target}}</a>",
        base = base_url
    )
    link_keys %>%
        walk2(names(.), function(id_key, target_key) {
            target_col <- as.name(target_key)
            id_col <- as.name(id_key)
            df <<- df %>%
                mutate(
                    UQ(target_col) :=
                        ifelse(!is.na(UQ(target_col)),
                               glue::glue(
                                   link_template,
                                   id = UQ(id_col),
                                   target = UQ(target_col)
                               ),
                               UQ(target_col))
                )
        })
    df
}

plot_file_counts_by_annotationkey <- function(
    view_df, annotation_keys, replace_missing = "Not Annotated",
    chart_height = NULL
) {

    chart <- annotation_keys %>%
        map2(.y = names(.), function(annotation_prettykey, annotation_key) {
            key_col <- as.name(annotation_key)
            plot_df <- view_df %>%
                group_by(.dots = annotation_key) %>%
                tally() %>%
                mutate_at(.vars = annotation_key,
                                 funs(replace(., is.na(.), replace_missing))) %>%
                mutate(UQ(key_col) := forcats::fct_relevel(
                    UQ(key_col), replace_missing, after = 0L
                )) %>%
                mutate(label = glue::glue(
                    "<b>{value}:</b>\n{count} files",
                    value = UQ(key_col),
                    count = n
                ))

            p <- plot_df %>%
                ggplot(aes(x = 1, y = n, text = label)) +
                geom_col(aes_(fill = as.name(annotation_key)),
                                  position = position_stack(reverse = FALSE),
                                  colour = "white", size = 0.2) +
                scale_fill_viridis_d() +
                xlab(annotation_prettykey) +
                ylab("Number of Files") +
                scale_x_continuous(expand = c(0, 0)) +
                scale_y_continuous(expand = c(0, 0)) +
                custom_theme_bw() +
                theme(axis.text.x = element_blank(),
                               axis.ticks.x = element_blank()) +
                guides(fill = FALSE)

            plotly::ggplotly(p, tooltip = "text",
                     width = 100 * length(annotation_keys) + 50,
                     height = chart_height)
        }) %>%
        plotly::subplot(shareY = TRUE, titleX = TRUE) %>%
        plotly::layout(showlegend = FALSE,
                       font = list(family = "Roboto, Open Sans, sans-serif")) %>%
        plotly::config(displayModeBar = F)
    chart
}

# synapser::synLogin(sessiontoken=input$cookie)
ntap_summary_df <- get_table_df("syn18496443", cache = TRUE) ## moved to NTAP folder

ntap_summary_df <- ntap_summary_df %>% 
  mutate_at(.vars = vars(dplyr::matches("(createdOn|modifiedOn)")),
            .funs = funs(lubridate::as_datetime(floor(. / 1000)))
            ) %>%
  mutate(study = name_project)

project_vars <- c("projectId", "name_project", "institutions", "fundingAgency",
                  "publication_count", "consortium")

count_vars <- c("fileId", "individualID", "specimenID", 
                "assay", "tool")

ntap_center_study_summary_df <- ntap_summary_df %>% 
  mutate(tool = ifelse(!is.na(summary_y), name_project, NA)) %>% 
  group_by(.dots = c(project_vars, "name_project")) %>% 
  summarise_at(.vars = count_vars,
               .funs = funs(n_distinct(., na.rm = TRUE))) %>% 
  replace_na(list(study = "Not Annotated")) %>% 
  mutate(study = ifelse(fileId == 0, NA, name_project))

### have to standardize consortium across projects bc the annotations in synapse aren't ready yet
ntap_center_study_summary_df <- ntap_center_study_summary_df %>%
  group_by(projectId) %>%
  fill(consortium) %>%
  fill(consortium, .direction = "up")

ntap_center_summary_df <- ntap_center_study_summary_df %>% 
  group_by(projectId) %>% 
  summarise(study = n_distinct(study, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(
    ntap_center_study_summary_df %>% 
      select(-study) %>% 
      group_by(.dots = project_vars) %>% 
      summarise_all(sum),
    by = "projectId"
  )

ntap_consortium_counts <- ntap_summary_df %>% 
  group_by(consortium) %>% 
  summarize_at(
    c("fileId", "study", "tumorType", "assay"), 
    n_distinct , na.rm = TRUE  ### added na.rm 
  )

plot_ntap_df <- ntap_center_study_summary_df %>% 
  mutate(sample = individualID + specimenID) %>% 
  group_by(name_project, consortium) %>% 
  summarise(avg_files = mean(fileId),
            study = n_distinct(study, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(study) %>% 
  mutate(name_project = fct_inorder(name_project),
         label = glue::glue(
           "<b>{value}:</b>\n{count} studies\n{avg} files per study",
           value = name_project,
           count = study,
           avg = avg_files
         )
  )

### wrap long labels
label_wrap_gen3 <- function(width = 100) {
  function(variable, value) {
    lapply(strwrap(as.character(value), width=width, simplify=FALSE), 
           paste, collapse="\n")
  }
}

p_ntap <- plot_ntap_df %>% 
  filter(.$avg_files >= 1) %>%
  mutate( consortium = gsub("cNF Initiative", "cNF", .$consortium)) %>%
  mutate(consortium = gsub("Francis Collins Scholars", "Collins", .$consortium)) %>%
  ggplot(aes(x = name_project, y = avg_files)) +
  geom_col(aes(fill = avg_files, text = label), 
           colour = "slategray", size = 0.3, alpha = 1) +
  # geom_point(y = 0.2, colour = "white", size = 2) +
  # geom_point(aes(colour = consortium), y = 0.2, size = 1.5) +
  coord_flip() +
  # scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2" ,"#D55E00", "#CC79A7", "#999999")) +
  guides(alpha = FALSE, fill = FALSE, colour = guide_legend(title = NULL)) +
  xlab("") +
  ylab("Average Files in Synapse") +
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(consortium ~ ., 
             scales = "free_y", space = "free_y",
             drop = TRUE) +
  theme_bw() +
  theme(strip.text.y = element_text(face = "bold", angle = 270),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1))


### editing from syndccutils
custom_theme_bw <- function() {
  theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          plot.title = element_text(face = "bold"))
}

plot_sample_counts_by_annotationkey_2d_NTAP <- function(
  view_df, sample_key = c("individualID", "specimenID"),
  annotation_keys, filter_missing = TRUE
) {
  # TODO: add some check to make sure length(annotation_keys) == 2
  if (filter_missing) {
    view_df <- view_df %>%
      filter_at(vars(one_of(c(names(annotation_keys), sample_key))),
                all_vars(!is.na(.) & !(. %in% c("null", "Not Applicable"))))
  }
  
  fill_vals <- unique(view_df[[names(annotation_keys)[1]]])
  bar_vals <- unique(view_df[[names(annotation_keys)[2]]])
  num_bars <- length(bar_vals)
  
  fill_margin <- max(map_int(fill_vals, stringr::str_length))
  bar_margin <- max(map_int(bar_vals, stringr::str_length))
  
  sample_labels <- list(individualID = "Individuals",
                        specimenID = "Specimens",
                        # cellLine = "Cell Lines",
                        id = "Files")
  
  replace_missing <- "Not Annotated"
  plot_df <- view_df %>%
    group_by(.dots = names(annotation_keys)) %>%
    summarize(n = n_distinct(UQ(as.name(sample_key)), na.rm = TRUE ) ) %>%
    ungroup() %>%
    mutate_at(.vars = names(annotation_keys),
              funs(replace(., is.na(.), replace_missing))) %>%
    mutate_at(.vars = names(annotation_keys),
              funs(forcats::fct_infreq(.))) %>%
    mutate_at(.vars = names(annotation_keys),
              funs(forcats::fct_rev(.))) %>%
    # mutate_at(.vars = names(annotation_keys),
    #                  funs(forcats::fct_relevel(., "Not Annotated"))) %>%
    mutate(label = glue::glue(
      "<b>{assay}:</b>\n{count} {samples}",
      assay = UQ(as.name(names(annotation_keys)[1])),
      count = n,
      samples = stringr::str_to_lower(sample_labels[[sample_key]]))
    )
  
  p <- plot_df %>%
    ggplot(aes_string(x = names(annotation_keys)[2], y = "n",
                      text = "label")) +
    geom_col(aes_string(fill = names(annotation_keys[1])),
             colour = "white", size = 0.2) +
    scale_fill_viridis_d(annotation_keys[[1]]) +
    xlab("") +
    ylab(glue::glue("Number of {label}",
                    label = sample_labels[[sample_key]])) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    custom_theme_bw()
  
  plotly::ggplotly(p, tooltip = 'text', height = num_bars * 40 + 155) %>%
    plotly::layout(margin = list(l = 10 + bar_margin * 6,
                                 r = 10 + fill_margin * 0,
                                 b = 55),
                   font = list(family = "Roboto, Open Sans, sans-serif"),
                   legend = list(tracegroupgap = 3, traceorder = "reversed",
                                 yanchor = "top", y = 1)) %>%
    plotly::config(displayModeBar = F)
}

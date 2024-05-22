#load libraries
library(tidyverse)
library(flextable)
library(agricolae)
library(scales)
library(janitor)

caet_performance_scatter <-
    function(x, #Set x axis variable (A CAET Score)
             y, #Set y axis variable (Performance indicator)
             lob = NULL, #set method for line of best fit, default = loess moving average, can also set to linear model with = "lm"
             teams = NULL, #choose which teams to include in the analysis
             #(will default to all teams in the data, but individual countries will only be provided with their data) - no need to set this
             facet = NULL, #choose variable to facet the graphs by (usually location but welcome to use others)
             ytitle = NULL, #set the y axis title
             xtitle = NULL, #set the x axis title
             min = NULL, #set the y axis scale minimum
             max = NULL, #set the y axis scale maximum
             scale = NULL# set the scale (either will be unchanged or can set to "log10" for monetary variables)
             ) {
        data$tmpx <- data[, x] #create new x variable column

        data$tmpy <- data[, y] #create new y variable column

        data$facet <- data[, facet] #create new facet variable column

        if(is.null(max)){

            max <- max(data$tmpy, na.rm = T) #if max is unset, take max of the variable
        }


        if(is.null(min)){

            min <- min(data$tmpy, na.rm = T) #if min us unset, take the min of the variable
        }


        if (!is.null(teams)) {

            data <- data %>%
                dplyr::filter(team_id %in% teams) #filter down to just the selected teams if this argument is set (shouldnt be)

        }

        x <- #create the scatterplot
            ggplot2::ggplot(data, aes(
                x = tmpx,
                y = tmpy,
                color = as.factor(pro_soils_group)
            )) +
            ggplot2::geom_point(size = 1) +
            ggplot2::scale_colour_manual(
                values = c("grey4", "forestgreen"),
                labels = c("Non-ProSoils", "ProSoils")
            ) +
            ggplot2::labs(y = ytitle,
                          x = xtitle,
                          color = NULL) +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.position = "bottom")+
            ggplot2::scale_x_continuous(limits = c(0,100))+
            ggplot2::scale_y_continuous(limits = c(min,max))

        if (!is.null(facet)) {
            x <- x +
                ggplot2::facet_wrap(~ facet) #add in the facet to the graph if set
        }

        if (!is.null(lob)) {
            x <- x +
                ggplot2::geom_smooth(method = lob) #set the method for the line of best fit if specified
        } else{
            x <- x +
                ggplot2::geom_smooth()
        }

        if(!is.null(scale)){
            x <- x+
                ggplot2::scale_y_log10() #add the log 10 scale if selected
        }

        return(x)

    }

correlations_table <- function(indicator) { #only need to supply the indicator
    data$indicator <- data[, indicator]

    x <- data %>%
        dplyr::select(totscore_caet, starts_with("stand_"), indicator, pro_soils_group) %>% #select the indicator and caet score columns
        tidyr::pivot_longer(cols = c(totscore_caet, starts_with("stand")), names_to = "dimension") %>% #convert to long form
        dplyr::mutate(
            dimension = case_when(
                dimension == "totscore_caet" ~ "Total Score",
                dimension == "stand_diversity" ~ "Diversity",
                dimension == "stand_syn" ~ "Synergies",
                dimension == "stand_rec" ~ "Recycling",
                dimension == "stand_eff" ~ "Efficiency",
                dimension == "stand_res" ~ "Resillience",
                dimension == "stand_cultfood" ~ "Culture & Food Tradition",
                dimension == "stand_cocrea" ~ "Co-creation & Sharing of Knowledge",
                dimension == "stand_human" ~ "Human & Social Values",
                dimension == "stand_circular" ~ "Circular & Solidarity Economy",
                dimension == "stand_respgov" ~ "Responsible Governance"
            ) #redefine the names of the scores
        ) %>%
        tidyr::nest(data = c(indicator, value)) %>% #nest the data within each dimension, i.e. 11 different datasets in a list which we will run the same function over
        dplyr::mutate(data = purrr::map(data, ~ {
            out <- cor.test(.x$value, .x$indicator, method = "spearman") #within each dataset run a correlation test
            tibble::tibble(rho = out$estimate[1],#take the correlation coefficient
                           p.value = out$p.value) #p value , is this different from 0
        })) %>%
        tidyr::unnest(cols = data) %>% #turn back to 1 dataset
        dplyr::mutate(pro_soils_group = ifelse(pro_soils_group == 1,  "ProSoils", "Non-ProSoils")) %>%
        tidyr::pivot_wider(names_from = pro_soils_group,
                           values_from = c(rho, p.value)) %>% #pivot to wide
        dplyr::select(
            dimension,
            rho_ProSoils,
            p.value_ProSoils,
            `rho_Non-ProSoils`,
            `p.value_Non-ProSoils` #rearrange columns
        ) %>%
        flextable::flextable() %>%
        flextable::set_header_labels( #rename columns
            values = c(
                dimension = "Dimension",
                rho_ProSoils = "ProSoils - rho",
                `p.value_ProSoils` = "ProSoils - p.value",
                `rho_Non-ProSoils` = "Non-ProSoils - rho",
                `p.value_Non-ProSoils` = "Non-ProSoils - p.value"
            )
        ) %>%
        flextable::colformat_double(digits = 3) %>%
        flextable::theme_alafoli() %>%
        flextable::bg(part = "all", bg = "white") %>% #set the background to white
        flextable::autofit()

    return(x)

}

mean_test <- function(indicator, title) {#just need to set the indicator, and a title for the table (usually the name of the indicator)

    data$indicator <- data[, indicator] #create new column for indicator variable

    x <- data.frame(table(data$pro_soils_group[!is.na(data$indicator)])) #create a table to get the Ns for the prosoils and non-prosoils group

    t.test(indicator ~ pro_soils_group, data = data) %>% #run the t-test
        broom::tidy() %>% #tidy the output into a dataframe
        dplyr::mutate(Indicator = title,
                      Group = "Overall") %>%
        dplyr::select(
            Indicator,
            Group,
            estimate1, #get non posoils mean
            estimate2, #get prosoils mean
            "Difference" = estimate,
            "t-value" = statistic,
            p.value
        ) %>%
        flextable::flextable() %>%
        flextable::set_header_labels(values = list(
            estimate1 = paste0("Non-ProSoils (N = ", x$Freq[1], ")"), #set the column name to Non prosoils (N = n)
            estimate2 = paste0("ProSoils (N = ", x$Freq[2], ")") #set the column name to prosoils (N = n)
        )) %>%
        flextable::colformat_double(j = 3:6, digits = 1) %>%
        flextable::colformat_double(j = 7, digits = 3) %>%
        flextable::theme_alafoli() %>%
        flextable::bg(part = "all", bg = "white") %>%
        flextable::autofit()
}

median_test <- function(indicator, title) { #just need to set indicator and the title

    data$indicator <- data[, indicator] #new column for the indicator

    x <- data.frame(table(data$pro_soils_group[!is.na(data$indicator)]))

    med_data <- data %>%
        dplyr::filter(!is.na(indicator)) #filter out any missing values

    med_t <- agricolae::Median.test(med_data$indicator,
                                    med_data$pro_soils_group,
                                    console = FALSE) #run the test of medians

    tibble::tibble(
        Indicator = title,
        Group = "Overall",
        w = med_t$medians[[1]][1], #get the non prosoils median
        z = med_t$medians[[1]][2], #get the prosoils median
        Difference = w - z,
        `Chisq` = med_t$statistics$Chisq,
        `Simulated p.value` = med_t$statistics$p.chisq
    ) %>%
        flextable::flextable() %>%
        flextable::set_header_labels(values = list(
            w = paste0("Non-ProSoils (N = ", x$Freq[1], ")"),
            z = paste0("ProSoils (N = ", x$Freq[2], ")")
        )) %>%
        flextable::theme_alafoli() %>%
        flextable::bg(part = "all", bg = "white") %>%
        flextable::colformat_double(j = 3:6, digits = 1) %>%
        flextable::colformat_double(j = 7, digits = 3) %>%
        flextable::autofit()

}

indicator_fisher_test <- function(indicator, title){ #set the indicator and a title for the indicator

    data$indicator <- data[,indicator]

    x <- tabyl(data, indicator, pro_soils_group, show_na = FALSE) %>% fisher.test() #run the fisher test (non-parametic chi-sq)

    tab <- janitor::tabyl(data, indicator, pro_soils_group, show_na = FALSE) %>% #create a frequency table
        janitor::adorn_percentages(denominator = "col") %>% #generate column percentages
        janitor::adorn_pct_formatting() %>% #format the percentages
        janitor::adorn_ns() %>% #set so % (n)
        flextable::flextable() %>% #format output
        flextable::set_header_labels(values = list(
            indicator = title,
            `0` = "Non-ProSoils",
            `1` = "ProSoils"
        )) %>%
        flextable::autofit() %>%
        flextable::theme_alafoli() %>%
        flextable::bg(part = "all", bg = "white") %>%
        flextable::add_footer_lines(values = paste0("Fisher test - p.value = ", round(x$p.value, 3))) #add in the test result as a footnote to the table

    return(tab)

}

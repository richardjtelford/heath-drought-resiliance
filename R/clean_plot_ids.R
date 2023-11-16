clean_plot_ids <- function(x, plot_list){
  x |> 
    mutate(plot = if_else(site == 'BUO' & year == '2016', #BUO had wrong plot numbers in 2016. Fix that
                               true = recode(plot,
                                             '10.1.' = '11.1.',
                                             '10.2.' = '10.1.',
                                             '10.3.' = '12.1.',
                                             '11.1.' = '10.3.',
                                             '11.2.' = '10.2.',
                                             '11.3.' = '11.2.',
                                             '12.1.' = '12.2.',
                                             '12.2.' = '11.3.',
                                             '12.3.' = '12.3.'
                               ), false = plot)) |>
    mutate(plot = if_else(site == 'ROS' & year == '2018', #ROS had wrong plot numbers in 2018. Fix that
                          true = recode(plot,
                                        '22.1.' = '22.3.',
                                        '22.3.' = '22.1.',
                                        '22.4.' = '22.6.',
                                        '22.5.' = '22.4.',
                                        '22.6.' = '22.7.',
                                        '22.7.' = '22.5.',
                                        '22.9.' = '22.10.',
                                        '22.0.' = '22.9.'
                          ), false = plot),
           plot = ifelse(site == 'ROS' & plot == '22.10', '22.0.', plot),
           plot = ifelse(site == 'SKO' & plot == '25.10', '25.0.', plot),
           plot = ifelse(site == 'TOR' & year == '2018' & plot == '14', '24.14.', plot),
           plot = ifelse(site == 'TOR' & year == '2018' & plot == '16', '24.16.', plot),
           plot = ifelse(site == 'ROS' & year == '2017' & plot == '22.9.', '22.0.', plot),
           plot = ifelse(site == 'ROS' & year == '2017' & plot == '22.0.', '22.9.', plot),
           plot = str_replace(plot, pattern = "$" , replacement =  "."),       # This fix the plot number issue on TOR
           plot = str_replace(plot, pattern = "\\.\\.", replacement = ".")) |> 
    mutate(plot = if_else(site == 'SKO' & !year == '2017' , #SKO were misplaced with one in three years
                          true = recode(plot,
                                        '25.2.' = '25.1.',
                                        '25.3.' = '25.2.',
                                        '25.4.' = '25.3.',
                                        '25.5.' = '25.4.',
                                        '25.6.' = '25.5.',
                                        '25.7.' = '25.6.',
                                        '25.8.' = '25.7.',
                                        '25.9.' = '25.8.',
                                        '25.10.' = '25.9.',
                                        '25.0.' = '25.9.',
                                        '25.1.' = '25.0.'
                          ),  false = plot)) |>
    # remove unused plots. Also drop some NA plots
    filter(!(site == "BER" & plot %in% paste0(11:15, "."))) |>
    #change one more code
    mutate(plot = if_else(site == "ROS" & plot == "22.10.", true = "22.0.", false = plot)) |> 
    semi_join(plot_list) |> 
    filter(!(site == "NOV" & plot == "20.0.")) # missing data from 2018, 2019
}
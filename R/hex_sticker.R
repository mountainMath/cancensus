#' Internal function to generate hex sticker
#' @keywords internal
generate_cancensus_sticker <- function() {
  census_data <- cancensus::get_census(dataset='CA16', regions=list(C="01"),
                                       vectors=c(minority_base="v_CA16_3954",minority="v_CA16_3957"),
                                       labels="short", geo_format='sf', level='CD') %>%
    dplyr::mutate(share=minority/minority_base) %>%
    dplyr::mutate(share_d=cut(share,breaks=c(-Inf,0.01,0.025,0.05,0.075,0.15,0.2,0.3,0.4,Inf))) %>%
    dplyr::mutate(share_d=cut(share,breaks=c(-Inf,quantile(share,seq(0.125,0.875,0.125)),Inf)))

  set.seed(123)
  colours=grDevices::colorRampPalette(colors = c("#aa2222","#ff0000"))(8) %>% sample
  colours <- c("#F20404",
               "#FF0000",
               "#C21818",
               "#E60909",
               "#B61D1D",
               "#CE1313",
               "#DA0E0E",
               "#AA2222")

 p <- ggplot2::ggplot(census_data %>% sf::st_transform(102002)) +
    ggplot2::geom_sf(size=0.1,ggplot2::aes(fill=share_d),colour="grey30") +
    ggplot2::theme_void() +
    #scale_fill_brewer(palette="Reds",guide=FALSE) +
    ggplot2::scale_fill_manual(values=colours,guide=FALSE) +
    hexSticker::theme_transparent() +
    ggplot2::coord_sf(datum=NA,clip = "off")

  hexSticker::sticker(p, package="cancensus", p_size=8, p_y = 1.45,
                      #s_x=1, s_y=1,
                      #s_width=2, s_height=2,
                      s_x=1, s_y=0.75,
                      s_width=2.5, s_height=2.5,
                      #h_color="#FF0000",
                      h_color="#FF0000",
                      h_fill="grey30",
                      p_color="white",
                      white_around_sticker = TRUE,
                      filename=here::here("images/cancensus-sticker.png"),
                      dpi=600)
}

#' @importFrom dplyr %>%

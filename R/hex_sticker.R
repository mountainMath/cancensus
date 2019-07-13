#' Internal function to generate hex sticker
#' @keywords internal
generate_cancensus_sticker <- function() {
  census_data <- get_census(dataset='CA16', regions=list(C="01"),
                            vectors=c(minority_base="v_CA16_3954",minority="v_CA16_3957"),
                            labels="short", geo_format='sf', level='CD') %>%
    dplyr::mutate(share=minority/minority_base) %>%
    dplyr::mutate(share_d=cut(share,breaks=c(-Inf,0.01,0.025,0.05,0.075,0.15,0.2,0.3,0.4,Inf))) %>%
    dplyr::mutate(share_d=cut(share,breaks=c(-Inf,quantile(share,seq(0.125,0.875,0.125)),Inf)))


  p <- ggplot2::ggplot(census_data %>% sf::st_transform(102002)) +
    ggplot2::geom_sf(ggplot2::aes(fill=share_d),size=0.01) +
    ggplot2::scale_fill_brewer(palette = "Reds",guide=FALSE) +
    ggplot2::theme_void() +
    hexSticker::theme_transparent() +
    ggplot2::coord_sf(datum=NA)


  hexSticker::sticker(p, package="cancensus",
                      p_size=8, p_y=1.5,
                      s_x=1, s_y=0.78, s_width=1.5, s_height=1.5,
                      h_color="#FF0000",
                      h_fill="grey40",
                      p_color="white",
                      filename=here::here("images/cancensus-sticker.png"))
}

#' @importFrom dplyr %>%

# create theme

light <- "white"
medium <- "#F0F0E9"
dark <- "#E0E0D4"
green <- "#00511D"
text <- "#555555"

create_theme(
  adminlte_color(
    dark
  ),
  adminlte_sidebar(
    #width = "400px",
    dark_hover_color = text,
    dark_color = text,
    light_bg = dark,
    dark_bg = dark,
    dark_submenu_hover_color = text	
    
  ),
  adminlte_global(
    content_bg = light,
    box_bg = light,
    info_box_bg = light),
  output_file = "www/mytheme.css"
)

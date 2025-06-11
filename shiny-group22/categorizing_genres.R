categorize_genres <- function(genres) {
  genre_mapping <- list(
    Rock = c("album rock", "classic rock", "garage rock", "blues rock", "folk-rock", "punk", "hard rock", "glam rock", "rock-and-roll", "new wave"),
    Pop = c("pop", "dance pop", "bubblegum pop", "synth-pop", "art pop", "indie pop"),
    Hip_Hop_Rap = c("hip hop", "rap", "alternative hip hop", "gangster rap"),
    Electronic_Dance = c("electronic", "edm", "electropop", "trance", "house", "techno", "dubstep"),
    Indie_Alternative = c("indie rock", "alternative pop", "alternative rock", "indie pop", "folk-pop"),
    Folk_Country = c("folk", "country", "bluegrass", "americana"),
    RnB_Soul = c("r&b", "soul", "funk", "motown"),
    Reggae_World = c("reggae", "latin", "caribbean", "world"),
    Jazz_Blues = c("jazz", "blues", "swing", "bebop"),
    Classical_Instrumental = c("classical", "instrumental")
  )
  
  categorized_genres <- rep(NA_character_, length(genres))
  
  for (i in seq_along(genres)) {
    for (category in names(genre_mapping)) {
      if (tolower(genres[i]) %in% genre_mapping[[category]]) {
        categorized_genres[i] <- category
        break
      }
    }
    if (is.na(categorized_genres[i])) {
      categorized_genres[i] <- "Other"
    }
  }
  
  return(categorized_genres)
}

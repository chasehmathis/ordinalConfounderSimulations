# calculate periodontal_disease


#periodontal definition
#The authors proposed a definition for mild periodontitis as 
#≥2 interproximal sites with AL ≥3 mm and ≥2 interproximal sites 
#with PD ≥4 mm (not on the same tooth) or one site with PD ≥5 mm. 
#The effect of the proposed definition on the total burden of periodontitis 
#was assessed in a convenience sample of 456 adults ≥35 years old and 
#compared with other previously reported definitions for similar 
#categories of periodontitis.,

calculate_periodontal_disease <- function(probing_depths){
  
mild_periodontal <- function(row){
  
  cond1 <- sum(row[loss_attatch_sites] >= 3, na.rm = T) >= 2
  cond2 <- sum(row[pd_interproximal_sites] >= 4, na.rm = T) >= 2
  cond3 <- sum(row[pd_interproximal_sites] >= 5, na.rm = T) >= 1
  return((cond1 & cond2) | cond3)
}
moderate_periodontal <- function(row){
  
  # Moderate periodontitis ‡2 interproximal sites with CAL 
  #‡4 mm(not on same tooth)or 
  # ‡2 interproximal sites with PD ‡5 mm(not on same tooth)
  teeth_names <- names(row)
  matches <- gregexpr("\\d{2}", teeth_names)
  teeth_nums <- regmatches(names(row), matches)
  df <- tibble(t(row[loss_attatch_sites] >= 4), unlist(teeth_nums[loss_attatch_sites]))
  colnames(df) <- c("LA", "ToothNum")
  x <- df |> 
    group_by(ToothNum) |> 
    summarise(x = sum(LA, na.rm = TRUE)) |> 
    pull(x)
  cond1 <- sum(x != 0) >= 2
  df <- tibble(t(row[pd_interproximal_sites] >= 5), unlist(teeth_nums[pd_interproximal_sites]))
  colnames(df) <- c("PD", "ToothNum")
  x <- df |> 
    group_by(ToothNum) |> 
    summarise(x = sum(PD, na.rm = TRUE)) |> 
    pull(x)
  cond2 <- sum(x != 0) >= 2
  return(cond1 | cond2)
}
severe_periodontal <- function(row){
  
  # ‡2 interproximal sites with CAL ‡6 
  # mm(not on same tooth)and ‡1 interproximal site with PD ‡5 mm
  teeth_names <- names(row)
  matches <- gregexpr("\\d{2}", teeth_names)
  teeth_nums <- regmatches(names(row), matches)
  df <- tibble(t(row[loss_attatch_sites] >= 6), unlist(teeth_nums[loss_attatch_sites]))
  colnames(df) <- c("LA", "ToothNum")
  x <- df |> 
    group_by(ToothNum) |> 
    summarise(x = sum(LA, na.rm = TRUE)) |> 
    pull(x)
  cond1 <- sum(x != 0) >= 2
  df <- tibble(t(row[pd_interproximal_sites] >= 5), unlist(teeth_nums[pd_interproximal_sites]))
  colnames(df) <- c("PD", "ToothNum")
  x <- df |> 
    group_by(ToothNum) |> 
    summarise(x = sum(PD, na.rm = TRUE)) |> 
    pull(x)
  cond2 <- sum(x != 0) >= 1
  return((cond1 & cond2))
}


# LA interproximal LAD -
attatchement_loss <- probing_depths[,loss_attatch_sites]
proximal_sites <- probing_depths[,pd_interproximal_sites]


mild_periodontal_vec <- logical(nrow(probing_depths))
moderate_periodontal_vec <- logical(nrow(probing_depths))
severe_periodontal_vec <- logical(nrow(probing_depths))
for (i in 1:nrow(probing_depths)) {
  #mild_periodontal_vec[i] <- mild_periodontal(probing_depths[i, ])
  moderate_periodontal_vec[i] <- moderate_periodontal(probing_depths[i, ])
  severe_periodontal_vec[i] <- severe_periodontal(probing_depths[i, ])
}
periodontal_disease <- ifelse(severe_periodontal_vec, 2, ifelse(moderate_periodontal_vec, 1, 0))
return(periodontal_disease)

}
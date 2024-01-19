library(plyr)
library(dplyr)
library(reshape2)

df_notes <- read.csv(file.choose())
df_notes$ppt_id <- paste(df_notes$facility_number, df_notes$pid, sep = "-")
dfn <- df_notes %>% select(ppt_id, reviewer_id, annotation_confident, nc_movement, nc_crying, nc_coughing, nc_coughing, nc_camera, nc_dark, nc_unfocused, nc_video, nc_other)

df_results <- read.csv(file.choose())
df_results$ppt_id <- paste(df_results$facility_number, df_results$pid, sep = "-")
dfr <- df_results %>% select(ppt_id, measurement_number, reviewer_id_1, rr_1, reviewer_id_2, rr_2, reviewer_id_3, rr_3)
dfr1 <- dfr %>% select(ppt_id, measurement_number, reviewer_id_1, rr_1)
dfr2 <- dfr %>% select(ppt_id, measurement_number, reviewer_id_2, rr_2)
dfr3 <- dfr %>% select(ppt_id, measurement_number, reviewer_id_3, rr_3)

jdf1 <- left_join(dfr, dfn, by = c("ppt_id", "reviewer_id_1" = "reviewer_id"))
jdf2 <- left_join(jdf1, dfn, by = c("ppt_id", "reviewer_id_2" = "reviewer_id"))
jdf2 <- jdf2 %>% mutate(
              # nc_awkward = case_when(is.na(nc_awkward.x) & is.na(nc_awkward.y) ~ NA,
              #                        is.na(nc_awkward.x) & !is.na(nc_awkward.y) ~ nc_awkward.y,
              #                        !is.na(nc_awkward.x) & is.na(nc_awkward.y) ~ nc_awkward.x,
              #                        nc_awkward.x==nc_awkward.y ~ nc_awkward.x,
              #                        nc_awkward.x!=nc_awkward.y ~ 1),
              nc_camera = case_when(is.na(nc_camera.x) & is.na(nc_camera.y) ~ NA,
                                    is.na(nc_camera.x) & !is.na(nc_camera.y) ~ nc_camera.y,
                                    !is.na(nc_camera.x) & is.na(nc_camera.y) ~ nc_camera.x,
                                    nc_camera.x==nc_camera.y ~ nc_camera.x,
                                    nc_camera.x!=nc_camera.y ~ 1),
              nc_coughing = case_when(is.na(nc_coughing.x) & is.na(nc_coughing.y) ~ NA,
                                      is.na(nc_coughing.x) & !is.na(nc_coughing.y) ~ nc_coughing.y,
                                      !is.na(nc_coughing.x) & is.na(nc_coughing.y) ~ nc_coughing.x,
                                      nc_coughing.x==nc_coughing.y ~ nc_coughing.x,
                                      nc_coughing.x!=nc_coughing.y ~ 1),
              nc_crying = case_when(is.na(nc_crying.x) & is.na(nc_crying.y) ~ NA,
                                    is.na(nc_crying.x) & !is.na(nc_crying.y) ~ nc_crying.y,
                                    !is.na(nc_crying.x) & is.na(nc_crying.y) ~ nc_crying.x,
                                    nc_crying.x==nc_crying.y ~ nc_crying.x,
                                    nc_crying.x!=nc_crying.y ~ 1),
              nc_dark = case_when(is.na(nc_dark.x) & is.na(nc_dark.y) ~ NA,
                                  is.na(nc_dark.x) & !is.na(nc_dark.y) ~ nc_dark.y,
                                  !is.na(nc_dark.x) & is.na(nc_dark.y) ~ nc_dark.x,
                                  nc_dark.x==nc_dark.y ~ nc_dark.x,
                                  nc_dark.x!=nc_dark.y ~ 1),
              nc_movement = case_when(is.na(nc_movement.x) & is.na(nc_movement.y) ~ NA,
                                      is.na(nc_movement.x) & !is.na(nc_movement.y) ~ nc_movement.y,
                                      !is.na(nc_movement.x) & is.na(nc_movement.y) ~ nc_movement.x,
                                      nc_movement.x==nc_movement.y ~ nc_movement.x,
                                      nc_movement.x!=nc_movement.y ~ 1),
              nc_video = case_when(is.na(nc_video.x) & is.na(nc_video.y) ~ NA,
                                   is.na(nc_video.x) & !is.na(nc_video.y) ~ nc_video.y,
                                   !is.na(nc_video.x) & is.na(nc_video.y) ~ nc_video.x,
                                   nc_video.x==nc_video.y ~ nc_video.x,
                                   nc_video.x!=nc_video.y ~ 1),
              nc_unfocused = case_when(is.na(nc_unfocused.x) & is.na(nc_unfocused.y) ~ NA,
                                       is.na(nc_unfocused.x) & !is.na(nc_unfocused.y) ~ nc_unfocused.y,
                                       !is.na(nc_unfocused.x) & is.na(nc_unfocused.y) ~ nc_unfocused.x,
                                       nc_unfocused.x==nc_unfocused.y ~ nc_unfocused.x,
                                       nc_unfocused.x!=nc_unfocused.y ~ 1),
              nc_other = case_when(is.na(nc_other.x) & is.na(nc_other.y) ~ NA,
                                   is.na(nc_other.x) & !is.na(nc_other.y) ~ nc_other.y,
                                   !is.na(nc_other.x) & is.na(nc_other.y) ~ nc_other.x,
                                   nc_other.x==nc_other.y ~ nc_other.x,
                                   nc_other.x!=nc_other.y ~ 1))
jdf2 <- jdf2 %>% select(c(1:9, 18, 27:34))
names(jdf2)[9] <- "annotation_confident_r1"
names(jdf2)[10] <- "annotation_confident_r2"

jdf <- left_join(jdf2, dfn, by = c("ppt_id", "reviewer_id_3" = "reviewer_id"))
jdf <- jdf %>% mutate(
  # nc_awkward = case_when(is.na(nc_awkward.x) & is.na(nc_awkward.y) ~ NA,
  #                        is.na(nc_awkward.x) & !is.na(nc_awkward.y) ~ nc_awkward.y,
  #                        !is.na(nc_awkward.x) & is.na(nc_awkward.y) ~ nc_awkward.x,
  #                        nc_awkward.x==nc_awkward.y ~ nc_awkward.x,
  #                        nc_awkward.x!=nc_awkward.y ~ 1),
  nc_camera = case_when(is.na(nc_camera.x) & is.na(nc_camera.y) ~ NA,
                        is.na(nc_camera.x) & !is.na(nc_camera.y) ~ nc_camera.y,
                        !is.na(nc_camera.x) & is.na(nc_camera.y) ~ nc_camera.x,
                        nc_camera.x==nc_camera.y ~ nc_camera.x,
                        nc_camera.x!=nc_camera.y ~ 1),
  nc_coughing = case_when(is.na(nc_coughing.x) & is.na(nc_coughing.y) ~ NA,
                          is.na(nc_coughing.x) & !is.na(nc_coughing.y) ~ nc_coughing.y,
                          !is.na(nc_coughing.x) & is.na(nc_coughing.y) ~ nc_coughing.x,
                          nc_coughing.x==nc_coughing.y ~ nc_coughing.x,
                          nc_coughing.x!=nc_coughing.y ~ 1),
  nc_crying = case_when(is.na(nc_crying.x) & is.na(nc_crying.y) ~ NA,
                        is.na(nc_crying.x) & !is.na(nc_crying.y) ~ nc_crying.y,
                        !is.na(nc_crying.x) & is.na(nc_crying.y) ~ nc_crying.x,
                        nc_crying.x==nc_crying.y ~ nc_crying.x,
                        nc_crying.x!=nc_crying.y ~ 1),
  nc_dark = case_when(is.na(nc_dark.x) & is.na(nc_dark.y) ~ NA,
                      is.na(nc_dark.x) & !is.na(nc_dark.y) ~ nc_dark.y,
                      !is.na(nc_dark.x) & is.na(nc_dark.y) ~ nc_dark.x,
                      nc_dark.x==nc_dark.y ~ nc_dark.x,
                      nc_dark.x!=nc_dark.y ~ 1),
  nc_movement = case_when(is.na(nc_movement.x) & is.na(nc_movement.y) ~ NA,
                          is.na(nc_movement.x) & !is.na(nc_movement.y) ~ nc_movement.y,
                          !is.na(nc_movement.x) & is.na(nc_movement.y) ~ nc_movement.x,
                          nc_movement.x==nc_movement.y ~ nc_movement.x,
                          nc_movement.x!=nc_movement.y ~ 1),
  nc_video = case_when(is.na(nc_video.x) & is.na(nc_video.y) ~ NA,
                       is.na(nc_video.x) & !is.na(nc_video.y) ~ nc_video.y,
                       !is.na(nc_video.x) & is.na(nc_video.y) ~ nc_video.x,
                       nc_video.x==nc_video.y ~ nc_video.x,
                       nc_video.x!=nc_video.y ~ 1),
  nc_unfocused = case_when(is.na(nc_unfocused.x) & is.na(nc_unfocused.y) ~ NA,
                           is.na(nc_unfocused.x) & !is.na(nc_unfocused.y) ~ nc_unfocused.y,
                           !is.na(nc_unfocused.x) & is.na(nc_unfocused.y) ~ nc_unfocused.x,
                           nc_unfocused.x==nc_unfocused.y ~ nc_unfocused.x,
                           nc_unfocused.x!=nc_unfocused.y ~ 1),
  nc_other = case_when(is.na(nc_other.x) & is.na(nc_other.y) ~ NA,
                       is.na(nc_other.x) & !is.na(nc_other.y) ~ nc_other.y,
                       !is.na(nc_other.x) & is.na(nc_other.y) ~ nc_other.x,
                       nc_other.x==nc_other.y ~ nc_other.x,
                       nc_other.x!=nc_other.y ~ 1))
jdf <- jdf %>% select(c(1:10, 19, 28:35))
names(jdf)[11] <- "annotation_confident_r3"
jdf <- jdf %>% select(-c(3, 5, 7))
names(jdf)[3] <- "annotatedrr_r1"
names(jdf)[4] <- "annotatedrr_r2"
names(jdf)[5] <- "annotatedrr_r3"

# Subset df's to facilitate variable renaming
jdf1 <- subset(jdf, measurement_number==1)
names(jdf1)[3:16] <- paste("m1_", names(jdf1)[3:16], sep = "")
jdf1 <- jdf1 %>% select(-2)

jdf2 <- subset(jdf, measurement_number==2)
names(jdf2)[3:16] <- paste("m2_", names(jdf2)[3:16], sep = "")
jdf2 <- jdf2 %>% select(-2)

jdf3 <- subset(jdf, measurement_number==3)
names(jdf3)[3:16] <- paste("m3_", names(jdf3)[3:16], sep = "")
jdf3 <- jdf3 %>% select(-2)

export_df <- left_join(jdf1, jdf2, by = "ppt_id")
export_df <- left_join(export_df, jdf3, by = "ppt_id")

# write to csv -- update file name
write.csv(export_df, "C:/Users/rgreen/Box/3_Output 3/Hybrid study/Diagnostic accuracy study/Analysis/tanzania_annotated_rr_import_2024-01-19.csv")

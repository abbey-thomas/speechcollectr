# Huggins Pitch
hp_names <- c("hp_practice","hp_1_1", "hp_2_2", "hp_3_3",
              "hp_4_1", "hp_5_2", "hp_6_3",
              "hp_7_1", "hp_8_2", "hp_9_3",
              "hp_10_1", "hp_11_2", "hp_12_3", "cal_noise")
HugginsPitchData <- list()
for (i in hp_names) {
  HugginsPitchData[[i]] <- tuneR::readWave(filename = paste0("www/", i,".wav"))
}

# Antiphase
ap_names <- c("ap_1_3", "ap_2_2", "ap_3_3",
              "ap_4_2", "ap_5_1", "ap_6_1",
              "cal_noise")
AntiphaseData <- list()
for (i in ap_names) {
  AntiphaseData[[i]] <- tuneR::readWave(filename = paste0("www/", i,".wav"))
}

# dest_icons
dest_icons <- c("tools","shield-alt","graduation-cap",
                "gamepad","music","hotel","plug",
                "mail-bulk","clinic-medical","paw",
                "tree","money-bill-alt","university",
                "balance-scale","car-side","microphone-alt",
                "mug-hot","fire-extinguisher","gas-pump",
                "palette","utensils","book-reader","gem",
                "shopping-cart","archive","ambulance","bell",
                "brush","camera","carrot","charging-station",
                "cloud","cogs","comment","concierge-bell",
                "couch","crown","egg","feather-alt",
                "envelope","dumbbell","city","bus-alt",
                "briefcase","binoculars","birthday-cake",
                "basketball-ball","baseball-ball",
                "baby-carriage","bath","archway",
                "fish","flag","gavel","gifts","globe",
                "hamburger","helicopter","mask",
                "microscope","key","ice-cream",
                "hiking","horse","leaf","map","moon",
                "mountain","paint-brush","pen","phone",
                "pizza-slice","plane","place-of-worship",
                "rocket","road","shuttle-van","school",
                "skiing","spa","store","table-tennis",
                "swimmer","subway","thumbtack",
                "ticket-alt","umbrella","thermometer-half",
                "tshirt","tractor","warehouse",
                "volleyball-ball","video","wine-bottle",
                "trophy","traffic-light","taxi",
                "sign","receipt", "anchor")

#usethis::use_data(AntiphaseData, HugginsPitchData, dest_icons, internal = TRUE, overwrite = TRUE)

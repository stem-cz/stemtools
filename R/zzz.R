if (rstudioapi::isAvailable()) {
    device <- "ragg"
    rstudioapi::writeRStudioPreference("graphics_backend", device)
    packageStartupMessage((paste("Rstudio now uses", device, "as graphical backend.")))
  }

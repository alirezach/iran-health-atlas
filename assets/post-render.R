# Quarto post-render hook: send the site root to the dashboard and disable
# Jekyll on GitHub Pages.
out <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR", "_site")
if (!dir.exists(out)) out <- "_site"
writeLines(c(
  "<!DOCTYPE html>",
  "<html lang=\"en\"><head><meta charset=\"utf-8\">",
  "<title>Iran Health Accessibility Atlas</title>",
  "<meta http-equiv=\"refresh\" content=\"0; url=dashboard/\">",
  "<link rel=\"canonical\" href=\"dashboard/\">",
  "</head><body><p><a href=\"dashboard/\">Open the Iran Health Accessibility Atlas</a></p></body></html>"
), file.path(out, "index.html"))
file.create(file.path(out, ".nojekyll"))


#################################################    
###   CONFIGURE RTOOLS PATH FOR RSTUDIO      #####
#################################################

# Configure Rtools path for RStudio
# written by Annie Kellner 
# Date: 2025-09-25

# THIS SCRIPT CONFIGURES RTOOLS PATH FOR RSTUDIO
# Run this script if you're having issues with package compilation
# or if RStudio can't find Rtools

# Method 1: Set environment variable (recommended)
# Replace the path with your actual Rtools installation path
Sys.setenv(RTOOLS43_HOME = "C:/Users/akellner/AppData/Local/R/rtools45")  # For Rtools 4.3
# Sys.setenv(RTOOLS42_HOME = "C:/rtools42")  # For Rtools 4.2 (uncomment if using 4.2)

# Method 2: Check if Rtools is properly configured
# Run this to verify Rtools is found:
if (require(devtools, quietly = TRUE)) {
  if (find_rtools()) {
    cat("Rtools found and properly configured!\n")
  } else {
    cat("Rtools not found. Please check your installation path.\n")
    cat("Common paths:\n")
    cat("- C:/rtools43 (for Rtools 4.3)\n")
    cat("- C:/rtools42 (for Rtools 4.2)\n")
    cat("- C:/Rtools (older versions)\n")
  }
} else {
  cat("devtools package not installed. Installing now...\n")
  install.packages("devtools")
}

# Method 3: Alternative - check system PATH
# This shows what's in your system PATH
cat("Current PATH entries containing 'rtools':\n")
path_entries <- strsplit(Sys.getenv("PATH"), ";")[[1]]
rtools_paths <- path_entries[grepl("rtools", path_entries, ignore.case = TRUE)]
if (length(rtools_paths) > 0) {
  cat(paste(rtools_paths, collapse = "\n"))
} else {
  cat("No Rtools paths found in system PATH\n")
}

# Method 4: Check for common Rtools installations
common_paths <- c(
  "C:/rtools43",
  "C:/rtools42", 
  "C:/Rtools",
  "C:/rtools"
)

cat("\nChecking for common Rtools installation paths:\n")
for (path in common_paths) {
  if (dir.exists(path)) {
    cat(paste("✓ Found:", path, "\n"))
  } else {
    cat(paste("✗ Not found:", path, "\n"))
  }
}

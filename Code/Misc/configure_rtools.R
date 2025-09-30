#################################################    
###   CONFIGURE RTOOLS PATH FOR RSTUDIO      #####
#################################################

# Configure Rtools path for RStudio
# written by Annie Kellner 
# Date: $(date)

# THIS SCRIPT CONFIGURES RTOOLS PATH FOR RSTUDIO
# Run this script if you're having issues with package compilation
# or if RStudio can't find Rtools

# Method 1: Set environment variable (recommended)
# Replace the path with your actual Rtools installation path
Sys.setenv(RTOOLS43_HOME = "C:/rtools43")  # For Rtools 4.3
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

# Method 5: Fix the specific issue with RStudio not recognizing compilation capability
cat("\n=== FIXING RSTUDIO COMPILATION ISSUE ===\n")

# Check if we have the necessary compilation tools
cat("Checking for compilation tools:\n")
tools_to_check <- c("gcc", "g++", "make")
for (tool in tools_to_check) {
  tool_path <- Sys.which(tool)
  if (tool_path != "") {
    cat(paste("✓", tool, "found at:", tool_path, "\n"))
  } else {
    cat(paste("✗", tool, "not found in PATH\n"))
  }
}

# Try to set up the proper environment for RStudio
cat("\nSetting up compilation environment...\n")

# Get the Rtools path that was found
rtools_path <- NULL
for (path in common_paths) {
  if (dir.exists(path)) {
    rtools_path <- path
    break
  }
}

if (!is.null(rtools_path)) {
  # Set the Rtools path
  Sys.setenv(RTOOLS43_HOME = rtools_path)
  
  # Add Rtools bin directories to PATH
  rtools_bin <- file.path(rtools_path, "usr", "bin")
  rtools_mingw_bin <- file.path(rtools_path, "mingw64", "bin")
  
  if (dir.exists(rtools_bin)) {
    current_path <- Sys.getenv("PATH")
    if (!grepl(rtools_bin, current_path, fixed = TRUE)) {
      Sys.setenv(PATH = paste(current_path, rtools_bin, sep = ";"))
      cat(paste("Added to PATH:", rtools_bin, "\n"))
    }
  }
  
  if (dir.exists(rtools_mingw_bin)) {
    current_path <- Sys.getenv("PATH")
    if (!grepl(rtools_mingw_bin, current_path, fixed = TRUE)) {
      Sys.setenv(PATH = paste(current_path, rtools_mingw_bin, sep = ";"))
      cat(paste("Added to PATH:", rtools_mingw_bin, "\n"))
    }
  }
  
  # Test compilation capability
  cat("\nTesting compilation capability...\n")
  if (require(devtools, quietly = TRUE)) {
    if (find_rtools()) {
      cat("✓ Rtools properly configured for compilation\n")
      
      # Try to install a simple package that requires compilation
      cat("Testing with Rttf2pt1 installation...\n")
      
      # Set install options to force compilation
      options(install.packages.compile.from.source = "always")
      
      cat("You can now try installing Rttf2pt1 with:\n")
      cat("install.packages('Rttf2pt1', type = 'source')\n")
      
    } else {
      cat("✗ Rtools still not properly configured\n")
    }
  }
} else {
  cat("✗ No Rtools installation found. Please install Rtools from:\n")
  cat("https://cran.rstudio.com/bin/windows/Rtools/\n")
}

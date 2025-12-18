# Master Build Script for Zwemsterdam
# Use this for local development/builds

print("Step 1: Running the data collection pipeline...")
source("get.R")

print("Step 2: Building the frontend...")
system("cd frontend && npm run build")

print("Step 3: Build complete!")
print("The static site is now available in 'frontend/dist/'")
print("For development, run: cd frontend && npm run dev")


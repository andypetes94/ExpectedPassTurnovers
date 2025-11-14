#!/bin/bash

# run_turnover_model.sh
# Script to run turnover model cross-validation and save outputs

# Set script parameters
R_SCRIPT="turnover_cv_analysis.R"
DATA_FILE="$1"  # Pass data file as first argument
OUTPUT_DIR="turnover_model_results_$(date +%Y%m%d_%H%M%S)"

# Create output directory
echo "Creating output directory: $OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Check if data file exists
if [ ! -f "$DATA_FILE" ]; then
    echo "Error: Data file '$DATA_FILE' not found!"
    exit 1
fi

echo "Starting turnover model analysis..."
echo "Data file: $DATA_FILE"
echo "Output directory: $OUTPUT_DIR"

# Run R script
Rscript "$R_SCRIPT" "$DATA_FILE" "$OUTPUT_DIR"

# Check if R script completed successfully
if [ $? -eq 0 ]; then
    echo "Analysis completed successfully!"
    echo "Results saved in: $OUTPUT_DIR"
    echo ""
    echo "Output files:"
    ls -la "$OUTPUT_DIR"
else
    echo "Error: R script failed!"
    exit 1
fi
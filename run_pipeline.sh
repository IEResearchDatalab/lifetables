#!/bin/bash
################################################################################
# Master Pipeline Script
# 
# Runs the full two-stage pipeline with logging and error handling
#
# Usage:
#   ./run_pipeline.sh [options]
#
# Options:
#   --test          Run in test mode (3 cities only)
#   --parallel N    Number of parallel workers (default: 8)
#   --detailed      Save detailed outputs
#   --resume        Resume from previous run
#   --stage1-only   Run only stage 1
#   --stage2-only   Run only stage 2
################################################################################

set -e  # Exit on error

# Default parameters
PARALLEL=8
TEST_MODE=false
DETAILED=false
RESUME=false
STAGE1_ONLY=false
STAGE2_ONLY=false

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --test)
      TEST_MODE=true
      shift
      ;;
    --parallel)
      PARALLEL="$2"
      shift 2
      ;;
    --detailed)
      DETAILED=true
      shift
      ;;
    --resume)
      RESUME=true
      shift
      ;;
    --stage1-only)
      STAGE1_ONLY=true
      shift
      ;;
    --stage2-only)
      STAGE2_ONLY=true
      shift
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

# Create log directory
mkdir -p logs

# Generate timestamp for log files
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOGFILE="logs/pipeline_${TIMESTAMP}.log"

# Function to log messages
log() {
  echo "[$(date +'%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOGFILE"
}

log "========================================="
log "Simon Project: Pipeline Execution"
log "========================================="
log ""
log "Configuration:"
log "  Parallel workers: $PARALLEL"
log "  Test mode: $TEST_MODE"
log "  Detailed outputs: $DETAILED"
log "  Resume mode: $RESUME"
log "  Log file: $LOGFILE"
log ""

# Build command-line arguments
STAGE1_ARGS="--parallel $PARALLEL"
STAGE2_ARGS="--parallel $PARALLEL"

if [ "$TEST_MODE" = true ]; then
  STAGE1_ARGS="$STAGE1_ARGS --test"
  STAGE2_ARGS="$STAGE2_ARGS --test"
fi

if [ "$RESUME" = true ]; then
  STAGE1_ARGS="$STAGE1_ARGS --resume"
  STAGE2_ARGS="$STAGE2_ARGS --resume"
fi

if [ "$DETAILED" = true ]; then
  STAGE2_ARGS="$STAGE2_ARGS --detailed"
fi

# ============================================================================
# STAGE 1: Precompute Cities
# ============================================================================

if [ "$STAGE2_ONLY" = false ]; then
  log "========================================="
  log "STAGE 1: Precomputing Cities"
  log "========================================="
  log ""
  
  STAGE1_START=$(date +%s)
  
  if Rscript 01_precompute_cities.R $STAGE1_ARGS 2>&1 | tee -a "$LOGFILE"; then
    STAGE1_END=$(date +%s)
    STAGE1_DURATION=$((STAGE1_END - STAGE1_START))
    log ""
    log "Stage 1 completed successfully in $((STAGE1_DURATION / 60)) minutes"
  else
    log ""
    log "ERROR: Stage 1 failed"
    exit 1
  fi
  
  log ""
fi

# ============================================================================
# STAGE 2: Compute Scenarios
# ============================================================================

if [ "$STAGE1_ONLY" = false ]; then
  log "========================================="
  log "STAGE 2: Computing Scenarios"
  log "========================================="
  log ""
  
  STAGE2_START=$(date +%s)
  
  if Rscript 02_compute_scenarios.R $STAGE2_ARGS 2>&1 | tee -a "$LOGFILE"; then
    STAGE2_END=$(date +%s)
    STAGE2_DURATION=$((STAGE2_END - STAGE2_START))
    log ""
    log "Stage 2 completed successfully in $((STAGE2_DURATION / 60)) minutes"
  else
    log ""
    log "ERROR: Stage 2 failed"
    exit 1
  fi
  
  log ""
fi

# ============================================================================
# Final Summary
# ============================================================================

log "========================================="
log "Pipeline Complete"
log "========================================="
log ""

if [ "$STAGE1_ONLY" = false ] && [ "$STAGE2_ONLY" = false ]; then
  TOTAL_DURATION=$((STAGE1_DURATION + STAGE2_DURATION))
  log "Total runtime: $((TOTAL_DURATION / 60)) minutes ($((TOTAL_DURATION / 3600)) hours)"
  log ""
fi

log "Output files:"
if [ "$STAGE2_ONLY" = false ]; then
  log "  Precomputed cities: precomputed/cities/"
  log "  Stage 1 progress: logs/stage1_progress.csv"
fi

if [ "$STAGE1_ONLY" = false ]; then
  log "  Results summary: results/lifespan_inequality_summary.csv"
  log "  Task manifest: logs/task_manifest.csv"
  if [ "$DETAILED" = true ]; then
    log "  Detailed outputs: results/detailed/"
  fi
fi

log ""
log "Log file: $LOGFILE"
log ""
log "========================================="

# Display result summary if available
if [ -f "results/lifespan_inequality_summary.csv" ] && [ "$STAGE1_ONLY" = false ]; then
  log ""
  log "Results preview:"
  head -20 results/lifespan_inequality_summary.csv | column -t -s, | tee -a "$LOGFILE"
fi

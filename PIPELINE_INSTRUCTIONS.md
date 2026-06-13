# Simon Project: Pipeline Execution Guide

## 📋 Overview

This pipeline computes lifespan inequality impacts for **854 European cities** across **2 SSP scenarios** and **11 adaptation levels** (18,788 total runs).

**Estimated runtime:** 15 hours on 8-core server

---

## 🗂️ File Structure

```
simon_project/
├── data/
│   ├── tmeanproj.gz.parquet          # Temperature projections
│   ├── coefs.csv                      # ERF coefficients
│   └── mortality_projections/         # Baseline mortality (optional)
├── R/
│   ├── load_coefficients.R
│   └── load_data.R
├── precomputed/                       # Created by Stage 1
│   └── cities/                        # City-level RDS files
├── results/                           # Created by Stage 2
│   ├── lifespan_inequality_summary.csv  # MAIN OUTPUT
│   └── detailed/                      # Optional detailed outputs
├── logs/                              # Execution logs
│   ├── pipeline_*.log
│   ├── stage1_progress.csv
│   └── task_manifest.csv
├── pipeline_functions.R               # Modular functions
├── 01_precompute_cities.R            # Stage 1 script
├── 02_compute_scenarios.R            # Stage 2 script
└── run_pipeline.sh                   # Master orchestration
```

---

## 🚀 Quick Start

### 1. Make scripts executable

```bash
chmod +x run_pipeline.sh
chmod +x 01_precompute_cities.R
chmod +x 02_compute_scenarios.R
```

### 2. Test with 3 cities (30 minutes)

```bash
./run_pipeline.sh --test --parallel 4
```

This runs:
- 3 cities × 2 SSPs × 11 adaptations = **66 scenarios**
- Validates the full pipeline
- Outputs to `results/lifespan_inequality_summary.csv`

### 3. Run full pipeline (15 hours)

```bash
nohup ./run_pipeline.sh --parallel 8 > logs/pipeline.out 2>&1 &
```

**Explanation:**
- `nohup`: Keeps running after logout
- `--parallel 8`: Uses 8 CPU cores
- `> logs/pipeline.out 2>&1`: Redirects all output to log file
- `&`: Runs in background

### 4. Monitor progress

```bash
# Watch log file in real-time
tail -f logs/pipeline_*.log

# Check how many cities completed (Stage 1)
wc -l logs/stage1_progress.csv

# Check how many scenarios completed (Stage 2)
wc -l results/lifespan_inequality_summary.csv

# View progress summary
tail -20 logs/pipeline_*.log
```

---

## 🛠️ Advanced Usage

### Resume Interrupted Run

If the pipeline crashes or is interrupted:

```bash
nohup ./run_pipeline.sh --parallel 8 --resume > logs/pipeline_resume.out 2>&1 &
```

This **skips already completed** cities/scenarios.

---

### Run Only Stage 1 (Precompute Cities)

Useful if you want to precompute all cities first, then run scenarios later:

```bash
nohup ./run_pipeline.sh --parallel 8 --stage1-only > logs/stage1.out 2>&1 &
```

Later, run Stage 2:

```bash
nohup ./run_pipeline.sh --parallel 8 --stage2-only > logs/stage2.out 2>&1 &
```

---

### Save Detailed Outputs (For Subset of Cities)

**Warning:** This creates ~100 GB of files for all cities.

```bash
# Save detailed outputs for ALL cities (not recommended)
./run_pipeline.sh --parallel 8 --detailed

# Better: Save detailed for specific cities
Rscript 02_compute_scenarios.R --parallel 8 --detailed --cities "AT001C,ES001C,FR001C"
```

Detailed outputs include:
- Full life tables by age
- Age×year mortality multipliers
- Age-specific excess deaths

---

### Parallel Workers

Adjust based on your server:

```bash
# 4 cores (for 16 GB RAM server)
./run_pipeline.sh --parallel 4

# 16 cores (for 64 GB RAM server)
./run_pipeline.sh --parallel 16
```

**Memory requirements:**
- **Stage 1**: ~800 MB per worker → 8 workers = 6.4 GB
- **Stage 2**: ~160 MB per worker → 8 workers = 1.3 GB

**Recommended:** 16 GB RAM minimum, 32 GB comfortable

---

## 📊 Output Files

### Main Output: `results/lifespan_inequality_summary.csv`

One row per scenario (18,788 rows), columns:

```
city_code, ssp, adaptation,
e20_base, e20_climate, delta_e20, pct_delta_e20,
gini_base, gini_climate, delta_gini, pct_change_gini,
sd_base, sd_climate, delta_sd, pct_change_sd,
iqr_base, iqr_climate, delta_iqr,
threshold_age,
total_excess_deaths, excess_below_threshold, excess_above_threshold,
pct_below, pct_above,
runtime_sec
```

**File size:** ~4 MB (manageable for Excel/R/Python)

### Progress Logs

- **`logs/stage1_progress.csv`**: Status of each city precomputation
- **`logs/task_manifest.csv`**: Status of each scenario (18,788 rows)
- **`logs/pipeline_*.log`**: Full execution log with timestamps

---

## 🔍 Monitoring & Debugging

### Check Current Progress

```bash
# How many cities precomputed?
ls -1 precomputed/cities/*.rds | wc -l

# How many scenarios completed?
tail -1 results/lifespan_inequality_summary.csv | wc -l

# View recent completions
tail -20 logs/pipeline_*.log
```

### Check for Errors

```bash
# Search for errors in log
grep -i "error" logs/pipeline_*.log

# Find failed cities (Stage 1)
grep "failed" logs/stage1_progress.csv

# Find failed scenarios (Stage 2)
grep "ERROR" logs/pipeline_*.log
```

### View Resource Usage

```bash
# CPU usage
top -u $USER

# Memory usage
free -h

# Disk space
df -h
```

---

## 🛑 Stop Running Pipeline

```bash
# Find process ID
ps aux | grep run_pipeline.sh

# Kill process (replace PID)
kill <PID>

# Or kill all R processes (use with caution!)
pkill -9 Rscript
```

Resume later with `--resume` flag.

---

## 📈 Expected Timeline

With 8 cores:

| Stage | Tasks | Time |
|-------|-------|------|
| **Stage 1** | 854 cities | **5-6 hours** |
| **Stage 2** | 18,788 scenarios | **8-10 hours** |
| **Total** | | **~15 hours** |

With 16 cores: **~8 hours total**

---

## 🧪 Testing Strategy

### Phase 1: Single City (10 min)

```bash
Rscript 02_compute_scenarios.R --cities "AT001C" --parallel 2
```

Runs 1 city × 2 SSPs × 11 adaptations = 22 scenarios

### Phase 2: Small Batch (2 hours)

```bash
./run_pipeline.sh --test --parallel 4
```

Runs 3 cities × 2 SSPs × 11 adaptations = 66 scenarios

### Phase 3: Full Production (15 hours)

```bash
nohup ./run_pipeline.sh --parallel 8 > logs/production.out 2>&1 &
```

---

## 🔧 Troubleshooting

### "No precomputed cities found"

**Problem:** Stage 2 can't find precomputed data  
**Solution:** Run Stage 1 first:

```bash
./run_pipeline.sh --stage1-only
```

### "Out of memory" error

**Problem:** Too many parallel workers  
**Solution:** Reduce workers:

```bash
./run_pipeline.sh --parallel 4
```

### Pipeline hangs/freezes

**Problem:** Possible deadlock in parallel processing  
**Solution:** 
1. Kill process: `pkill Rscript`
2. Resume: `./run_pipeline.sh --resume`

### Missing mortality data

**Problem:** `mortality_projections/XX_mortality.csv` not found  
**Effect:** Uses synthetic Gompertz model (fallback)  
**Solution:** Either:
- Accept synthetic data (reasonable for demo)
- Download Eurostat EUROPOP2023 data

---

## 💾 Disk Space Requirements

- **Precomputed cities:** ~15 GB (854 × ~18 MB each)
- **Results summary:** ~4 MB
- **Detailed outputs** (optional): ~100 GB
- **Logs:** ~50 MB
- **Total:** ~20 GB (without detailed outputs)

---

## 🎯 Success Criteria

Pipeline is successful if:

1. ✅ `logs/stage1_progress.csv` shows 854 cities completed
2. ✅ `results/lifespan_inequality_summary.csv` has 18,788 rows
3. ✅ No critical errors in `logs/pipeline_*.log`
4. ✅ File size ~4 MB for summary CSV

---

## 📧 Support

If issues arise:
1. Check `logs/pipeline_*.log` for error messages
2. Check `logs/stage1_progress.csv` and `logs/task_manifest.csv` for failed tasks
3. Try `--resume` to skip completed work
4. Try `--test` mode to validate setup

---

## 🔄 Re-running Pipeline

To start fresh:

```bash
# Remove all outputs
rm -rf precomputed/ results/ logs/

# Run from scratch
./run_pipeline.sh --parallel 8
```

To update specific cities:

```bash
# Remove specific city's precomputed data
rm precomputed/cities/AT001C.rds

# Re-run with resume (will recompute only AT001C)
./run_pipeline.sh --resume
```

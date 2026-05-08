# GitHub large-file handling

GitHub rejects regular Git blobs larger than 100 MiB. This project therefore writes large model-result objects as compressed chunk directories by default (`CONFIG$save_single_rds = FALSE`). The single `.rds` files under `output/intermediate/` are ignored by `.gitignore`.

Chunked model objects use this naming convention:

```text
output/intermediate/<object-name>_rds_chunks/
  manifest.json
  part-001.rdsbin
  part-002.rdsbin
  ...
```

Each `part-*.rdsbin` is capped by `CONFIG$rds_chunk_size_mib` (default: 45 MiB), comfortably below GitHub's 100 MiB hard limit.

If an oversized `.rds` was already committed before this patch, `.gitignore` will not remove it from history. One safe repair path is:

```bash
git rm --cached output/intermediate/event_study_results_all_specs_aa2021_rep_margin_m5.rds
git commit -m "Stop tracking oversized model RDS"
git push
```

If GitHub still rejects the push because the large file exists in earlier local commits, remove it from history with `git filter-repo` or BFG, then force-push the cleaned branch. Alternatively, track large `.rds` files with Git LFS instead of chunking.

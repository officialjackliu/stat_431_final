#!/usr/bin/env python3
"""Round selected model-comparison CSVs to 5 significant figures in place.

Also preserves prior-sensitivity helpers:
- rounds prior_sensitivity_posterior_compare.csv
- rounds prior_sensitivity_test_metrics.csv and adds diff columns if missing
"""

from __future__ import annotations

import csv
import math
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
PRIOR_SENSITIVITY_DIR = ROOT / "results" / "prior_sensitivity"
MODEL_COMPARISON_DIR = ROOT / "results" / "model_comparison"
PREDICTION_DIR = ROOT / "results" / "prediction"
MCMC_DIR = ROOT / "results" / "mcmc"
POSTERIOR_CSV = PRIOR_SENSITIVITY_DIR / "prior_sensitivity_posterior_compare.csv"
TEST_CSV = PRIOR_SENSITIVITY_DIR / "prior_sensitivity_test_metrics.csv"
ROUND_ONLY_FILES = [
    MODEL_COMPARISON_DIR / "model_comparison_dic.csv",
    MODEL_COMPARISON_DIR / "model_comparison_loo_compare.csv",
    MODEL_COMPARISON_DIR / "model_comparison_loo.csv",
    MODEL_COMPARISON_DIR / "model_comparison_waic.csv",
    PREDICTION_DIR / "model_prediction_comparison.csv",
    MCMC_DIR / "mcmc_mcse_summary.csv",
]


def round_sig(x: float, n: int = 5) -> float:
    if x == 0 or not math.isfinite(x):
        return float(x)
    sign = -1.0 if x < 0 else 1.0
    ax = abs(x)
    m = math.floor(math.log10(ax))
    decimals = int(n - 1 - m)
    return sign * round(ax, decimals)


def maybe_float(value: str) -> float | None:
    try:
        # Treat empty cells as non-numeric
        if value is None or value == "":
            return None
        return float(value)
    except (TypeError, ValueError):
        return None


def round_numeric_fields_in_csv(path: Path, n_sig: int = 5) -> None:
    with open(path, newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))
        fieldnames = list(rows[0].keys()) if rows else []

    rounded_rows: list[dict[str, object]] = []
    for row in rows:
        out: dict[str, object] = {}
        for key, raw in row.items():
            as_num = maybe_float(raw)
            out[key] = round_sig(as_num, n_sig) if as_num is not None else raw
        rounded_rows.append(out)

    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fieldnames)
        w.writeheader()
        w.writerows(rounded_rows)


def main() -> None:
    # --- posterior compare ---
    with open(POSTERIOR_CSV, newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))

    out_post = []
    for r in rows:
        b = float(r["baseline_mean"])
        a = float(r["alt_mean"])
        d = float(r["diff"])
        out_post.append(
            {
                "parameter": r["parameter"],
                "baseline_mean": round_sig(b, 5),
                "alt_mean": round_sig(a, 5),
                "diff": round_sig(d, 5),
            }
        )

    with open(POSTERIOR_CSV, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(
            f, fieldnames=["parameter", "baseline_mean", "alt_mean", "diff"]
        )
        w.writeheader()
        w.writerows(out_post)

    # --- test metrics: round + diff columns ---
    with open(TEST_CSV, newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))

    if len(rows) != 2:
        raise SystemExit(f"Expected 2 rows in {TEST_CSV}, got {len(rows)}")

    baseline = rows[0]
    alt = rows[1]
    rmse_b = float(baseline["test_RMSE"])
    rmse_a = float(alt["test_RMSE"])
    mae_b = float(baseline["test_MAE"])
    mae_a = float(alt["test_MAE"])

    d_rmse = rmse_a - rmse_b
    d_mae = mae_a - mae_b

    out_test = [
        {
            "prior": baseline["prior"],
            "test_RMSE": round_sig(rmse_b, 5),
            "test_MAE": round_sig(mae_b, 5),
            "diff_RMSE": "",
            "diff_MAE": "",
        },
        {
            "prior": alt["prior"],
            "test_RMSE": round_sig(rmse_a, 5),
            "test_MAE": round_sig(mae_a, 5),
            "diff_RMSE": round_sig(d_rmse, 5),
            "diff_MAE": round_sig(d_mae, 5),
        },
    ]

    with open(TEST_CSV, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(
            f,
            fieldnames=["prior", "test_RMSE", "test_MAE", "diff_RMSE", "diff_MAE"],
        )
        w.writeheader()
        w.writerows(out_test)

    print("Updated:", POSTERIOR_CSV)
    print("Updated:", TEST_CSV)

    for csv_path in ROUND_ONLY_FILES:
        if csv_path.exists():
            round_numeric_fields_in_csv(csv_path, n_sig=5)
            print("Updated:", csv_path)
        else:
            print("Skipped (not found):", csv_path)


if __name__ == "__main__":
    main()

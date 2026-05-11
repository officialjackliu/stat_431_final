#!/usr/bin/env python3
"""Render CSV tables as PNG/JPG images (matplotlib).

Usage:
  python export_table_image.py                         # individual + combined defaults
  python export_table_image.py path/to/a.csv           # individual only
  python export_table_image.py --combined-only       # merged score panel only
"""

from __future__ import annotations

import argparse
import csv
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MODEL_COMPARISON_DIR = ROOT / "results" / "model_comparison"

DEFAULT_CSVS = [
    MODEL_COMPARISON_DIR / "model_comparison_dic.csv",
    MODEL_COMPARISON_DIR / "model_comparison_waic.csv",
    MODEL_COMPARISON_DIR / "model_comparison_loo.csv",
    MODEL_COMPARISON_DIR / "model_comparison_loo_compare.csv",
]

COMBINED_STEM = "model_comparison_scores_combined"


def read_csv_table(csv_path: Path) -> tuple[list[str], list[list[str]]]:
    with open(csv_path, newline="", encoding="utf-8") as f:
        rows = list(csv.reader(f))
    if not rows:
        raise ValueError(f"Empty CSV: {csv_path}")
    return rows[0], rows[1:]


def _style_table(table, ncols: int, *, compact: bool = False) -> None:
    table.auto_set_font_size(False)
    table.set_fontsize(7 if compact and ncols > 6 else (8 if ncols > 6 else 9))
    if compact:
        table.scale(1.05, 1.25)
    else:
        table.scale(1.1, 1.6)
    for (row, col), cell in table.get_celld().items():
        if row == 0:
            cell.set_facecolor("#4472C4")
            cell.get_text().set_color("white")
            cell.get_text().set_weight("bold")
        else:
            cell.set_facecolor("#F2F2F2" if row % 2 == 0 else "white")


def render_table_on_axes(
    ax,
    header: list[str],
    data: list[list[str]],
    title: str,
    *,
    compact: bool = False,
) -> None:
    ax.axis("off")
    ax.set_title(
        title,
        fontsize=10 if compact else 11,
        pad=3 if compact else 8,
        loc="left",
        fontweight="bold",
    )
    table = ax.table(
        cellText=data,
        colLabels=header,
        loc="center",
        cellLoc="center",
    )
    _style_table(table, len(header), compact=compact)


def export_one(csv_path: Path, *, dpi: int = 200) -> tuple[Path, Path]:
    import matplotlib.pyplot as plt

    csv_path = csv_path.resolve()
    if not csv_path.exists():
        raise FileNotFoundError(csv_path)

    header, data = read_csv_table(csv_path)
    ncols = len(header)
    nrows = len(data)

    title = csv_path.stem.replace("_", " ").replace("-", " ").title()
    fig_w = max(8.0, min(14.0, 1.2 * ncols + 4))
    fig_h = max(2.5, 0.38 * (nrows + 1) + 1.0)

    fig, ax = plt.subplots(figsize=(fig_w, fig_h))
    render_table_on_axes(ax, header, data, title)

    plt.tight_layout()
    out_png = csv_path.with_suffix(".png")
    out_jpg = csv_path.with_suffix(".jpg")
    fig.savefig(out_png, dpi=dpi, bbox_inches="tight", facecolor="white")
    fig.savefig(out_jpg, dpi=dpi, bbox_inches="tight", facecolor="white", pil_kwargs={"quality": 95})
    plt.close()

    return out_png, out_jpg


def export_combined(
    csv_paths: list[Path],
    *,
    dpi: int = 200,
    out_stem: str = COMBINED_STEM,
) -> tuple[Path, Path]:
    import matplotlib.pyplot as plt

    panels: list[tuple[str, list[str], list[list[str]]]] = []
    for path in csv_paths:
        path = path.resolve()
        if not path.exists():
            raise FileNotFoundError(path)
        header, data = read_csv_table(path)
        subtitle = path.stem.replace("model_comparison_", "").replace("_", " ").upper()
        panels.append((subtitle, header, data))

    # One column, top to bottom: DIC, WAIC, LOO, LOO COMPARE
    height_ratios = [len(data) + 1.35 for _, _, data in panels]
    fig_h = sum(height_ratios) * 0.42 + 0.9
    fig, axes = plt.subplots(
        len(panels),
        1,
        figsize=(12, fig_h),
        gridspec_kw={"height_ratios": height_ratios, "hspace": 0.12},
    )
    if len(panels) == 1:
        axes = [axes]

    fig.suptitle(
        "Model comparison scores (training data): DIC, WAIC, and LOO-CV",
        fontsize=12,
        fontweight="bold",
        y=0.995,
    )

    for ax, (subtitle, header, data) in zip(axes, panels, strict=True):
        render_table_on_axes(ax, header, data, subtitle, compact=True)

    fig.text(
        0.5,
        0.008,
        "Lower DIC, WAIC, and LOOIC indicate better in-sample fit; higher elpd_loo is better. "
        "Scores compare baseline vs alternative beta priors on the same training likelihood.",
        ha="center",
        fontsize=8,
        color="#333333",
    )

    fig.subplots_adjust(left=0.04, right=0.98, top=0.97, bottom=0.045, hspace=0.28)
    out_png = MODEL_COMPARISON_DIR / f"{out_stem}.png"
    out_jpg = MODEL_COMPARISON_DIR / f"{out_stem}.jpg"
    fig.savefig(out_png, dpi=dpi, bbox_inches="tight", facecolor="white")
    fig.savefig(out_jpg, dpi=dpi, bbox_inches="tight", facecolor="white", pil_kwargs={"quality": 95})
    plt.close()

    return out_png, out_jpg


def main() -> None:
    p = argparse.ArgumentParser(description="Export CSV tables as PNG/JPG images.")
    p.add_argument(
        "csv_paths",
        nargs="*",
        type=Path,
        default=None,
        help="CSV files (default: model_comparison_*.csv list)",
    )
    p.add_argument("--dpi", type=int, default=200)
    p.add_argument(
        "--combined-only",
        action="store_true",
        help="Write only the merged comparison figure",
    )
    p.add_argument(
        "--no-combined",
        action="store_true",
        help="Skip merged comparison figure",
    )
    args = p.parse_args()

    use_defaults = not args.csv_paths
    paths = [Path(x) for x in args.csv_paths] if args.csv_paths else DEFAULT_CSVS

    if args.combined_only:
        png, jpg = export_combined(paths, dpi=args.dpi)
        print("Wrote:", png)
        print("Wrote:", jpg)
        return

    for csv_path in paths:
        png, jpg = export_one(csv_path, dpi=args.dpi)
        print("Wrote:", png)
        print("Wrote:", jpg)

    if use_defaults and not args.no_combined:
        png, jpg = export_combined(DEFAULT_CSVS, dpi=args.dpi)
        print("Wrote:", png)
        print("Wrote:", jpg)


if __name__ == "__main__":
    main()

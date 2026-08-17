#!/usr/bin/env python3
"""Extract latest school-district mill rate from WI Tableau workbook.

Outputs:
- data/raw/sd_taxes/school_district_mill_rate_latest.csv
- data/raw/sd_taxes/school_district_mill_rate_latest.parquet
"""

from __future__ import annotations

import argparse
import shutil
import zipfile
from pathlib import Path

import pandas as pd
import requests
from tableauhyperapi import Connection, CreateMode, HyperProcess, Telemetry

DEFAULT_WORKBOOK_URL = "https://public.tableau.com/workbooks/SchoolDistrictIncomeandPropertyTax.twb"


def download_workbook(url: str, out_path: Path) -> None:
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with requests.get(url, stream=True, timeout=120) as resp:
        resp.raise_for_status()
        with out_path.open("wb") as f:
            for chunk in resp.iter_content(chunk_size=1024 * 1024):
                if chunk:
                    f.write(chunk)


def unpack_workbook(workbook_path: Path, unpack_dir: Path) -> None:
    if unpack_dir.exists():
        shutil.rmtree(unpack_dir)
    unpack_dir.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(workbook_path, "r") as zf:
        zf.extractall(unpack_dir)


def load_hyper_table(hyper_path: Path) -> pd.DataFrame:
    with HyperProcess(telemetry=Telemetry.DO_NOT_SEND_USAGE_DATA_TO_TABLEAU) as hyper:
        with Connection(
            endpoint=hyper.endpoint,
            database=str(hyper_path),
            create_mode=CreateMode.NONE,
        ) as conn:
            rows = list(
                conn.execute_query(
                    'SELECT "School District Name", "District Number", "IPAS Year", '
                    '"Pivot Field Names", "Pivot Field Values" '
                    'FROM "Extract"."Extract"'
                )
            )
    return pd.DataFrame(
        rows,
        columns=["School District Name", "District Number", "IPAS Year", "Pivot Field Names", "Pivot Field Values"],
    )


def main() -> None:
    parser = argparse.ArgumentParser(description="Extract latest WI school district mill-rate table.")
    parser.add_argument("--base-dir", default=".", help="Project base directory (default: current directory)")
    parser.add_argument(
        "--workbook-url",
        default=DEFAULT_WORKBOOK_URL,
        help="School district Tableau workbook URL ending in .twb",
    )
    parser.add_argument(
        "--refresh-workbook",
        action="store_true",
        help="Download and unpack the workbook before extracting",
    )
    args = parser.parse_args()

    base_dir = Path(args.base_dir).resolve()
    source_dir = base_dir / "scripts" / "sources"
    workbook_path = source_dir / "SchoolDistrictIncomeandPropertyTax.twb"
    unpack_dir = source_dir / "sd_tax_unpack"
    out_dir = base_dir / "data" / "raw" / "sd_taxes"
    out_dir.mkdir(parents=True, exist_ok=True)

    if args.refresh_workbook or not workbook_path.exists():
        print(f"Downloading workbook: {args.workbook_url}")
        download_workbook(args.workbook_url, workbook_path)

    if args.refresh_workbook or not unpack_dir.exists():
        print(f"Unpacking workbook: {workbook_path}")
        unpack_workbook(workbook_path, unpack_dir)

    hyper_candidates = sorted(unpack_dir.rglob("federated.hyper"))
    if not hyper_candidates:
        raise FileNotFoundError(f"No federated.hyper found under {unpack_dir}")
    hyper_path = hyper_candidates[0]
    print(f"Using hyper extract: {hyper_path}")

    raw_df = load_hyper_table(hyper_path)
    df = raw_df.rename(
        columns={
            "School District Name": "district_name",
            "District Number": "district_number",
            "IPAS Year": "ipas_year",
            "Pivot Field Names": "metric",
            "Pivot Field Values": "mill_rate",
        }
    )

    df = df[df["metric"].astype(str).str.strip().eq("Mill Rate")].copy()
    df["district_number"] = pd.to_numeric(df["district_number"], errors="coerce")
    df["ipas_year"] = pd.to_numeric(df["ipas_year"], errors="coerce")
    df["mill_rate"] = pd.to_numeric(df["mill_rate"], errors="coerce")
    df["district_name"] = df["district_name"].astype(str).str.strip()
    df = df.dropna(subset=["district_number", "ipas_year", "mill_rate"])

    df["district_number"] = df["district_number"].astype(int)
    df["ipas_year"] = df["ipas_year"].astype(int)
    df["SDID"] = df["district_number"].astype(str).str.zfill(4)

    latest = (
        df.sort_values(["district_number", "ipas_year"])
        .groupby("district_number", as_index=False)
        .tail(1)
        .sort_values("district_number")
    )

    csv_path = out_dir / "school_district_mill_rate_latest.csv"
    parquet_path = out_dir / "school_district_mill_rate_latest.parquet"
    latest.to_csv(csv_path, index=False)
    latest.to_parquet(parquet_path, index=False)

    print(f"Wrote {len(latest)} rows to {csv_path}")
    print(f"Wrote {len(latest)} rows to {parquet_path}")
    print(f"Latest IPAS year in output: {latest['ipas_year'].max()}")


if __name__ == "__main__":
    main()

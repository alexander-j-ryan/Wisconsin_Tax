#!/usr/bin/env python3
"""Download and export all tables from the Wisconsin Local Government Tableau workbook.

This script:
1) Downloads the Tableau workbook package from Tableau Public
2) Unzips embedded .hyper extract files
3) Exports every table in every schema from each .hyper file to CSV
"""

from __future__ import annotations

import argparse
import csv
import re
import shutil
import zipfile
from pathlib import Path

import requests
from tableauhyperapi import Connection, CreateMode, HyperProcess, Telemetry

DEFAULT_WORKBOOK_URL = (
    "https://public.tableau.com/workbooks/LocalGovernmentDashboard_0.twb"
)


def safe_name(text: str) -> str:
    return re.sub(r"[^A-Za-z0-9._-]+", "_", text).strip("_")


def download_workbook(url: str, out_path: Path) -> None:
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with requests.get(url, stream=True, timeout=120) as resp:
        resp.raise_for_status()
        with out_path.open("wb") as f:
            for chunk in resp.iter_content(chunk_size=1024 * 1024):
                if chunk:
                    f.write(chunk)


def unpack_workbook(workbook_path: Path, unpack_dir: Path) -> list[Path]:
    if unpack_dir.exists():
        shutil.rmtree(unpack_dir)
    unpack_dir.mkdir(parents=True, exist_ok=True)

    with zipfile.ZipFile(workbook_path, "r") as zf:
        zf.extractall(unpack_dir)

    return sorted(unpack_dir.rglob("*.hyper"))


def export_hyper_to_csv(hyper_path: Path, out_dir: Path, dataset_name: str) -> int:
    dataset_dir = out_dir / dataset_name
    dataset_dir.mkdir(parents=True, exist_ok=True)

    exported = 0
    with HyperProcess(telemetry=Telemetry.DO_NOT_SEND_USAGE_DATA_TO_TABLEAU) as hyper:
        with Connection(
            endpoint=hyper.endpoint,
            database=str(hyper_path),
            create_mode=CreateMode.NONE,
        ) as conn:
            for schema_name in conn.catalog.get_schema_names():
                tables = conn.catalog.get_table_names(schema_name)
                for table_name in tables:
                    cols = conn.catalog.get_table_definition(table_name).columns
                    col_names = [col.name.unescaped for col in cols]
                    query = f"SELECT * FROM {table_name}"

                    schema_txt = safe_name(table_name.schema_name.name.unescaped)
                    table_txt = safe_name(table_name.name.unescaped)
                    out_csv = dataset_dir / f"{schema_txt}__{table_txt}.csv"

                    with out_csv.open("w", newline="", encoding="utf-8") as f:
                        writer = csv.writer(f)
                        writer.writerow(col_names)
                        result = conn.execute_query(query)
                        for row in result:
                            writer.writerow(list(row))

                    exported += 1

    return exported


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Export all tables from the Wisconsin Local Government Tableau workbook."
    )
    parser.add_argument(
        "--workbook-url",
        default=DEFAULT_WORKBOOK_URL,
        help="Tableau Public workbook URL ending in .twb",
    )
    parser.add_argument(
        "--base-dir",
        default=".",
        help="Project base directory (default: current directory)",
    )
    args = parser.parse_args()

    base_dir = Path(args.base_dir).resolve()
    source_dir = base_dir / "sources"
    unpack_dir = source_dir / "workbook_unpacked"
    raw_dir = base_dir / "data" / "raw"

    workbook_path = source_dir / "LocalGovernmentDashboard_0.twb"

    print(f"Downloading workbook: {args.workbook_url}")
    download_workbook(args.workbook_url, workbook_path)

    print(f"Unpacking workbook: {workbook_path}")
    hyper_files = unpack_workbook(workbook_path, unpack_dir)
    if not hyper_files:
        raise RuntimeError("No .hyper files found in workbook package.")

    print(f"Found {len(hyper_files)} hyper file(s).")
    total_tables = 0
    for idx, hp in enumerate(hyper_files, start=1):
        print(f"Exporting tables from: {hp}")
        dataset_name = f"{idx:02d}_{safe_name(hp.parent.name)}_{safe_name(hp.stem)}"
        count = export_hyper_to_csv(hp, raw_dir, dataset_name)
        total_tables += count
        print(f"  exported {count} table(s)")

    print(f"Done. Exported {total_tables} table(s) to {raw_dir}")


if __name__ == "__main__":
    main()

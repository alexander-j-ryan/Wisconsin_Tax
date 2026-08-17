#!/usr/bin/env python3

from __future__ import annotations

import argparse
import shutil
import zipfile
from pathlib import Path

import pandas as pd
import requests

from tableauhyperapi import (
    Connection,
    CreateMode,
    Date,
    HyperProcess,
    TableName,
    Telemetry,
    Timestamp,
)


# -------------------------------------------------------------------
# Columns we expect the assessed-value table to contain
#
# I would NOT require both TAXYR and YEAR yet.
# -------------------------------------------------------------------

REQUIRED_COLS = [
    "AUTHCODE",
    "Municipality",
    "COUNTY_NAME",
    "Real Property Class",
    "ASSESSED VALUE",
]


# -------------------------------------------------------------------
# Download Tableau workbook
# -------------------------------------------------------------------

def download(url: str, out_file: Path) -> None:

    out_file.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    headers = {
        "User-Agent": (
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
            "AppleWebKit/537.36 Safari/537.36"
        )
    }

    with requests.get(
        url,
        stream=True,
        timeout=180,
        headers=headers,
    ) as r:

        r.raise_for_status()

        with out_file.open("wb") as f:

            for chunk in r.iter_content(
                chunk_size=1024 * 1024
            ):

                if chunk:
                    f.write(chunk)

    print(
        f"Downloaded "
        f"{out_file.stat().st_size / 1024**2:,.1f} MB"
    )


# -------------------------------------------------------------------
# Unpack Tableau workbook
# -------------------------------------------------------------------

def normalize_value(value):
    """
    Convert Tableau Hyper-specific types to standard Python types
    that pandas / PyArrow can write to Parquet.
    """

    if value is None:
        return None

    if isinstance(value, Date):
        return value.to_date()

    if isinstance(value, Timestamp):
        return value.to_datetime()

    return value

def unpack_zip(
    zip_path: Path,
    out_dir: Path,
) -> None:

    if not zipfile.is_zipfile(zip_path):

        raise RuntimeError(
            "\nDownloaded Tableau file is not a ZIP-compatible "
            "packaged workbook:\n"
            f"{zip_path}"
        )

    if out_dir.exists():
        shutil.rmtree(out_dir)

    out_dir.mkdir(
        parents=True,
        exist_ok=True,
    )

    with zipfile.ZipFile(
        zip_path,
        "r",
    ) as zf:

        zf.extractall(out_dir)


# -------------------------------------------------------------------
# Normalize field names for matching
# -------------------------------------------------------------------

def normalize_col_name(x: str) -> str:
    """
    Normalize Tableau column names so matching isn't defeated by
    capitalization, underscores, or extra spaces.
    """

    return (
        x.lower()
        .replace("_", " ")
        .strip()
    )


# -------------------------------------------------------------------
# Search Hyper databases
# -------------------------------------------------------------------

def find_table_with_required_cols(
    hyper_path: Path,
) -> tuple[TableName, list[str]] | None:

    required_normalized = {
        normalize_col_name(c)
        for c in REQUIRED_COLS
    }

    with HyperProcess(
        telemetry=Telemetry.DO_NOT_SEND_USAGE_DATA_TO_TABLEAU
    ) as hyper:

        with Connection(
            endpoint=hyper.endpoint,
            database=str(hyper_path),
            create_mode=CreateMode.NONE,
        ) as conn:

            for schema_name in conn.catalog.get_schema_names():

                for table_name in (
                    conn.catalog.get_table_names(
                        schema_name
                    )
                ):

                    table_definition = (
                        conn.catalog.get_table_definition(
                            table_name
                        )
                    )

                    cols = [
                        c.name.unescaped
                        for c in table_definition.columns
                    ]

                    cols_normalized = {
                        normalize_col_name(c)
                        for c in cols
                    }

                    print()
                    print(
                        f"Checking table: {table_name}"
                    )

                    print(
                        f"  columns: {len(cols)}"
                    )

                    # Print names so we can inspect what Tableau
                    # actually stored.
                    for c in cols:
                        print(f"    {c}")

                    if required_normalized.issubset(
                        cols_normalized
                    ):

                        print(
                            "\n*** MATCH FOUND ***"
                        )

                        return table_name, cols

    return None


# -------------------------------------------------------------------
# Export Hyper table
# -------------------------------------------------------------------
def export_table_to_parquet(
    hyper_path: Path,
    table_name: TableName,
    col_names: list[str],
    out_parquet: Path,
) -> None:

    out_parquet.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    rows = []

    with HyperProcess(
        telemetry=Telemetry.DO_NOT_SEND_USAGE_DATA_TO_TABLEAU
    ) as hyper:

        with Connection(
            endpoint=hyper.endpoint,
            database=str(hyper_path),
            create_mode=CreateMode.NONE,
        ) as conn:

            print()
            print(f"Reading table {table_name}...")

            with conn.execute_query(
                f"SELECT * FROM {table_name}"
            ) as result:

                for row in result:

                    rows.append(
                        [
                            normalize_value(value)
                            for value in row
                        ]
                    )

    print(f"Read {len(rows):,} rows.")

    df = pd.DataFrame(
        rows,
        columns=col_names,
    )

    print(
        f"DataFrame: "
        f"{len(df):,} rows x "
        f"{len(df.columns):,} columns"
    )

    # -----------------------------------------------------------
    # Inspect YEAR after converting Hyper Date -> Python date
    # -----------------------------------------------------------

    if "YEAR" in df.columns:

        print()
        print("YEAR example values:")
        print(df["YEAR"].head())

        print(
            f"YEAR dtype before cleanup: "
            f"{df['YEAR'].dtype}"
        )

        # Tableau appears to store YEAR as a date such as
        # 2003-06-01. Convert to pandas datetime.
        df["YEAR"] = pd.to_datetime(
            df["YEAR"],
            errors="coerce",
        )

    # -----------------------------------------------------------
    # Write Parquet
    # -----------------------------------------------------------

    df.to_parquet(
        out_parquet,
        index=False,
        engine="pyarrow",
    )

    print()
    print("Saved:")
    print(f"  {out_parquet}")

# -------------------------------------------------------------------
# Main
# -------------------------------------------------------------------

def main() -> None:

    parser = argparse.ArgumentParser(
        description=(
            "Download Wisconsin DOR Assessed Values Tableau "
            "workbook and export its assessed-value data."
        )
    )

    parser.add_argument(
        "--workbook-url",
        default=(
            "https://public.tableau.com/"
            "workbooks/AssessedValues0_2.twb"
        ),
    )

    parser.add_argument(
        "--base-dir",
        default=".",
    )

    args = parser.parse_args()

    # ---------------------------------------------------------------
    # Directories
    # ---------------------------------------------------------------

    base = (
        Path(args.base_dir)
        .expanduser()
        .resolve()
    )

    src = (
        base
        / "sources"
        / "assessed_values"
    )

    twb = (
        src
        / "AssessedValues0_2.twb"
    )

    unpack = (
        src
        / "unpacked"
    )

    out_file = (
        base
        / "data"
        / "raw"
        / "tvc_taxes"
        / "02_Assessed_Values_AV_Real_Property"
        / "Extract__Extract.parquet"
    )

    # ---------------------------------------------------------------
    # Download
    # ---------------------------------------------------------------

    print("=" * 80)
    print("DOWNLOADING WORKBOOK")
    print("=" * 80)

    print(
        f"URL: {args.workbook_url}"
    )

    download(
        args.workbook_url,
        twb,
    )

    # ---------------------------------------------------------------
    # Unpack
    # ---------------------------------------------------------------

    print()
    print("=" * 80)
    print("UNPACKING WORKBOOK")
    print("=" * 80)

    unpack_zip(
        twb,
        unpack,
    )

    # ---------------------------------------------------------------
    # Locate Hyper files
    # ---------------------------------------------------------------

    hypers = sorted(
        unpack.rglob("*.hyper")
    )

    if not hypers:

        raise RuntimeError(
            "No .hyper files found in workbook package."
        )

    print()
    print(
        f"Found {len(hypers)} Hyper file(s)."
    )

    for hp in hypers:
        print(f"  {hp}")

    # ---------------------------------------------------------------
    # Search for assessed-value table
    # ---------------------------------------------------------------

    chosen_hyper = None
    chosen_table = None
    chosen_cols = None

    for hp in hypers:

        print()
        print("=" * 80)
        print(f"SEARCHING: {hp}")
        print("=" * 80)

        found = find_table_with_required_cols(
            hp
        )

        if found is not None:

            chosen_hyper = hp
            chosen_table, chosen_cols = found

            break

    # Use explicit None tests instead of `not`
    if (
        chosen_hyper is None
        or chosen_table is None
        or chosen_cols is None
    ):

        raise RuntimeError(
            "\nDid not find a table containing all required "
            "assessed-value columns.\n\n"
            "Review the column names printed above. Tableau may "
            "use different underlying field names than the "
            "displayed names."
        )

    # ---------------------------------------------------------------
    # Export
    # ---------------------------------------------------------------

    print()
    print("=" * 80)
    print("EXPORTING")
    print("=" * 80)

    print(
        f"Hyper file:\n"
        f"  {chosen_hyper}"
    )

    print(
        f"Table:\n"
        f"  {chosen_table}"
    )

    export_table_to_parquet(
        hyper_path=chosen_hyper,
        table_name=chosen_table,
        col_names=chosen_cols,
        out_parquet=out_file,
    )

    print()
    print("=" * 80)
    print("DONE")
    print("=" * 80)


if __name__ == "__main__":
    main()
#!/usr/bin/env python3
"""Estimate Wisconsin property-tax collections by class and tax district.

This workflow combines:
1) Class-level assessed values (AssessedValues Tableau workbook), and
2) Tax-district millage / levy fields (TVC Tableau workbook),
to estimate class-level collections for each tax district and year.
"""

from __future__ import annotations

import argparse
import re
from pathlib import Path

import numpy as np
import pandas as pd


CLASS_MAP = {
    "Residential": "Residential",
    "Commercial": "Commercial",
    "Manufacturing": "Industrial",
    "Agricultural": "Agricultural",
    "Agricultural Forest": "Agricultural",
    "Forest": "Other",
    "Undeveloped": "Other",
    "Other": "Other",
    "L": "Other",
    "T": "Other",
}

COMPONENT_FIELDS = [
    "County Tax",
    "K-12 School Tax",
    "Municipal Tax",
    "Technical College Tax",
    "Special Districts Tax",
    "State Tax",
    "TID Tax",
    "School Levies Credit",
    "Municipal Tax Levy",
    "Net Tax Levy",
    "Gross Tax Levy",
]


def norm_auth_code(x: object) -> str | None:
    if x is None or (isinstance(x, float) and np.isnan(x)):
        return None
    digits = re.sub(r"[^0-9]", "", str(x))
    if not digits:
        return None
    return digits.zfill(5)


def find_one(path_glob: str, base_dir: Path) -> Path:
    matches = sorted(base_dir.glob(path_glob))
    if not matches:
        raise FileNotFoundError(f"No file matched: {path_glob}")
    return matches[0]


def build_class_values(assessed_path: Path) -> pd.DataFrame:
    av = pd.read_parquet(
        assessed_path,
        columns=[
            "TAXYR",
            "YEAR",
            "AUTHCODE",
            "Municipality",
            "COUNTY_NAME",
            "Real Property Class",
            "ASSESSED VALUE",
        ],
    )
    av["year"] = pd.to_numeric(av["TAXYR"], errors="coerce")
    av["year"] = av["year"].fillna(pd.to_datetime(av["YEAR"], errors="coerce").dt.year)
    av["year"] = av["year"].astype("Int64")
    av["auth_code"] = av["AUTHCODE"].map(norm_auth_code)
    av["assessed_value"] = pd.to_numeric(av["ASSESSED VALUE"], errors="coerce")
    av["class_raw"] = av["Real Property Class"].astype("string")
    av["class_group"] = av["class_raw"].map(CLASS_MAP).fillna("Other")

    av = av[
        av["year"].notna()
        & av["auth_code"].notna()
        & av["class_raw"].notna()
        & av["assessed_value"].notna()
        & (av["assessed_value"] >= 0)
    ].copy()

    class_values = (
        av.groupby(
            ["year", "auth_code", "COUNTY_NAME", "Municipality", "class_raw", "class_group"],
            as_index=False,
        )["assessed_value"]
        .sum()
        .rename(columns={"COUNTY_NAME": "county_name", "Municipality": "municipality"})
    )

    totals = class_values.groupby(["year", "auth_code"], as_index=False)["assessed_value"].sum()
    totals = totals.rename(columns={"assessed_value": "district_total_assessed_value"})
    class_values = class_values.merge(totals, on=["year", "auth_code"], how="left")
    class_values["class_share_of_assessed_value"] = (
        class_values["assessed_value"] / class_values["district_total_assessed_value"]
    )

    return class_values


def build_rate_table(rate_path: Path) -> pd.DataFrame:
    rate = pd.read_parquet(
        rate_path,
        columns=[
            "YEAR",
            "Auth. Code",
            "County",
            "Muni Name",
            "Muni Type",
            "Pivot Field Names",
            "Pivot Field Values",
            "Gross Tax Levy",
            "Net Tax Levy",
        ],
    )
    rate["year"] = pd.to_datetime(rate["YEAR"], errors="coerce").dt.year.astype("Int64")
    rate["auth_code"] = rate["Auth. Code"].map(norm_auth_code)
    rate["pivot_name"] = rate["Pivot Field Names"].astype("string")
    rate["pivot_value"] = pd.to_numeric(rate["Pivot Field Values"], errors="coerce")
    rate["gross_tax_levy"] = pd.to_numeric(rate["Gross Tax Levy"], errors="coerce")
    rate["net_tax_levy"] = pd.to_numeric(rate["Net Tax Levy"], errors="coerce")

    rate = rate[rate["year"].notna() & rate["auth_code"].notna()].copy()

    wide_rates = (
        rate.pivot_table(
            index=["year", "auth_code"],
            columns="pivot_name",
            values="pivot_value",
            aggfunc="first",
        )
        .reset_index()
        .rename_axis(None, axis=1)
    )

    meta = (
        rate.groupby(["year", "auth_code"], as_index=False)
        .agg(
            county=("County", "first"),
            muni_name=("Muni Name", "first"),
            muni_type=("Muni Type", "first"),
            gross_tax_levy=("gross_tax_levy", "max"),
            net_tax_levy=("net_tax_levy", "max"),
        )
        .copy()
    )

    out = meta.merge(wide_rates, on=["year", "auth_code"], how="left")
    out = out.rename(
        columns={
            "Net Tax Rate (mills)": "net_tax_rate_mills",
            "Gross Tax Rate (mills)": "gross_tax_rate_mills",
        }
    )
    return out


def build_component_table(tvc_main_path: Path) -> pd.DataFrame:
    tvc = pd.read_parquet(
        tvc_main_path,
        columns=[
            "YEAR",
            "Auth. Code",
            "County",
            "Muni Name",
            "Muni Type",
            "Pivot Field Names",
            "Pivot Field Values",
        ],
    )
    tvc["year"] = pd.to_datetime(tvc["YEAR"], errors="coerce").dt.year.astype("Int64")
    tvc["auth_code"] = tvc["Auth. Code"].map(norm_auth_code)
    tvc["pivot_name"] = tvc["Pivot Field Names"].astype("string")
    tvc["pivot_value"] = pd.to_numeric(tvc["Pivot Field Values"], errors="coerce")

    tvc = tvc[tvc["year"].notna() & tvc["auth_code"].notna()].copy()
    tvc = tvc[tvc["pivot_name"].isin(COMPONENT_FIELDS)].copy()

    comp_wide = (
        tvc.pivot_table(
            index=["year", "auth_code", "County", "Muni Name", "Muni Type"],
            columns="pivot_name",
            values="pivot_value",
            aggfunc="first",
        )
        .reset_index()
        .rename_axis(None, axis=1)
        .rename(
            columns={
                "County": "county",
                "Muni Name": "muni_name",
                "Muni Type": "muni_type",
            }
        )
    )
    return comp_wide


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Estimate WI property-tax collections by class and tax district."
    )
    parser.add_argument(
        "--base-dir",
        default=".",
        help="Project base directory (default: current directory)",
    )
    args = parser.parse_args()

    base_dir = Path(args.base_dir).resolve()
    raw_dir = base_dir / "data" / "raw" / "tvc_taxes"
    out_dir = base_dir / "data" / "intermediate"
    out_dir.mkdir(parents=True, exist_ok=True)

    assessed_path = find_one("data/raw/tvc_taxes/*Assessed_Values_AV_Real_Property*/Extract__Extract.parquet", base_dir)
    rate_path = raw_dir / "02_TVC_Dashboard_1_Tax_Rate_Data_Tax_Rate_Data" / "Extract__Extract.parquet"
    tvc_main_path = raw_dir / "01_TVC_Dashboard_Tableau_TVC_Data_0.1" / "Extract__Extract.parquet"

    class_values = build_class_values(assessed_path)
    rates = build_rate_table(rate_path)
    components = build_component_table(tvc_main_path)

    class_with_rates = class_values.merge(
        rates[
            [
                "year",
                "auth_code",
                "county",
                "muni_name",
                "muni_type",
                "net_tax_rate_mills",
                "gross_tax_rate_mills",
                "net_tax_levy",
                "gross_tax_levy",
            ]
        ],
        on=["year", "auth_code"],
        how="left",
    )

    class_with_rates["estimated_net_tax_from_rate"] = (
        class_with_rates["assessed_value"] * class_with_rates["net_tax_rate_mills"] / 1000.0
    )
    class_with_rates["estimated_gross_tax_from_rate"] = (
        class_with_rates["assessed_value"] * class_with_rates["gross_tax_rate_mills"] / 1000.0
    )
    class_with_rates["has_rate"] = class_with_rates["net_tax_rate_mills"].notna()

    class_with_components = class_with_rates.merge(
        components,
        on=["year", "auth_code"],
        how="left",
        suffixes=("", "_comp"),
    )

    for field in COMPONENT_FIELDS:
        out_col = f"est_{field.lower().replace(' ', '_').replace('-', '_')}"
        class_with_components[out_col] = (
            class_with_components["class_share_of_assessed_value"]
            * pd.to_numeric(class_with_components.get(field), errors="coerce")
        )

    # Output 1: class assessed values by district/year
    assessed_out = class_values.sort_values(["year", "county_name", "municipality", "class_group"])
    assessed_out.to_csv(
        out_dir / "wi_property_class_assessed_values_by_tax_district.csv", index=False
    )
    assessed_out.to_parquet(
        out_dir / "wi_property_class_assessed_values_by_tax_district.parquet", index=False
    )

    # Output 2: rate-based collection estimates by district/year/class
    est_cols = [
        "year",
        "auth_code",
        "county_name",
        "municipality",
        "county",
        "muni_name",
        "muni_type",
        "class_raw",
        "class_group",
        "assessed_value",
        "district_total_assessed_value",
        "class_share_of_assessed_value",
        "net_tax_rate_mills",
        "gross_tax_rate_mills",
        "net_tax_levy",
        "gross_tax_levy",
        "has_rate",
        "estimated_net_tax_from_rate",
        "estimated_gross_tax_from_rate",
    ]
    est_out = class_with_rates[est_cols].sort_values(
        ["year", "county_name", "municipality", "class_group"]
    )
    est_out.to_csv(out_dir / "wi_property_tax_class_estimates_from_millage.csv", index=False)
    est_out.to_parquet(
        out_dir / "wi_property_tax_class_estimates_from_millage.parquet", index=False
    )

    # Output 3: component-allocation estimates by district/year/class
    comp_cols = est_cols + [
        "County Tax",
        "K-12 School Tax",
        "Municipal Tax",
        "Technical College Tax",
        "Special Districts Tax",
        "State Tax",
        "TID Tax",
        "School Levies Credit",
        "Municipal Tax Levy",
        "Net Tax Levy",
        "Gross Tax Levy",
    ] + [f"est_{f.lower().replace(' ', '_').replace('-', '_')}" for f in COMPONENT_FIELDS]

    comp_out = class_with_components[comp_cols].sort_values(
        ["year", "county_name", "municipality", "class_group"]
    )
    comp_out.to_csv(
        out_dir / "wi_property_tax_class_estimates_component_allocated.csv", index=False
    )
    comp_out.to_parquet(
        out_dir / "wi_property_tax_class_estimates_component_allocated.parquet", index=False
    )

    # Output 4: statewide yearly summary
    est_matched = est_out[est_out["has_rate"]].copy()
    summary = (
        est_matched.groupby(["year", "class_group"], as_index=False)
        .agg(
            assessed_value=("assessed_value", "sum"),
            estimated_net_tax_from_rate=("estimated_net_tax_from_rate", "sum"),
            estimated_gross_tax_from_rate=("estimated_gross_tax_from_rate", "sum"),
        )
        .sort_values(["year", "class_group"])
    )
    summary.to_csv(out_dir / "wi_property_tax_class_estimates_statewide_summary.csv", index=False)

    latest_matched = int(est_matched["year"].max())
    latest_summary = summary[summary["year"] == latest_matched].copy()
    latest_total = latest_summary["estimated_net_tax_from_rate"].sum()
    print(
        f"Built class-tax estimates for years {int(summary['year'].min())}-{latest_matched}. "
        f"Latest estimated net total: ${latest_total:,.2f}"
    )
    print(latest_summary.to_string(index=False))


if __name__ == "__main__":
    main()

# Wisconsin Taxes

Repository for downloading and exporting tabular data from the Wisconsin Local Government Tableau dashboard.

## Current source

- Tableau viz page: `public.tableau.com/app/profile/research.policy/viz/LocalGovernmentDashboard_0/LocalGovernment`
- Workbook package used by script: `https://public.tableau.com/workbooks/LocalGovernmentDashboard_0.twb`

## Setup

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

## Export all tables

```bash
python3 scripts/export_tableau_local_government.py --base-dir .
```

Outputs:

- Downloaded workbook: `sources/LocalGovernmentDashboard_0.twb`
- Unpacked extract files: `sources/workbook_unpacked/`
- Exported CSV tables: `data/raw/<hyper_name>/<schema>__<table>.csv`

## Notes

- The workbook currently contains embedded Tableau Hyper extracts. Exporting those extracts captures all rows available in the published workbook (all years, counties/cities/villages/towns present in the extract).
- If Tableau republishes with a new workbook name/version, update `DEFAULT_WORKBOOK_URL` in the script.

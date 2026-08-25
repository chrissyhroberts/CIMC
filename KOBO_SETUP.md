# Kobo production setup

The Quarto project runs `prepare_dashboard_data.R` before every render.

## Development mode

Leave `data_source.mode: development` in `dashboard_config.yml`. The preparation
step stages the five linked dummy datasets into `data/active/`, and the dashboard
reads those staged files.

## Production mode

1. In `dashboard_config.yml`, enter the correct Kobo server URL and the three
   deployed project asset UIDs under `data_source.production.asset_uids`.
2. Put the API token in the environment variable named by
   `api_token_environment_variable`. The default is `KOBO_API_TOKEN`.
3. Change `data_source.mode` to `production`.
4. Render the Quarto book normally.

For a temporary R session, set the token before rendering:

```r
Sys.setenv(KOBO_API_TOKEN = "your-token-here")
```

For an unattended deployment, configure `KOBO_API_TOKEN` in the hosting or CI
secret store. Do not commit the token to YAML, R code, shell scripts, logs, or
the rendered site.

Production mode downloads all pages from Kobo API v2, writes timestamped raw
JSON snapshots to the configured snapshot directory, validates the deployed
forms, stages normalized CSV files in `data/active/`, and derives the daily and
monthly analysis datasets. By default, the operational cutoff is the retrieval
date in the configured study time zone, so a day with no submissions still
advances missed-report monitoring. This can be changed with
`data_cutoff_strategy`.

Participant-level action lists are always generated in the project directory.
They are not copied into the production HTML site unless
`publish_action_lists_in_rendered_site` is explicitly changed to `true`.

Official references:

- [KoboToolbox API setup and asset UIDs](https://support.kobotoolbox.org/api)
- [KoboToolbox API v2 submissions and pagination](https://support.kobotoolbox.org/migrating_api.html)

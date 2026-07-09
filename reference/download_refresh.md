# Download or redownload data

Downloads a file or re-downloads a file to refresh the cached files.

## Usage

``` r
download_refresh(file_url, local, refresh)
```

## Arguments

- file_url:

  String. URL for file you'd like to download

- local:

  String. A character string with the name where the downloaded file is
  saved. Tilde-expansion is performed.

- refresh:

  Logical. Should saved files be refreshed.

## Value

String. Local File path

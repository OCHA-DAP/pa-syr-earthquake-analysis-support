
# Syria Earthquake Analysis Support

This repository is for simple analysis on the shelter survey done in the wake
of the 2023 earthquake impacting northwest Syria.

## Data wrangling

The first part of the script simply pulls the data into a more usable format.

### Name cleaning

A conversion dictionary of XLSForm names is defined in Google Sheets
that converts the raw XML names into cleaner versions of themselves.

### Translation

Text fields in Arabic are passed to the Google Translation API for translation
into English. This allows any users working in the Syria response to be able
to explore and understand the data.

### Field calculation

A few fields are calculated, described in the XLSForm dictionary, that estimate
the total number of households and population within the area.

## Technical requirements

Running the scripts requires some technical setup, beyond installing the
necessary packages.

### Data file

The local data file is stored on the team drive. Store the path to this in the
project environment by editing the project-level `.Renviron` file using
`usethis::edit_r_environ("project")`. Add this line:

```r
SYR_EQUAKE_DIR="Path to folder"
```

### Google Sheets

The survey is stored in Google Sheets. This is accessible through the
[googlesheets4](https://googlesheets4.tidyverse.org) R package that wraps the
V4 API. Authentication is fairly simple using your e-mail and browser, but
other options are available. Read the
[documentation here](https://googlesheets4.tidyverse.org/articles/auth.html).

The account will need access to the specific files
used here, and the specific URL for the survey XLSForm. Add this to the
`.Renviron` file as described above:

```r
SYR_EQUAKE_GS_URL="URL for Google Sheets"
```

### Google Translation API

Translation is done through the Google Translation API. This requires setup
of a project on Google Cloud, provision of a services account and keys to
access it, and then downloading of those keys as a JSON. This is described
in the [googleTranslateR](https://github.com/ropensci/googleLanguageR)
documentation.

With the downloaded JSON file, store it as `.gl_auth_client.json` in the top
level of the project folder, and then add the following line to your project
`.Renviron`:

```r
GL_AUTH=.gl_auth_client.json
```

This will allow automatic authentication for usage of the Translation API.
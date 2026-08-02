# Student Performance Factors — EDA

Exploratory analysis and failure-mode logistic regression on the
`StudentPerformanceFactors.csv` dataset. The report source is `EDA.Rmd`;
helper functions live in `R/`.

## Live report

Deployed automatically to GitHub Pages on every push to `main` via
`.github/workflows/pages.yml`.

### One-time setup (after pushing to GitHub)

1. Push this repo to GitHub.
2. In the repo, go to **Settings → Pages**.
3. Under **Build and deployment → Source**, select **GitHub Actions**.
4. Push to `main` (or run the workflow manually from the **Actions** tab) to
   trigger the first deploy. The site URL will appear on the Pages settings
   page and in the workflow run summary.

## Local rendering

```r
rmarkdown::render("EDA.Rmd")
```

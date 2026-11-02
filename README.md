# edpollution

R package and analysis code for **Air Pollution and Children's Health Inequalities**,
Milena Suarez Castillo, David Benatia and Christine Le Thi, *Journal of
Environmental Economics and Management* 131 (2025), 103149.

[Published paper](https://doi.org/10.1016/j.jeem.2025.103149) · [Working paper PDF](https://www.dropbox.com/scl/fi/hmjhwyercs9lhgj1q3ps7/air-pollution-children-working-paper-2025-03-14.pdf?rlkey=fjzgs8hrenr3soaaxr8bcnggd&dl=0) · [Author website and paper materials](https://davidbenatia.com/research/)

## Contents

- `R/`: functions for residential histories, pollution exposure and health-data analysis.
- `man/`: R documentation.
- `code_AirPollution_ChildrenHealthInequalities/`: paper-analysis scripts.

## Usage

Install the dependencies listed in `DESCRIPTION`, then install the package:

```r
remotes::install_github("dvbn/edpollution")
```

Run the numbered study scripts in order, followed by the appendix scripts.
The analysis requires access to EDP/EDP-Santé, the study-specific health extracts,
and pollution and weather inputs. These administrative datasets are not included.
Configure the data locations in `R/paths_utils.R` for your authorized environment.

Package authorship, maintenance and licensing information are recorded in `DESCRIPTION`.

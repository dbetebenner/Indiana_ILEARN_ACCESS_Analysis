# ILEARN × WIDA ACCESS: Signal vs Noise

A contained R pipeline that pairs Indiana **ILEARN ELA** with **WIDA ACCESS Overall** composite scores and asks whether there is a WIDA level at which the content test stops looking like noise. The operational question is whether that transition lines up with Indiana’s EL exit policy.

This directory is a **sandbox analysis**, not a package and not a client delivery. It reads two existing SGP LONG files and writes only aggregate tables and figures.

---

## Hypothesis

States require ELA of every student, including English learners. WIDA ACCESS is the English-language proficiency test used to decide whether a student can access content in English. The working claim:

- At low WIDA (roughly Levels 1–2), ILEARN ELA behaves like **noise**: floor pile-up, near-independence of the two ranks.
- At higher WIDA a **signal** appears: rank dependence, ILEARN distributions that start to look like they can access English-language content.

If that transition is real, it is evidence about where an exit criterion should sit. It is **not** itself the legal exit rule.

## What this is not

- Not the SGPc foundry WIDA-to-WIDA growth copula work (`SGPc-foundry/Indiana/english-language-proficiency/`).
- Not the IDOE growth RFP (`CenterForAssessment/Proposals/IDOE_Growth_ILEARN_ACCESS_2026/`).
- Not a re-run of the 38-hour Copula_Sensitivity family-selection grid. That work already found **t** and **Frank** dominate educational pairs. This pipeline fits those families for description and spends its effort on **local / threshold** dependence.

---

## Policy anchors

Source: Indiana DOE, *Exit Criteria for English Learners Guidance 2025–2026* (April 2025; ESSA State Plan amendment approved summer 2024). Encoded in the SGPc foundry authoring step and in `config.R`.

| Anchor | Meaning |
|---|---|
| **5.0** Overall composite | Absolute auto-exit |
| **4.3–4.9** Overall | Provisional pathway (grade 3+, Tier B/C): ILP-committee additional evidence, not a pure scale-score cut |

WIDA_IN LONG already stores a 7-level scheme: L1–L4, **L4.3** (4.3–4.9 collapsed), L5, L6. Decimal proficiency levels live in `ACHIEVEMENT_LEVEL_ORIGINAL`.

A fitted change-point is evidence about where content-test signal appears. It may or may not equal the legal exit rule.

---

## Data

Read-only sources (paths in `config.R`):

| Assessment | File | Keys | What we use |
|---|---|---|---|
| ILEARN / ISTEP | `Indiana/master/SGP/Data/Indiana_SGP_LONG_Data.Rdata` | `STUDENT_ID`, `SCHOOL_YEAR`, `GRADE_ID` | ELA (primary), Mathematics (contrast); `SCALE_SCORE`, `ACHIEVEMENT_LEVEL`, `ENGLISH_LEARNER_STATUS`, `SGP` |
| WIDA ACCESS | `WIDA_IN/master/Data/WIDA_IN_SGP_LONG_Data.Rdata` | `ID`, `YEAR`, `GRADE` | Overall composite (see caveat); `ACHIEVEMENT_LEVEL`, `ACHIEVEMENT_LEVEL_ORIGINAL`, `SGP` |

**Join:** `STUDENT_ID` = `ID`, `SCHOOL_YEAR` = `YEAR`, `GRADE_ID` = `GRADE`. Same year, same grade. Match rate in grades 3–8 is very high (almost every WIDA tester has ILEARN ELA).

**Overall, not Reading.** WIDA `CONTENT_AREA` is stored as `"READING"` but the score is the **Overall composite**. Every axis title and table header in this pipeline says “WIDA ACCESS Overall Composite.”

### Scale-change rules

| Window | Years | Role |
|---|---|---|
| **Primary** | 2021–2025 | Stable ILEARN scale (~5060–5920) and stable WIDA old scale. This is the estimand. |
| First ILEARN | 2019 | Replicate, not pooled with 2021–2025. |
| Dual reset | 2026 | Both scales changed. **Ranks only.** ILEARN 2026 `SCALE_SCORE` in the combined LONG is on the equated old scale (Indiana SGP 2026 did not swap names back; WIDA did). |
| ISTEP contrast | 2017–2018 | Different ELA test. Named contrast, not ILEARN evidence. |
| 2020 | — | ILEARN cancelled; WIDA exists but cannot pair. |

Placeholder floor scores (`ILEARN < 5000`, `WIDA == 100`) are dropped before any copula.

Raw 2025 and 2026 scores are never pooled. Rank methods are invariant to monotone scale changes, which is why they are the primary metric.

### Filters (`config.R`)

- Grades 3–8
- `VALID_CASE` only
- ILEARN score ≥ 5000
- WIDA score > 100
- Minimum N: 200 to fit a copula cell, 50 to report a WIDA-level slice, 10 to print a cell at all (else `suppress = TRUE`)

---

## Research questions

1. **Does a number exist, and is it the same by grade?** Look at `outputs/local_tau_changepoints.csv`, `outputs/exit_grade_consistency.csv`, and the τ(u) figures.
2. **Are the results consistent across years?** Compare the primary window (2021–2025) to 2026 on the rank scale (`outputs/exit_q1q2_summary.csv`, `outputs/ext_2026_dual_scale.csv`).
3. **How else can these data inform an exit criterion?** Step 06: Math contrast, never-EL reference, lagged pairing, panel / first crossing, SGP-vs-SGP (secondary), 2026 dual-scale robustness. Step 07: inferred **exiters** (ILEARN + WIDA at *t*, ILEARN only at *t+1*) versus stayers and never-EL. Step 08: lagged copula / 50-50 next-year ILEARN-proficiency cut — a **documented contrast**, not a recommended exit rule.

---

## How to run

From this directory:

```r
Rscript run_all.R          # steps 00–09 + JSON manifests
Rscript run_all.R 1 2      # pairing + N tables (00 always runs)
Rscript run_all.R 7        # exiter cohort only (uses slim ILEARN/WIDA caches)
Rscript run_all.R 8        # lagged 50-50 proficiency cut + manifests
Rscript run_all.R 9        # copula asymmetry + Youden explainer
Rscript 03_global_dependence.R   # a single step, if cache/pairs.rds exists
```

```bash
./docs/render.sh           # reveal.js deck → docs/index.html (GitHub Pages)
```

Requires R ≥ 4.3 with `data.table`, `copula`, `ggplot2`, `scales`, `jsonlite`. Optional: `pcaPP` (fast Kendall), `hexbin`, `energy`, `segmented`. Quarto ≥ 1.4 to render the deck.

Student-level pairs live in `cache/pairs.rds` (gitignored). Slim ILEARN/WIDA extracts are also cached there so the 10-million-row LONG is loaded once.

### Pipeline

```
00  scale audit          outputs/audit_*.csv, data_dictionary.csv
01  ingest + pair        cache/pairs.rds (gitignored); outputs/pair_manifest.csv
02  N counts             outputs/n_*.csv; figures/02_n_heatmap.*
03  global dependence    outputs/dep_*.csv; figures/03_pobs_facets_<year>.*
04  local / threshold    outputs/local_*.csv; figures/04_*
05  exit criterion       outputs/exit_*.csv; figures/05_exit_youden_<year>.*
06  extensions           outputs/ext_*.csv; figures/06_*
07  exiters              cache/exiters.rds (gitignored); outputs/exiter_*.csv; figures/07_*
08  lagged proficiency   cache/lagged.rds (gitignored); outputs/lagged_*.csv; figures/08_*
09  copula asymmetry     outputs/asym_*.csv; figures/09_copula_asymmetry_*; figures/05b_youden_explainer
    manifests            outputs/manifests/{index,results}.json and tables/*.json
```

Each numbered script can be `source()`’d on its own. `config.R` plus `R/` are the shared layer.

---

## Containment / FERPA

- Source LONGs are read-only and never copied into this directory.
- `cache/` is gitignored. It holds student-level pairs and slim extracts.
- `outputs/` and `figures/` are aggregates only. `write_output()` and `write_json()` refuse columns named `ID` / `STUDENT_ID` / `STN`.
- No student identifiers are printed to the console or written to committed files.
- Cells with reporting N below 10 are **dropped** from committed tables (`redact_small_n()` / `MIN_N_SUPPRESS`). Thin sides of a split (e.g. n_below) are blanked. `suppress` is not a substitute for dropping the count.

---

## Directory

```
IN_ILEARN_WIDA_ACCESS_analysis_2026/
  README.md
  .gitignore
  config.R
  run_all.R
  00_scale_audit.R
  01_ingest_and_pair.R
  02_n_counts.R
  03_global_dependence.R
  04_local_dependence.R
  05_exit_criterion.R
  06_extensions.R
  07_exiters.R
  08_lagged_proficiency.R
  09_copula_asymmetry.R
  docs/           # GitHub Pages reveal.js deck (renders to docs/index.html)
  R/
    io.R pairing.R ranks.R copula.R threshold.R plots.R asymmetry.R plots_asym.R
  cache/          # gitignored
  outputs/        # aggregate csv
  figures/        # pdf + png
```

---

## Results

First full run (2026-08-27). All numbers below are aggregates from `outputs/`. A change-point or a Youden-optimal cut is **not** a recommended exit number. It describes where ILEARN ELA starts to move with WIDA ACCESS Overall.

### N

**332,633** same-year, same-grade pairs after the floor-score filters (213,551 in the 2021–2025 primary window). Planning-time reconnaissance had 340,463 before those filters.

| Year | Stratum | Grade 3 | 4 | 5 | 6 | 7 | 8 |
|---|---|---:|---:|---:|---:|---:|---:|
| 2021 | primary | 7,849 | 7,468 | 6,185 | 5,263 | 4,988 | 3,835 |
| 2022 | primary | 8,045 | 8,289 | 6,822 | 5,725 | 5,733 | 5,434 |
| 2023 | primary | 8,714 | 8,381 | 7,246 | 6,143 | 6,187 | 6,101 |
| 2024 | primary | 9,074 | 9,211 | 7,700 | 6,921 | 6,807 | 6,838 |
| 2025 | primary | 9,821 | 9,187 | 8,082 | 6,919 | 7,419 | 7,164 |
| 2026 | ranks only | 10,088 | 8,515 | 7,421 | 6,660 | 6,850 | 7,115 |

WIDA pair rate in grades 3–8 is near 1. L6 cells are thin (hundreds, not thousands) and are flagged when they fall below the slice gate. See `figures/02_n_heatmap.png`.

ILEARN proficiency labels are `Below` / `Approaching` / `At` / `Above Proficiency`. EL status is usable in 2019–2025 (`Limited English Proficient` vs `Not an English Language Learner`); it is missing in 2026 and was mixed with meal codes in some earlier years.

### Global dependence

Primary-window Kendall’s τ is **0.50–0.61** (median 0.55); Spearman 0.69–0.81. The two tests are **not** independent overall. Confirmatory copula AIC selected Frank in 46% of year × grade cells, t in 28%, Gaussian in 26% — consistent with weaker tails than the typical same-test longitudinal pair.

The signal-vs-noise claim is therefore a **local** claim, not a global one -- and, in copula terms, a **radial-asymmetry** claim (step 09).

### Copula asymmetry (step 09)

The confirmatory families (t / Frank / Gaussian) are exchangeable and **radially symmetric**: by construction they force `lambda_lower = lambda_upper` and treat the both-low corner (the WIDA floor, the noise region) as a mirror of the both-high corner. The signal-vs-noise hypothesis is exactly the claim that those corners differ, so step 09 measures the departure from symmetry directly (empirical, rank-based -- no re-fitting).

The dependence is **exchangeable** (swapping the two tests barely moves the copula; index ~0.006) but clearly **radially asymmetric**. Upper-tail concordance runs above lower-tail: `lambda_upper - lambda_lower` rises from ~0.11 (2021) to ~0.22 (2025), stable in sign and strengthening. The both-high "signal corner" couples more tightly than the both-low "noise corner" -- a second, independent signature of the access story, and precisely the structure the symmetric fits erase. See `outputs/asym_*.csv`, `figures/09_copula_asymmetry_surface.png` (the D(u,v) map), `figures/09_copula_asymmetry_tails.png` (lambda_U vs lambda_L), and `figures/09_copula_asymmetry_stats.png`.

A **Youden explainer** figure (`figures/05b_youden_explainer.png`) accompanies the exit-criterion story: it shows Youden's J as the ROC height above chance and why the J-optimal cut sits near 4.3 while sensitivity collapses at 5.0.

### Where the signal appears (2025, typical of the primary window)

| WIDA level | N | Within-level τ | Median ILEARN percentile | P(ILEARN proficient) |
|---|---:|---:|---:|---:|
| 1 | 6,048 | 0.03 | 17 | 0.00 |
| 2 | 7,057 | 0.10 | 24 | 0.00 |
| 3 | 16,575 | 0.22 | 49 | 0.02 |
| 4 | 5,582 | 0.09 | 70 | 0.10 |
| 4.3 (provisional) | 9,220 | 0.14 | 81 | 0.24 |
| 5 (auto-exit) | 3,610 | 0.19 | 93 | 0.60 |
| 6 | 500 | 0.20 | 94 | 0.74 |

L1–L2: ILEARN is on the floor and within-band ranks do not move together. That is the noise region. From L3 the ILEARN distribution walks up the grade; by L4.3 paired EL students sit around the 80th percentile of the grade, and by L5 around the 93rd. Within-band τ stays modest the whole way (restriction of range). The signal is the **location** of ILEARN, not a sudden jump in local correlation.

τ(u) change-points in the primary window have median WIDA PL **3.95** (IQR 3.6–4.3). They are noisier than the level table and should not be treated as a single cut.

### Q1. Does a number exist, and is it the same by grade?

A **region** exists, not one number. Youden-optimal WIDA cuts against ILEARN proficiency in 2021–2025 have median **4.05** (range 3.8–4.7). By grade:

| Grade | Median optimal cut (5 years) | SD |
|---|---:|---:|
| 3 | 3.9 | 0.04 |
| 4 | 4.5 | 0.09 |
| 5 | 4.6 | 0.16 |
| 6 | 3.9 | 0.06 |
| 7 | 4.1 | 0.09 |
| 8 | 4.0 | 0.10 |

Grades 4–5 sit near the start of the **4.3** provisional band; 3 and 6–8 sit a little below it. None of the Youden optima are at **5.0**. Youden at 4.3 beats Youden at 5.0 in every primary cell — 5.0 is too rare to be a good classifier of ILEARN proficiency (sensitivity collapses).

That is **not** an argument that 5.0 is the wrong legal exit. ILEARN proficiency is a high bar for EL students: even among those who first reach 5.0, only about **40%** are ILEARN-proficient that year (`ext_panel_first_crossing.csv`). 5.0 marks students who are already high in the ILEARN distribution (median ~93rd percentile), not the first appearance of signal.

### Q2. Consistent across years?

Yes in the primary window, and 2026 agrees on the rank scale.

- Global τ is stable across 2021–2025 (0.50–0.61) and 2026 (0.54–0.61).
- 2026 native vs OLD-scale ranks give the same Kendall τ to two decimals (`ext_2026_dual_scale.csv`). The scale reset does not change the dependence story.
- 2026 ILEARN achievement levels in the combined LONG do not support the proficiency classifier (Youden on “proficient” is NA). Within-year percentiles still work, as planned.
- 2019 (first ILEARN year) and 2017–2018 (ISTEP) show the same pattern at slightly different optimal cuts (median 4.2). Treat them as contrasts, not as ILEARN evidence.

### Q3. Other readings

- **Math vs ELA.** Primary-window Kendall τ is lower for Mathematics (median 0.45) than for ELA (0.55). The ELA threshold is sharper, which is what a language-access story predicts.
- **Never-EL reference.** In 2025, paired EL ILEARN medians sit 148 scale-score points below the never-EL median at WIDA L1 and **cross the never-EL median between L4.3 and L5** (gap −19 at L4.3, +25 at L5).
- **Lagged pairing** (WIDA t−1 → ILEARN t) is only slightly weaker than same-year (median τ 0.52 vs 0.55). The access claim is not an artifact of same-spring pairing.
- **First crossing.** Students first reaching 4.3: ILEARN percentile 59 → 72, P(proficient) 0.13 → 0.23. First reaching 5.0: 77 → 86, P(proficient) 0.38 → 0.40. Most of the ILEARN location gain has already happened by the time a student hits auto-exit.
- **SGP vs SGP** (secondary). Growth-on-growth Kendall τ is only ~0.15. Status, not growth, is where the access signal lives.

### How this bears on an exit criterion

1. ILEARN ELA is not informative about English access below WIDA **Level 3**. L1–L2 is noise in the sense of the hypothesis.
2. Signal is clearly present by the **4.3–4.9** provisional band (ILEARN around the 80th percentile of the grade). That is also where Youden against ILEARN proficiency peaks.
3. **5.0 auto-exit** selects students who are already at the top of the ILEARN distribution. It is a conservative access standard, not the first point at which ILEARN stops looking like noise.
4. The empirical region is **not identical by grade** (4–5 run higher than 3 and 6–8) but it is **stable across 2021–2025 and 2026 ranks**.
5. None of this replaces the ILP-committee pathway. The data say the committee band is where the content test begins to look like a content test.

---

## Exiter cohort (ILEARN after WIDA disappears)

Complementary to the same-year and first-crossing results above. Those look at students who **stay** in the paired sample. Exit is the complementary event: ILEARN + WIDA ACCESS Overall at *t*, ILEARN only at *t+1*. That is the first time these two files can speak to what happens after ACCESS stops, which is the claim an exit criterion actually makes.

These LONGs do not carry an official IDOE exit roster. The cohort is a **testing-pattern exit**, not a legal exit. Every table below uses that definition. Grade 8 at *t* cannot be followed (no grade 9 ILEARN) and is counted as attrition, not exit. `<4.3` disappearances are documented, not treated as policy exits.

### Definition

For each adjacent ILEARN pair `(t, t+1)` and grade advance `(g, g+1)` with *g* in 3–7:

| Role | Pattern |
|---|---|
| **Exiter** | Both tests at *(t, g)*; ILEARN at *(t+1, g+1)*; **no** WIDA at *t+1* in any grade |
| **Stayer** | Both tests at *t* and both at *t+1* |
| **Never-EL** | ILEARN both years, no WIDA either year, `EL_STATUS` not Limited English Proficient where the flag is usable (2019–2025) |
| **Attritor** | Both tests at *t*, no ILEARN at *t+1* (mobility, grade 8, 2020 hole) |

Last-WIDA band at *t* (`WIDA_PL`): `<4.3` (unexpected disappearance), `4.3–4.9` (committee pathway), `>=5.0` (auto-exit).

Legal windows: 2021→2022 through 2024→2025 (primary; stable ILEARN scale, EL flag usable). 2025→2026 is ranks / within-year percentiles only. 2018→2019 and 2019→2021 are not used.

Student-level panel: `cache/exiters.rds` (gitignored). Aggregates: `outputs/exiter_*.csv`. Figures: `figures/07_exiter_*.png`.

### N

**14,386** inferred exiters in the primary window (grades 3–7). Almost all of them left ACCESS from `>=5.0` (12,568). The committee band is rare until 2024.

| *t* → *t+1* | Exiters | of which `<4.3` | `4.3–4.9` | `>=5.0` | Stayers | Attritors |
|---|---:|---:|---:|---:|---:|---:|
| 2021 → 2022 | 2,920 | 204 | 57 | 2,659 | 27,439 | 5,229 |
| 2022 → 2023 | 3,551 | 197 | 37 | 3,317 | 29,569 | 6,928 |
| 2023 → 2024 | 3,462 | 221 | 69 | 3,172 | 31,530 | 7,780 |
| 2024 → 2025 | 4,453 | 211 | 822 | 3,420 | 33,251 | 8,847 |
| 2025 → 2026 (ranks) | 5,047 | 231 | 1,197 | 3,619 | 33,406 | 10,139 |

Stayers at `>=5.0` are almost nonexistent (197 across the whole primary window). Auto-exit is doing what it says: students at 5.0 do not keep taking ACCESS. `<4.3` “exiters” stay in the hundreds every year — mobility or missed ACCESS, not a policy pathway. See `figures/07_exiter_n_heatmap.png`.

### EL-status robustness (2019–2025)

Share of inferred exiters whose ILEARN `EL_STATUS` moves from Limited English Proficient at *t* to Not an English Language Learner at *t+1*:

| Last WIDA at *t* | Typical P(LEP → not EL) | Typical P(still EL) |
|---|---:|---:|
| `>=5.0` | 0.89–0.95 | 0.03–0.07 |
| `4.3–4.9` | 0.20–0.37 | 0.53–0.76 |
| `<4.3` | 0.14–0.23 | 0.70–0.75 |

The `>=5.0` testing-pattern matches the official classification. The `4.3–4.9` and `<4.3` patterns do **not**: most of those students are still classified EL the next year. They are ACCESS non-takers, not confirmed exits. The 2024 jump in `4.3–4.9` disappearances (822) still has only 23% classification agreement. Treat the committee-band “exiter” contrast as thin and contaminated.

### Achievement before vs after (primary window)

Paired ILEARN within-year percentile and proficiency, same students. Never-EL median percentile in this window is about 54.

| Role | Last WIDA | N | Pctile *t* | Pctile *t+1* | Δ | P(prof) *t* | P(prof) *t+1* |
|---|---|---:|---:|---:|---:|---:|---:|
| Exiter | `<4.3` | 833 | 16 | 17 | +1 | 0.04 | 0.05 |
| Exiter | `4.3–4.9` | 985 | 57 | 55 | −2 | 0.45 | 0.44 |
| Exiter | `>=5.0` | 12,568 | 63 | 64 | +1 | 0.55 | 0.58 |
| Stayer | `<4.3` | 95,717 | 18 | 20 | +2 | 0.04 | 0.06 |
| Stayer | `4.3–4.9` | 25,875 | 45 | 46 | +2 | 0.26 | 0.31 |
| Stayer | `>=5.0` | 197 | 64 | 66 | +2 | 0.58 | 0.57 |
| Never-EL | — | 1,415,373 | 54 | 54 | 0 | 0.44 | 0.45 |

No post-exit collapse. `>=5.0` leavers sit ~9–10 percentile points **above** the never-EL median and hold or rise. `4.3–4.9` leavers sit near never-EL and dip about 2 points; they also start higher than `4.3–4.9` **stayers** (57 vs 45), so the ones who disappear from ACCESS are already the stronger content students in that band. `<4.3` disappearances stay on the ILEARN floor, like `<4.3` stayers. See `figures/07_exiter_percentile.png`.

### Exit-year ILEARN SGP

SGP at *t+1* is growth during the first year without ACCESS.

| Role | Last WIDA | N | Median SGP | P(SGP < 35) | P(SGP > 65) |
|---|---|---:|---:|---:|---:|
| Exiter | `<4.3` | 833 | 41 | 0.44 | 0.26 |
| Exiter | `4.3–4.9` | 985 | 50 | 0.33 | 0.34 |
| Exiter | `>=5.0` | 12,568 | 58 | 0.27 | 0.42 |
| Stayer | `<4.3` | 95,717 | 45 | 0.39 | 0.30 |
| Stayer | `4.3–4.9` | 25,875 | 54 | 0.30 | 0.38 |
| Stayer | `>=5.0` | 197 | 57 | 0.28 | 0.39 |
| Never-EL | — | 1,415,373 | 50 | 0.34 | 0.35 |

`4.3–4.9` leavers post a typical 50. `>=5.0` leavers post 58 — a right shift, not a left one. `<4.3` disappearances are the only left-shifted group (median 41), which is what an unexpected ACCESS drop among still-low students looks like. See `figures/07_exiter_sgp.png`.

### What this says about 4.3 vs 5.0

Together with the same-year level table (L1–L2 = floor; L4.3 ≈ 80th percentile while still tested; L5 ≈ 93rd):

1. Students who actually leave ACCESS from `>=5.0` hold their ILEARN location, stay above the never-EL median, and post slightly-above-typical growth. The classification flag agrees they exited. There is no post-exit academic collapse in this band.
2. Students who disappear from ACCESS at `4.3–4.9` also hold location and post ~50 SGP — **but** the cell is small until 2024, and most of them are still classified EL. The committee-band contrast is not a clean “official exit” sample.
3. The threshold debate, on these files, is about **who is allowed to leave**, not about collapse after services stop. Same-year copulas already showed 4.3 is where ILEARN starts to look like a content test and 5.0 selects the top of the distribution. The exiter panel does not overturn that: 5.0 leavers are higher and grow a bit faster; 4.3 leavers who remain in ILEARN do not fall apart. It also cannot, by itself, certify the committee pathway, because most 4.3 disappearances are not confirmed exits.

Do not treat `cache/exiters.rds` as IDOE’s official exit list.

---

## Lagged 50-50 proficiency cut (contrast only)

A common recipe for setting an ACCESS cut is: find the WIDA Overall score at which a student has better than a 50-50 chance of being ILEARN ELA-proficient **the following year**. The joint is a copula $C(U_t^{\mathrm{WIDA}}, V_{t+1}^{\mathrm{ILEARN}})$. Let $v^*$ be the ILEARN proficiency cut on the paired rank scale; invert $P(V \ge v^* \mid U = u) = 1/2$ and map $u^*$ back to a WIDA proficiency level. The empirical sibling is the first local window ($\pm 0.15$ PL) whose P(proficient at $t+1$) crosses 0.50.

**This is not a recommended exit rule.** Never-EL students — people who never took ACCESS — are not all ILEARN-proficient. In the primary window their P(proficient) is **0.43**. A 50-50 content-test bar is harder than the never-EL average. ILEARN proficiency is not a proxy for English access.

**168,693** lagged pairs in the primary window (WIDA grades 2–7 → ILEARN grades 3–8). 2025→2026 is excluded from the inversion (proficiency classifier is NA).

| Quantity | Value |
|---|---:|
| Pooled copula cut (WIDA PL at *t*) | **4.8** (Gaussian; reached) |
| Pooled local-empirical cut | **5.1** |
| P(prof *t+1* \| WIDA ≥ 4.3) | 0.42 |
| P(prof *t+1* \| WIDA ≥ 5.0) | 0.59 |
| Local P(prof *t+1* \| WIDA ≈ 4.3) | 0.27 |
| Local P(prof *t+1* \| WIDA ≈ 5.0) | 0.48 |
| Never-EL P(ILEARN proficient) | 0.43 |

The recipe rediscovers something near **5.0**. That is what you get when you ask ACCESS to predict a hard content outcome. Grades 3–4 invert lower (about 4.0–4.5); grades 6–7 often never reach 50% even at the top of the observed WIDA range. See `outputs/lagged_fifty_fifty_cut.csv`, `figures/08_lagged_p_prof_primary.png`, and `figures/08_lagged_fifty_fifty_by_grade.png`.

---

## JSON manifests

Every aggregate CSV is also written as JSON under `outputs/manifests/` for secondary analysis and reporting. No student identifiers.

| File | Role |
|---|---|
| `outputs/manifests/index.json` | Catalog of tables and figures |
| `outputs/manifests/results.json` | Curated nested findings (`in.ilearn.wida.results.v1`) — what the deck reads |
| `outputs/manifests/tables/*.json` | Row-oriented copy of each `outputs/*.csv` |

`write_all_manifests()` runs at the end of `run_all.R` (and at the end of step 08). The GitHub Pages deck in `docs/` consumes `results.json` only.

---

## Presentation (GitHub Pages)

`docs/` is a reveal.js deck in the same template as `DBetebenner/Rhode_Island_082726_Presentation` (Josefin / Noto, 1920×1080, click-to-enlarge figures). It renders to `docs/index.html`.

```bash
./docs/render.sh
```

Set the GitHub Pages source to **Deploy from a branch → `/docs`**. This directory is not yet a git repository on disk; initialize and push when you want Pages live. The HTML is self-contained (`embed-resources: true`) and does not read `cache/`.

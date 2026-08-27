# Technical review — ILEARN × WIDA ACCESS signal-vs-noise pipeline

Reviewer pass covering (1) framing of purpose and research questions, (2) correctness and
completeness of the R pipeline, and (3) the presentation. Additions made in this pass —
a copula-asymmetry step, a Youden explainer, and figure-legibility fixes — are described in
the last section.

---

## 1. Purpose and research questions — is the point clear?

**Verdict: the scientific framing is strong and unusually disciplined; the *exposition* was
thin, and that is a presentation problem, not a design problem.**

The hypothesis is stated crisply and, importantly, *falsifiably*: ILEARN ELA behaves like
noise at low WIDA (floor pile-up, near-independence of ranks) and like signal at higher WIDA,
and the transition — if real — is *evidence about* where an exit criterion sits, explicitly
**not** the legal rule. The repeated separation of "where the content test starts to look
like a content test" from "who is allowed to leave EL services" is the single best thing
about the framing. It keeps the analysis honest: WIDA measures English-language proficiency,
ILEARN measures grade-level content, native English speakers fail ILEARN, and a content-test
cut is not a language-access cut. That guardrail is stated in the README, the config, and the
deck, and every downstream claim respects it.

The three research questions are well posed and, on the whole, well answered:

- **Q1 (does a number exist, same by grade?)** — answered as "a *region*, not a number"
  (Youden-optimal 3.8–4.7, grade medians differing), which is the correct and defensible
  reading. Good.
- **Q2 (consistent across years?)** — answered with the rank-scale invariance argument and the
  2026 dual-scale check. This is the most rigorous part of the design: making ranks the
  primary metric so the 2026 scale reset cannot contaminate the estimand is exactly right.
- **Q3 (other readings)** — genuinely comprehensive: Math contrast, never-EL reference, lagged
  pairing, first crossing, SGP-vs-SGP, the exiter cohort, and the 50-50 recipe are each framed
  as a distinct piece of evidence with its own caveats.

Where the framing was under-served: a reader who is *not* already inside the author's head has
to work to extract the throughline. The README is excellent as a lab notebook but reads as a
catalog of results; the deck (see §3) states conclusions faster than it motivates them. The
substance is all there — it needed a clearer "why each analysis exists and what would have
refuted it" spine. That is fixable, and the additions below add one more load-bearing beam to
that spine (the asymmetry lens) plus a proper explanation of Youden.

---

## 2. R pipeline — correctness and completeness

<CODE_REVIEW>

**Architecture.** `config.R` + `R/` shared layer + numbered idempotent steps is a clean,
reproducible design. Each step can be `source()`-d alone; the pairs cache decouples the
expensive load from the analysis; `run_all.R` sequences them. Naming is consistent and the
roxygen coverage on the `R/` helpers is good. Rank-first methodology is applied consistently
(`R/ranks.R`, pseudo-observations within (year, grade)).

**Correctness — nothing that invalidates a headline result, several things worth fixing:**

- **(Conceptual, the important one) The confirmatory copula families cannot represent the
  hypothesis.** `COPULA_FAMILIES = c("t","frank","gaussian")` are all *exchangeable* and
  *radially symmetric*. Two structural consequences: (a) they force `lambda_lower == lambda_upper`
  (t) or both to 0 (Frank/Gaussian), so the `lambda_lower`/`lambda_upper` columns extracted in
  `R/copula.R::fit_one_copula()` are **structurally uninformative** about tail asymmetry — they
  report the family's assumption, not the data; and (b) they treat the both-low corner (the
  WIDA floor / noise region) as a mirror of the both-high corner (signal). The signal-vs-noise
  claim *is* a claim that those corners differ. So the global copula layer, by construction,
  smooths away the very structure the project is about. This is not a bug in the code — the
  fits are correct — but it is a gap in the analysis. Addressed in §4 by a new asymmetry step
  that measures the departure from symmetry directly.

- **(Bug, fixed) Exiter-heatmap band ordering.** `wida_exit_band()` correctly returns an
  ordered factor `c("<4.3","4.3-4.9",">=5.0")`, but in `07_exiters.R::build_transition()` the
  never-EL rows set `WIDA_EXIT_BAND := NA` (logical), and the subsequent `rbindlist(..., fill=TRUE)`
  coerces the ordered factor to **character**. By the time `plot_exiter_n()` receives it the
  level order is lost, so the committed `07_exiter_n_heatmap` renders the bands as
  `<4.3, >=5.0, 4.3-4.9` — `>=5.0` in the middle. Fixed by restoring the ordered factor inside
  `plot_exiter_n()` (localized, safe); the root-cause note is in the code comment.

- **(Bug, latent, deck) `role == role` masking in `docs/…qmd`.** The setup helpers
  `ex_band <- function(role, band) { … ex_ach[role == role & WIDA_EXIT_BAND == band] … }` and
  `sgp_band` name their first parameter `role`, identical to the data.table column. Inside the
  `[` the bare `role` resolves to the *column*, so `role == role` is always `TRUE` and the
  filter degenerates to band-only. These helpers are **defined but never called** (the slides
  filter with `ex_ach[role == "exiter", …]` directly), so no rendered number is wrong — but the
  dead code should be deleted or the parameter renamed (`which_role`) before it gets used.
  (Note `lev_row(label)` is *correct* precisely because its parameter name does not collide
  with any column.)

- **(Minor) Distance correlation is advertised but never computed.** `dependence_measures(..., dcor=FALSE)`
  is always called with `dcor=FALSE` (step 03), and `HAS_ENERGY` is probed but unused for a
  result. Either wire it in as a nonparametric cross-check (it would be a nice independent
  corroboration of the τ story) or drop the scaffolding.

- **(Minor, robustness) `tau_changepoint()` grid search** fits an `lm` per candidate breakpoint
  over the τ(u) curve and is fine, but the README already flags the change-points as noisy
  (median PL 3.95, IQR 3.6–4.3) and rightly leans on the level table / Youden instead. Consider
  demoting the change-point to a diagnostic and foregrounding the (more stable) Youden region —
  which the deck effectively already does.

- **(Style) Redaction coupling.** `redact_small_n()` hardcodes the family of `n_*`/stat column
  pairs. It is careful and correct for the current tables, but every new table with a new count
  column silently escapes side-redaction unless its column is added. The new asymmetry tables
  use `n` (covered by the primary-N drop) and non-count grids (no rows to redact), so they are
  safe, but the pattern is worth a comment for future steps.

**Completeness against the research questions:** good. Q1/Q2/Q3 are each backed by dedicated
outputs and figures, the year strata are handled with the right caution (2019 replicate, 2026
ranks-only, ISTEP contrast, 2020 hole), and the exiter cohort (step 07) genuinely adds the
"after ACCESS stops" evidence that an exit criterion actually speaks to. The one missing lens
was the dependence-structure asymmetry (now added). Nothing else material is absent.

</CODE_REVIEW>

<SECURITY_REVIEW>

FERPA/containment is handled well and deliberately: source LONGs are read-only and never copied
in; `cache/` is gitignored and holds the only student-level artifacts; `write_output()` /
`write_json()` refuse `ID`/`STUDENT_ID`/`STN` columns; `redact_small_n()` drops cells below
`MIN_N_SUPPRESS`. The de-identification used for this review pass (a rank/level extract with the
`ID` column removed *before* anything left the source machine) is consistent with that posture —
no identifiers were moved. One forward note: the manifest/JSON path (`R/manifest.R`) should
inherit the same column guard as `write_output()` for any future nested field that could carry a
raw score keyed to a small cell; today's aggregates are safe.

</SECURITY_REVIEW>

---

## 3. Presentation — why it read "thin," and what changed

The deck's problem was not accuracy; it was **motivation-per-slide**. It tends to assert a
finding and move on, so a semi-technical reader (an ILP committee member, an IDOE analyst)
gets the "what" without enough "why should I believe this / why does it matter." The copula
work in particular was doing heavy lifting under the hood (τ, family selection, tail
dependence) while the slides only surfaced a single global τ number, which *undersells* the
most interesting thing in the data.

Two concrete gaps drove most of the thinness:

1. **The copula was described but never *shown* to be doing anything.** The whole point of a
   copula is the dependence *shape*, and the shape here is asymmetric in a way that maps
   one-to-one onto the hypothesis. Showing that asymmetry turns "we fit some copulas" into "the
   dependence structure itself carries the signal."
2. **Youden was used as the workhorse of Q1 but never explained.** The single most consequential
   number in the deck (the ~4.1 optimal cut, and the claim that 5.0 collapses sensitivity) rests
   on a statistic the audience was never introduced to.

Both are addressed below.

---

## 4. Additions made in this pass

<PLANNING>

**A. Copula asymmetry (new step 09 + `R/asymmetry.R` + figures).** Everything is empirical /
rank-based — no MPL re-fitting — so it is fast and inherits the pipeline's scale-invariance.

- `R/asymmetry.R`: empirical copula on a grid; the radial-asymmetry surface
  `D(u,v) = C_n(u,v) − C_n^refl(u,v)` (0 everywhere iff radially symmetric); nonparametric
  tail-dependence functions `lambda_L(t), lambda_U(t)`; and scalar indices (radial `sup|D|`,
  radial CvM, exchangeability `sup|C(u,v)−C(v,u)|`, tail asymmetry `lambda_U−lambda_L`, and a
  both-high-minus-both-low corner-τ gap).
- **Finding:** the copula is **exchangeable** (index ≈ 0.006 — swapping the two tests barely
  moves it) but clearly **radially asymmetric**. Upper-tail concordance runs well above
  lower-tail: `lambda_U − lambda_L` grows from ~0.11 (2021) to ~0.22 (2025), stable in sign and
  strengthening. The both-high "signal corner" is more tightly coupled than the both-low "noise
  corner." This is a *second, independent* signature of the access story — and it is exactly the
  structure the t/Frank/Gaussian fits erase, which is why it was invisible before.
- Figures: `09_copula_asymmetry_surface` (the D(u,v) map), `09_copula_asymmetry_tails`
  (`lambda_U` vs `lambda_L`), `09_copula_asymmetry_stats` (indices by year). Two deck slides plus
  a section divider explain and show it.

**B. Youden explainer (figure `05b_youden_explainer` + two deck slides).** Panel A shows the ROC
geometry with J as the vertical height above chance at the optimal cut; panel B shows
sensitivity, specificity, and J across candidate cuts with the optimum and the 4.3 / 5.0 anchors.
The slides define sensitivity/specificity/J, show why J peaks near 4.3 and collapses at 5.0
(sensitivity falls off), and make the policy point that J weights false-exit and false-hold
equally — so a loss-averse policy can rationally sit *above* the J-optimal cut, and, more
importantly, that J optimizes agreement with *ILEARN proficiency*, which is the wrong construct
for an English-access decision.

**C. Figure legibility (`R/plots.R`).** The count heatmaps now use a sequential fill with a
per-cell **adaptive label colour** (white on dark cells, near-black on light cells, chosen by
relative luminance), so labels stay readable across the whole range instead of assuming the
scale is light. The exiter-heatmap band-ordering bug (above) is fixed in the same file. Both
`02_n_heatmap` and `07_exiter_n_heatmap` were regenerated.

</PLANNING>

**To regenerate everything:** `Rscript run_all.R` now runs steps 00–09; `Rscript run_all.R 9`
runs the asymmetry step and the Youden explainer alone (it reads `cache/pairs.rds` and, for the
Youden figure, `outputs/exit_agreement_by_cut.csv` from step 05). Then `./docs/render.sh`
rebuilds the deck.

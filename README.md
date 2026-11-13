# Search results deduplication pipeline

Reproducible deduplication of the systematic review search results from
Scopus, Web of Science, IEEE Xplore and the ACM Digital Library, using
[ASySD](https://github.com/camaradesuk/ASySD) (Hair et al., 2023).

The repository contains **code only**. Raw database exports and generated
outputs are not tracked in git.

## Running it

Requirements: R (≥ 4.4; developed on 4.6.2).

```sh
# once: install the exact package versions recorded in renv.lock
Rscript -e 'renv::restore()'

# every time
Rscript run_pipeline.R
```

The raw exports are read from `../Search Results` (set in `config.yml`):

| Source | Folder | Format |
|---|---|---|
| Scopus | `Scopes/*.ris` | RIS |
| Web of Science | `Web of Science/*.txt` | WoS tagged ("Plain text file", full record) |
| IEEE Xplore | `IEEE/*.csv` | CSV |
| ACM DL | `acm/*.txt` | ACM Reference Format plain text |

The enrichment step (below) saves its online lookups to
`../Search Results/enrichment_cache/enrichment_cache.csv`. Treat this file
as part of the raw data: share it together with the exports.

`output/input_manifest.csv` records a SHA-256 checksum of every raw file and
the enrichment cache, so anyone rerunning the pipeline can confirm they have
the same inputs. Given the same raw files, the same enrichment cache and the
same `decisions/manual_decisions.csv`, the pipeline produces byte-identical
results.

## Outputs (`output/`)

| File | Contents |
|---|---|
| `deduplicated_library.ris` / `.bib` / `.csv` | One record per unique study report, ready for the screening tool |
| `summary.txt`, `prisma_counts.csv` | Counts for the PRISMA flow diagram |
| `record_log.csv` | Every original record → the group it belongs to and whether it was kept as the primary record |
| `matched_pairs.csv` | Every link used to merge records, with how it was made (`auto`, `exact`, `manual`) and ASySD similarity scores |
| `manual_review_pairs.csv` | Possible duplicates that need a human decision |
| `enrichment_log.csv` | For every enriched record: how it was looked up, the outcome, which fields were filled and from which service; for title searches, the candidate title, DOI and similarity score |
| `groups_with_doi_conflicts.csv` | Groups that still contain different DOIs (should normally be empty) |
| `input_manifest.csv`, `session_info.txt` | Checksums of inputs; R and package versions |

## How deduplication works

1. **Import** (`R/01_import.R`). Each export is read into the same fields:
   author, year, title, journal/venue, volume, issue, pages, DOI, ISSN/ISBN,
   abstract. Every record gets a stable ID such as `Scopus-00042` (source +
   position, files read in alphabetical order). The ACM export is formatted
   citations rather than structured data, so ACM fields are recovered with
   pattern matching and the export contains no abstracts (see step 3).
2. **Clean** (`R/02_clean.R`). HTML entities and tags are removed, DOIs are
   reduced to the bare `10.xxxx/...` form, and author names are rewritten in
   one style (`Surname AB, ...`, accents removed) so that "H. Talat",
   "Talat, H." and "Talat, H" compare as equal. The original author names
   are kept for the exported library.
3. **Enrich** (`R/03_enrich.R`). For the sources listed in `config.yml`
   (by default ACM), missing fields are fetched online:
   - *Records with a DOI* are looked up in [OpenAlex](https://openalex.org)
     (in batches of 50). If OpenAlex has no abstract, [Crossref](https://www.crossref.org)
     is tried. Empty abstract, volume, issue, pages and venue fields are filled.
   - *Records without a DOI* are searched by title in OpenAlex (year ± 1).
     The best result is accepted only if the titles are near-identical
     (similarity ≥ 0.95, i.e. 1 − edit distance / length), the year is
     within 1, and the first author's surname is among its authors. An
     accepted match supplies **only the abstract**. A title match can be a
     different version of the paper (e.g. a repository copy with its own
     DOI), so its DOI is logged but not used.
   - Values from the original export are never overwritten. Whole-proceedings
     records are not looked up.
   - Every lookup (including "not found" and rejected matches) is saved in
     the cache file, and later runs read the cache instead of querying
     again, so results do not change as the online databases are updated.
     Set `fetch: false` in `config.yml` to run fully offline from the cache.
     Records not yet in the cache are looked up on the next run with
     `fetch: true`.
   - Optional environment variables: `OPENALEX_API_KEY` (a free key from
     openalex.org gives your own daily allowance instead of the shared one)
     and `CONTACT_EMAIL` (sent as `mailto` to OpenAlex and Crossref).
4. **Match with ASySD** (`R/04_deduplicate.R`). ASySD upper-cases and strips
   punctuation, then finds candidate pairs that agree *exactly* on blocking
   fields (title + author, title + pages, DOI, author + year + pages, ...).
   Each candidate pair is scored field by field with Jaro–Winkler similarity
   (0–1), and accepted as a duplicate only if a rule over several fields is
   met. For example: title > 0.9, author > 0.75 and DOI > 0.95, or title,
   author, abstract and journal all > 0.9. Pairs that are similar but fail
   every rule become "possible" pairs for manual review.
   - Whole-proceedings records exported by ACM (e.g. "Proceedings of the
     2024 ... Conference", no authors) are not compared with articles,
     because ASySD would pair every one of them with every other. They only
     match each other on identical title and year.
   - **DOI safeguard.** ASySD's own check for conflicting DOIs never takes
     effect (its condition `(journal|isbn > 0.9)` is always true), so it
     can merge a conference paper with its journal version. Here, any
     accepted pair whose DOIs differ is sent to manual review instead.
     DOIs count as the same if they are identical, differ only in the
     registrant prefix, or differ by ≤ 3 characters (WoS DOIs often contain
     typos such as `10.11445/` for `10.1145/`).
5. **Group and merge** (`R/05_groups.R`). Records are nodes and accepted
   pairs are links; each connected set of records is one group (if A = B
   and B = C, then A, B and C are one study report). Manual decisions add
   (`match`) or remove (`no_match`) links. Each group becomes one record:
   the base is the record from the highest-priority source
   (Scopus > WoS > IEEE > ACM, set in `config.yml`), and any empty field is
   filled from the other records in source-priority order. The group ID is
   the base record's ID and `record_ids` lists all merged records.
6. **Report** (`R/06_report.R`). Writes the library, logs and counts.

Conference papers and their extended journal versions have different DOIs,
so they are kept as separate records. Linking multiple reports of the same
study is done at screening, not deduplication (Cochrane Handbook ch. 4).

## Manual review

1. Run `Rscript run_pipeline.R`.
2. Open `output/manual_review_pairs.csv`. For each row, compare the two
   records and type `match` or `no_match` in the `decision` column
   (optionally fill `reviewer` and `note`). Rows left blank stay
   undecided; undecided pairs are **not** merged.
3. Run `Rscript save_decisions.R`. This copies the decisions (IDs and
   decision only, no bibliographic data) into
   `decisions/manual_decisions.csv`.
4. Rerun `Rscript run_pipeline.R`, check the counts, and commit
   `decisions/manual_decisions.csv`.

Decisions are stored against record IDs, so they stay valid as long as the
raw exports are unchanged (check `input_manifest.csv`). If the searches are
re-run and re-exported, the record IDs change and the review must be redone.

## Inter-rater reliability sample

After deduplication is final (no pairs awaiting a decision):

```sh
Rscript irr_sample.R
```

This picks 100 random publications from the deduplicated library and writes
`output/irr_sample.csv` (a screening sheet with blank decision/note columns
for each rater) and `output/irr_sample.ris` (to load into a screening tool).
Whole-proceedings records are not eligible.

The draw is reproducible: each record is scored with SHA-256 of
`<seed>:<duplicate_id>` and the lowest-scoring records are chosen, which
gives the same sample on any computer or R version. The chosen IDs are
saved to `decisions/irr_sample_ids.csv`; commit this file. Later runs reuse
it, so the sample stays fixed. Sample size, seed and rater names are set in
`config.yml` under `irr_sample`. To draw a new sample, delete the IDs file.

## Reporting

For the methods section: *"Records were deduplicated with a scripted R
pipeline (available at …) using ASySD v0.4.7 (Hair et al., 2023) for
candidate matching. Missing abstracts and bibliographic fields for ACM
Digital Library records were retrieved from OpenAlex and Crossref by DOI,
or by near-identical title match (≥ 0.95 similarity, same first author,
year ± 1) where no DOI was available. Automatically matched pairs with
conflicting DOIs and pairs flagged as possible duplicates were checked
manually."*

Hair K, Bahor Z, Macleod M, Liao J, Sena ES. The Automated Systematic Search
Deduplicator (ASySD): a rapid, open-source, interoperable tool to remove
duplicate citations in biomedical systematic reviews. *BMC Biology*
2023;21:189. https://doi.org/10.1186/s12915-023-01686-z

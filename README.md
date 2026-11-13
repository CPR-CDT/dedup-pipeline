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

`output/input_manifest.csv` records a SHA-256 checksum of every raw file, so
anyone rerunning the pipeline can confirm they have the same exports.
Given the same raw files and the same `decisions/manual_decisions.csv`, the
pipeline produces byte-identical results.

## Outputs (`output/`)

| File | Contents |
|---|---|
| `deduplicated_library.ris` / `.bib` / `.csv` | One record per unique study report, ready for the screening tool |
| `summary.txt`, `prisma_counts.csv` | Counts for the PRISMA flow diagram |
| `record_log.csv` | Every original record → the group it belongs to and whether it was kept as the primary record |
| `matched_pairs.csv` | Every link used to merge records, with how it was made (`auto`, `exact`, `manual`) and ASySD similarity scores |
| `manual_review_pairs.csv` | Possible duplicates that need a human decision |
| `groups_with_doi_conflicts.csv` | Groups that still contain different DOIs (should normally be empty) |
| `input_manifest.csv`, `session_info.txt` | Checksums of inputs; R and package versions |

## How deduplication works

1. **Import** (`R/01_import.R`). Each export is read into the same fields:
   author, year, title, journal/venue, volume, issue, pages, DOI, ISSN/ISBN,
   abstract. Every record gets a stable ID such as `Scopus-00042` (source +
   position, files read in alphabetical order). The ACM export is formatted
   citations rather than structured data, so ACM fields are recovered with
   pattern matching and ACM records have no abstracts.
2. **Clean** (`R/02_clean.R`). HTML entities and tags are removed, DOIs are
   reduced to the bare `10.xxxx/...` form, and author names are rewritten in
   one style (`Surname AB, ...`, accents removed) so that "H. Talat",
   "Talat, H." and "Talat, H" compare as equal. The original author names
   are kept for the exported library.
3. **Match with ASySD** (`R/03_deduplicate.R`). ASySD upper-cases and strips
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
4. **Group and merge** (`R/04_groups.R`). Records are nodes and accepted
   pairs are links; each connected set of records is one group (if A = B
   and B = C, then A, B and C are one study report). Manual decisions add
   (`match`) or remove (`no_match`) links. Each group becomes one record:
   the base is the record from the highest-priority source
   (Scopus > WoS > IEEE > ACM, set in `config.yml`), and any empty field is
   filled from the other records in source-priority order. The group ID is
   the base record's ID and `record_ids` lists all merged records.
5. **Report** (`R/05_report.R`). Writes the library, logs and counts.

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

## Reporting

For the methods section: *"Records were deduplicated with a scripted R
pipeline (available at …) using ASySD v0.4.7 (Hair et al., 2023) for
candidate matching. Automatically matched pairs with conflicting DOIs and
pairs flagged as possible duplicates were checked manually."*

Hair K, Bahor Z, Macleod M, Liao J, Sena ES. The Automated Systematic Search
Deduplicator (ASySD): a rapid, open-source, interoperable tool to remove
duplicate citations in biomedical systematic reviews. *BMC Biology*
2023;21:189. https://doi.org/10.1186/s12915-023-01686-z

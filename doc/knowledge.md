# Knowledge Layer

`brex knowledge` turns notes into a searchable, reviewable knowledge pool. It is based on the local-first model used elsewhere in `brain-ex`: SQLite stores indexes, metadata, retrieval history, and audit records; vault markdown stores durable human-readable artifacts after promotion.

The design borrows these ideas from the Fossil knowledge-layer docs in `~/Projects/fossil-scm/doc/ai/`:

- SQLite is the control plane for runtime metadata, retrieval, review, and provenance.
- Durable text should be materialized as browseable artifacts instead of staying only in SQL rows.
- Retrieval should be tier-aware and provenance-aware.
- Repeated retrieval should reinforce useful notes without deleting lower-tier source material.
- Processing should surface duplicates, stale artifacts, and promotion candidates.

## Workflow

1. Capture notes with `brex note add`, `brex note`, or vault imports.
2. Run `brex knowledge sync` to index notes into `knowledge_items`. Most knowledge commands also sync first.
3. Run `brex knowledge search <query>` to retrieve notes. Retrieval events are recorded.
4. Run `brex knowledge process` to classify duplicates, stale items, and promotion readiness.
5. Run `brex knowledge queue` to inspect items that need review.
6. Run `brex knowledge promote <id> --tier 2` or `--tier 3` to materialize useful knowledge under the vault `knowledge/` tree.

## Commands

### `brex [brain] knowledge sync`

Synchronizes rows from `notes` into `knowledge_items`.

It preserves:

- note title and subject
- source type and source reference
- content hash for duplicate detection
- existing retrieval count, heat, tier, and artifact metadata

When a promoted item changes in the source note, sync marks its artifact state as `stale`.

### `brex [brain] knowledge search <query> [--limit N]`

Searches indexed notes and records a retrieval event.

Search ranking combines:

- lexical matches in title, subject, and content
- tier weight
- heat
- retrieval count
- artifact state
- duplicate penalty

Each successful retrieval writes:

- one row in `knowledge_retrievals`
- one or more rows in `knowledge_retrieval_results`
- reinforcement on `knowledge_items.heat`
- updated `retrieval_count` and `last_retrieved_at`
- co-retrieval edges in `knowledge_links` when multiple notes are returned
- review rows in `knowledge_reviews`

### `brex [brain] knowledge browse [--limit N]`

Lists indexed knowledge with concise provenance metadata. Use this to see IDs before `show`, `history`, or `promote`.

### `brex [brain] knowledge show <id>`

Shows one indexed knowledge item, including source metadata, artifact status, promotion status, and content.

### `brex [brain] knowledge history [id]`

Without an ID, lists recent retrieval events.

With an ID, lists retrieval events that returned that item, including rank and score.

### `brex [brain] knowledge process`

Runs the review loop across indexed notes:

- detects exact duplicates by content hash
- marks duplicate notes as `duplicate`
- marks repeatedly retrieved atomic notes as `ready`
- marks changed promoted notes as `stale`
- writes review audit rows

The current atomicity check is intentionally simple: long notes are flagged as needing split review; shorter notes can become promotion-ready after repeated retrieval.

### `brex [brain] knowledge queue`

Shows items that need review:

- `ready`: repeated retrieval suggests promotion review
- `duplicate`: exact duplicate of another note
- `stale`: promoted artifact no longer matches the source note

### `brex [brain] knowledge promote <id> [--tier N] [--status STATUS]`

Materializes a knowledge item into the configured vault.

Defaults:

- `--tier 2` writes a draft artifact under `knowledge/tier2/`
- `--tier 3` writes an atomic artifact under `knowledge/tier3/` and defaults to `materialized`
- duplicate items are refused
- brains without a configured vault cannot promote

Generated artifacts include title, tier, process level, source reference, subject, promotion timestamp, and content.

Example:

```sh
brex work knowledge promote 7 --tier 3
```

Creates a markdown file similar to:

```text
my_vault/knowledge/tier3/some-title-7.md
```

## Tiers And Statuses

Tiers describe curation state:

- `0`: raw capture
- `1`: working note
- `2`: draft synthesis
- `3`: atomic durable concept

Process levels describe review state:

- `working`
- `review`
- `draft-ready`
- `draft`
- `atomic`
- `duplicate`

Promotion and artifact statuses describe publication state:

- `pool`: indexed but not ready
- `ready`: repeated retrieval suggests promotion review
- `duplicate`: exact duplicate of another item
- `draft`: promoted to draft artifact
- `materialized`: promoted as durable artifact
- `stale`: source changed after promotion

## Tables

Knowledge features create these tables in the brain database:

- `knowledge_items`: indexed notes, provenance fields, tier, heat, retrieval counters, duplicate lineage, and artifact references
- `knowledge_retrievals`: retrieval events
- `knowledge_retrieval_results`: note-level retrieval result audit records
- `knowledge_links`: explicit note-to-note links such as co-retrieval reinforcement
- `knowledge_reviews`: processing, retrieval, and promotion review audit rows
- `knowledge_pool`: legacy interaction-weight table retained for compatibility

The ordinary `notes` table remains the source of note content. Knowledge tables are derived metadata and audit state.

## Provenance

Every indexed note gets:

- `source_type`: currently `note`
- `source_id`: stable subject/title reference such as `data/SQL Basics`
- `source_ref`: markdown-style source reference such as `data/SQL Basics.md`

Promoted notes also get:

- `artifact_kind`: currently `file`
- `artifact_ref`
- `artifact_path`
- `artifact_status`

Source fields describe origin. Artifact fields describe durable publication targets. These are intentionally separate.

## Update Behavior

`brex init --vault` imports vault notes and immediately syncs the knowledge pool.

`brex update` rebuilds notes from the vault and syncs knowledge metadata.

`brex update --file <path>` updates one note and then syncs the knowledge pool so search and provenance metadata reflect the changed file.

## Testing

Knowledge coverage lives in `tst/knowledge.bats`.

The tests cover:

- schema creation
- sync
- search success, empty search, and limits
- browse and show
- global and per-item history
- retrieval audit rows and co-retrieval links
- duplicate detection
- promotion readiness
- tier 2 and tier 3 artifact creation
- duplicate-promotion refusal
- missing ID and missing item errors
- no-vault promotion errors
- stale artifact detection after source changes
- `update --file` knowledge resync
- usage output for missing or unknown subcommands

Run focused tests:

```sh
./bld/build.sh
bats tst/knowledge.bats
```

Run the full suite with an isolated writable home:

```sh
HOME=$(mktemp -d /tmp/brain-ex-test-home.XXXXXX) bats tst
```

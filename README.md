# brain-ex (`brex`)

Local-first, offline-first CLI note-taking and task management app backed by SQLite. Built for instant (`<10 ms`) terminal capture and deterministic scripting by both humans and external coding agents—with zero embedded LLM/network calls on normal operations.

## Operating Modes

`brex` keeps a single fast local SQLite brain and pairs cleanly with your preferred workspace:

1. **Standalone SQLite (`brex init`)** — Pure CLI + `$EDITOR` over a local `.db` file.
2. **Obsidian Vault (`brex init --vault <dir>` + `brex update`)** — Bidirectional Markdown mirror with frontmatter-aware tasks, `[[subject/title]]` wikilinks, and optional vault promotion (`brex knowledge promote`).
3. **Tandem with `daat` (`daat_url` + `brex sync`)** — Clean separation of duties:
   - **`brex` owns fast local CLI capture, task triage, and offline search** for you and your terminal coding agents.
   - **`daat` owns the web UI, knowledge graph, embeddings, tag ontology, AI chat agent, and automatic curation/distillation**—syncing curated `Knowledge Pool` and `Tag Summaries` notes back into your local `brex` database (`tier = 2`, `curated`) via `brex sync`.

## Commands

```
Usage: brex [brain] <command> [subcommand] [arguments]

brex init
brex brain < list | use >
brex [brain] note < add | edit | connect | last >
brex [brain] task < add | list | done | delay | prioritize | show | last >
brex [brain] sync [ --dry-run ] [ --url <url> ]
brex [brain] update [ --file <path> ] [ --force ]
brex [brain] sql [ --query <sql> ]
brex [brain] knowledge < search | sync | browse | show | history | process | queue | promote >

brex <command> -h or --help for more info
```

## Examples

- `brex init --name work --editor vim`
- `brex init --vault my_vault --name work --editor vim --git`
- `brex note add --title "standup" --subject "work" --content "daily sync" --update`
- `brex work task add --title "finish docs" --due_to "2026-02-20" --importance 4 --urgency 3`
- `brex work knowledge search "database indexes" --limit 5`
- `brex work knowledge search "database indexes" --remote`
- `brex sync --dry-run`
- `brex brain list`
- `brex brain use --name work`

## Knowledge Layer

`brex knowledge` indexes local notes into SQLite tables (`knowledge_items`, `knowledge_retrievals`, `knowledge_links`, `knowledge_reviews`), tracks retrieval heat and co-retrieval edges, flags exact duplicates, boosts `daat`-curated `knowledge-pool` notes automatically (`tier = 2`), and supports materializing promoted notes into an Obsidian vault (`knowledge/tier2/`, `knowledge/tier3/`).

See [doc/knowledge.md](doc/knowledge.md) for the full command reference and schema details.

## Directory Structure

| Directory | Purpose |
|-----------|---------|
| `bld/` | Build scripts and static `package.lua` manifest |
| `bin/` | Compiled binary output (gitignored) |
| `pub/` | Release packaging artifacts |
| `src/` | Luam source modules |
| `tst/` | Bats integration test suite |
| `doc/` | Design and command documentation |

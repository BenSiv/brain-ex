# Brain-Ex

A note and task management CLI tool written in Ada.

## About

Brain-Ex is a command-line tool for managing notes and tasks using SQLite as the backend.

## Features

- **Notes**: Create, view, and organize notes with timestamps
- **Tasks**: Manage tasks with due dates and subjects  
- **SQLite Backend**: All data stored in a local SQLite database
- **Config Management**: Simple YAML-based configuration

## Requirements

- GNAT Ada compiler (v14.2.0 or later)
- SQLite3
- Bash shell

## Building

```bash
./build.sh
```

This creates the `brain_ex` executable in the project root.

## Usage

### Initialize a new brain

```bash
./brain_ex init --name myproject
```

### Create notes

```bash
# Quick log note
./brain_ex note --content "Meeting notes from today"

# Note with subject and title
./brain_ex note add --subject "work" --title "Project Ideas" --content "Some ideas..."
```

### Manage tasks

```bash
# Add a task
./brain_ex task --content "Review pull request"

# List tasks
./brain_ex task list

# List tasks by subject
./brain_ex task list --subject "work"
```

### View recent notes

```bash
./brain_ex note last --number 10
```

## Project Structure

```
brain-ex/
├── ada_src/           # Ada source files
├── archive/lua/       # Original Lua implementation
├── doc/              # Documentation
├── brain_ex.gpr      # GNAT project file
└── build_ada.sh      # Build script
```

## Configuration

Configuration is stored in `~/.config/brain-ex/config.yaml`:

```yaml
brain: /path/to/database.db
editor: nano
vault: /optional/vault/path
git: false
```

## Development

The Ada implementation follows the original Lua design but leverages Ada's strong typing and safety features. Key modules:

- `Src.Init` - Database initialization
- `Src.Note` - Note operations
- `Src.Tasks` - Task management
- `Src.Sql` - SQLite interface
- `Src.Config` - Configuration management

## License

MIT License - see LICENSE file

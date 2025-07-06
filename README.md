# Modern Emacs Configuration

A clean, minimal Emacs configuration focused on enhanced completion, AI assistance, and developer productivity.

## Features

### Core Completion System
- **Vertico** + **Marginalia** + **Orderless** - Modern minibuffer completion
- **Consult** - Enhanced commands with live preview (search, buffers, files)
- **Embark** - Context actions on completion candidates
- **Corfu** - In-buffer completion with 0.2s auto-completion delay

### Development Tools
- **Treesitter** - Advanced syntax highlighting via `treesit-auto`
- **LSP** - Language server support via `eglot` for Python, JS, TS, Go, Rust
- **Magit** - Full Git interface with `C-x g`
- **Diff-hl** - Git diff highlighting in buffers
- **Markdown** - Enhanced markdown editing with code block highlighting

### AI Integration
- **Claude Code** - Full CLI integration with project awareness
- **GitHub Copilot** - AI-powered code completion with tab completion

### Terminal Integration
- **Vterm** - Full terminal emulator
- **Eat** - Emulate A Terminal with eshell integration

### Note-Taking & Organization
- **Org Mode** - Enhanced with pretty entities, inline images, and clean startup
- **Org Capture** - Quick note capture with structured metadata and smart tagging

### Smart Features
- Fixed light theme (modus-operandi) for consistent appearance
- JSON auto-formatting on save (files < 50KB only)
- Organized backup system in `backups/` subdirectories
- Auto-revert for external file changes
- Save-place mode for cursor position memory
- Emacs server auto-start for external connections

## Key Bindings

### Core Navigation
- `C-s` - Enhanced search (consult-line)
- `C-x b` - Enhanced buffer switching (consult-buffer)
- `C-x C-r` - Recent files (consult-recent-file)
- `C-x g` - Git status (magit)

### Context Actions
- `C-.` - Context menu (embark-act)
- `C-;` - Smart context actions (embark-dwim)

### AI & Development
- `C-c c` - Claude Code commands (full transient menu)
- `C-c M-c` - Toggle Copilot mode
- `C-c M-n/p` - Next/previous Copilot completion
- `C-c M-f` - Accept Copilot completion by line

### Note-Taking
- `C-c n` - Capture new note with metadata

### Utilities
- `C-s-f` - Toggle fullscreen
- `Tab` - Accept Copilot completion
- `C-Tab` - Accept Copilot completion by word

## Installation

1. Clone or copy this configuration to `~/.emacs.d/`
2. Start Emacs - packages will auto-install on first run
3. Set up API keys in your shell environment:
   ```bash
   # Add any needed API keys for AI services
   ```
4. Install language servers for LSP support as needed
5. Ensure `~/workspace/notes/` directory exists for note-taking

## Directory Structure

```
~/.emacs.d/
├── init.el                    # Main configuration
├── early-init.el             # Early UI and backup configuration
├── custom.el                 # Auto-generated customizations
├── CLAUDE.md                 # AI assistant instructions
├── README.md                 # This file
├── lisp/                     # Custom packages (auto-created)
├── backups/                  # Backup directories (auto-created)
│   ├── backups/              # File backups
│   └── auto-saves/           # Auto-save files
├── elpa/                     # Package installation directory
└── eshell/                   # Eshell configuration (auto-created)
```

## Language Server Setup

Install language servers for full LSP support:

```bash
# Python
pip install python-lsp-server

# JavaScript/TypeScript
npm install -g typescript-language-server typescript

# Go
go install golang.org/x/tools/gopls@latest

# Rust
rustup component add rust-analyzer
```

## Notes Directory

The configuration saves captured notes to `~/workspace/notes/`. Create this directory or adjust the path in the org-capture template in `init.el`.

## Customization

### Key Configuration Points
- **Theme**: Fixed modus-operandi theme in `early-init.el`
- **Note Directory**: Update org-capture template path in `init.el`
- **LSP Languages**: Add to `eglot` hook in `init.el`
- **Backup Locations**: Adjust paths in `early-init.el`

### Adding New Packages
Use the `use-package` macro following the existing patterns in `init.el`. All packages with `:ensure t` will auto-install.

This configuration emphasizes productivity through intelligent completion, seamless AI integration, and modern development tools while maintaining Emacs' flexibility and power.
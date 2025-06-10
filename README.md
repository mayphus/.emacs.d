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
- **Markdown** - Enhanced markdown editing with code block highlighting

### AI Integration
- **GPTel** - Multi-backend AI support (Gemini default, DeepSeek available)
- **Claude Code** - Full CLI integration with project awareness
- **GitHub Reports** - Custom package for automated GitHub activity reports

### Note-Taking & Organization
- **Denote** - Structured note-taking system with linking and backlinks
- **Org Mode** - Enhanced with pretty entities, inline images, and clean startup
- **Stocks** - Real-time stock market data with auto-refresh

### Smart Features
- Auto theme switching (light/dark based on macOS system appearance, checks every 60 seconds)
- JSON auto-formatting on save (files < 50KB only)
- Environment variable importing for API keys (OPENAI, GEMINI, DEEPSEEK)
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
- `C-c a d` - Switch to DeepSeek AI backend
- `C-c a g` - Switch to Gemini AI backend
- `C-c a c` - Send to current AI backend
- `C-c a m` - GPTel menu
- `C-c g d` - GitHub daily report
- `C-c g w` - GitHub weekly report
- `C-c g r` - GitHub repository summary
- `C-c g i` - GitHub contribution insights

### Note-Taking
- `C-c n n` - Create new note
- `C-c n i` - Insert link to note
- `C-c n b` - Show backlinks

### Utilities
- `F5` - Toggle light/dark theme manually
- `C-s-f` - Toggle fullscreen
- `C-c s` - Stock market display

## Installation

1. Clone or copy this configuration to `~/.emacs.d/`
2. Start Emacs - packages will auto-install on first run
3. Set up API keys in your shell environment:
   ```bash
   export GEMINI_API_KEY="your-gemini-key"
   export DEEPSEEK_API_KEY="your-deepseek-key"
   export OPENAI_API_KEY="your-openai-key"  # optional
   ```
4. Install language servers for LSP support as needed
5. Ensure `~/workspaces/org/` directory exists for note-taking

## Directory Structure

```
~/.emacs.d/
├── init.el                    # Main configuration
├── early-init.el             # Early UI and backup configuration
├── custom.el                 # Auto-generated customizations
├── CLAUDE.md                 # AI assistant instructions
├── README.md                 # This file
├── lisp/                     # Custom packages
│   ├── github-reports.el     # GitHub activity reports
│   └── stocks.el             # Stock market integration
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

The configuration expects notes in `~/workspaces/org/` for Denote integration. Create this directory or adjust the path in `init.el`.

## Stock Market Features

The custom stocks package provides:
- **Real-time Data**: Yahoo Finance API integration
- **Default Symbols**: S&P 500, Dow Jones, NASDAQ, Russell 2000, VIX
- **Auto-refresh**: Configurable interval (default 5 seconds)
- **Interactive Management**: Add/remove symbols dynamically
- **Visual Feedback**: Color-coded gains/losses

### Stock Buffer Commands
- `g` - Refresh data
- `a` - Add new symbol
- `d` - Remove symbol
- `t` - Toggle auto-refresh
- `q` - Quit window

## GitHub Reports Features

The custom GitHub Reports package provides automated reporting:
- **Daily Reports**: Activity summary for current day
- **Weekly Reports**: Comprehensive weekly analysis
- **Repository Summary**: Current repo health and status
- **Contribution Insights**: Development patterns and productivity metrics

All reports are generated via Claude Code integration using the `gh` CLI.

## Customization

### Key Configuration Points
- **Theme Switching**: Modify `auto-switch-theme` function in `init.el:211-225`
- **Note Keywords**: Update `denote-known-keywords` in `init.el:269`
- **LSP Languages**: Add to `eglot` hook in `init.el:124`
- **AI Backends**: Configure additional providers in GPTel section (`init.el:154-187`)
- **Stock Symbols**: Modify `stock-default-symbols` in `lisp/stocks.el:19`
- **Backup Locations**: Adjust paths in `early-init.el:17-25`

### Adding New Packages
Use the `use-package` macro following the existing patterns in `init.el`. All packages with `:ensure t` will auto-install.

This configuration emphasizes productivity through intelligent completion, seamless AI integration, and modern development tools while maintaining Emacs' flexibility and power.
# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs configuration directory that provides a modern, minimalist setup focused on enhanced completion and integration with Claude Code. This configuration serves as a general-purpose development environment suitable for various programming tasks, web development, automation scripts, and AI/ML experiments.

## Key Configuration Architecture

### Core Components
- `init.el` - Main configuration file with package setup and key bindings
- `early-init.el` - Early initialization for UI elements and backup configuration
- `lisp/` - Directory for custom Elisp packages (auto-created when needed)
- `custom.el` - Auto-generated customization file (if exists)

### Package Management
Uses built-in `use-package` with `:ensure t` for automatic package installation. Packages are installed to `elpa/` directory.

### Essential Packages and Their Roles
- **magit** - Git interface (bound to `C-x g`)
- **vertico + marginalia + orderless** - Modern completion system trio
- **consult** - Enhanced commands with live preview (C-s, C-x b, C-x C-r, M-g g, M-s d/g/r)
- **embark + embark-consult** - Context actions on completion candidates
- **corfu** - In-buffer completion UI with auto-completion (0.2s delay)
- **treesit-auto** - Advanced syntax highlighting via treesitter
- **eglot** - LSP support for Python, JS, TS, Go, Rust
- **which-key** - Key binding discovery (0.3s delay)
- **claude-code** - Integration with Claude Code CLI (bound to `C-c c`)
- **gptel** - Multi-backend AI support (Gemini default, DeepSeek available) (bound to `C-c a`)
- **github-reports** - GitHub integration tools (bound to `C-c g`)
- **denote** - Note-taking system for org files (bound to `C-c n`)
- **org** - Enhanced org-mode with pretty entities and inline images
- **markdown-mode** - Enhanced markdown editing
- **standard-themes** - Light/dark theme pair with auto-switching
- **exec-path-from-shell** - Environment variable importing

### Key Bindings Structure
- `C-c c` - Claude Code command map (primary interface for AI assistance)
  - `C-c c p` - Switch between Claude project sessions
  - `C-c c P` - List all active Claude project sessions
  - `C-u C-c c k` - Kill specific project's Claude session
- `C-c a` - GPTel AI commands (d=DeepSeek, g=Gemini, c=send, m=menu)
- `C-c g` - GitHub Reports command map (d=daily, w=weekly, r=repo summary, i=insights)
- `C-c n` - Denote note-taking commands
- `C-x g` - magit-status (git interface)
- `C-s` - consult-line (enhanced search)
- `C-x b` - consult-buffer (enhanced buffer switching)
- `C-x C-r` - consult-recent-file
- `M-g g` - consult-goto-line
- `M-s d` - consult-find
- `M-s g` - consult-grep
- `M-s r` - consult-ripgrep
- `C-.` - embark-act (context actions)
- `C-;` - embark-dwim (smart context actions)
- `F5` - toggle-theme (light/dark theme switching)
- `C-s-f` - toggle fullscreen

### Directory Structure
- `backups/backups/` - Backup files (auto-created in early-init.el)
- `backups/auto-saves/` - Auto-save files (auto-created in early-init.el)
- `elpa/` - Package installation directory
- `lisp/` - Custom Elisp packages (contains meow-config.el and claude-code.el/)
- `eshell/` - Eshell configuration (auto-created)
- Root contains only essential configuration files

## Working with This Configuration

### Testing Configuration Changes
- Restart Emacs to test full configuration: `emacs -Q -l init.el`
- Evaluate expressions in current session: `M-: (expression)`

### Package Management
- Install new packages by adding `use-package` declarations to `init.el`
- Packages auto-install on first Emacs startup with `:ensure t`
- Update all packages: `M-x package-upgrade-all`

### Claude Code Integration
The configuration includes claude-code.el for AI assistance with multi-project support:
- Start Claude session: `C-c c c` (project root) or `C-c c d` (current directory)
- Switch between projects: `C-c c p` (switch project) or `C-c c P` (list all projects)
- Send commands: `C-c c s` (basic) or `C-c c x` (with context)
- Access all features via transient menu: `C-c c m`
- Execute Emacs Lisp: Use `emacsclient --eval '(expression)'` for external integration

#### Multi-Project Features
- Each project gets its own Claude session with buffer name `*claude-<project>*`
- Automatic project detection based on current file's project root
- Commands automatically route to the correct project's Claude session
- Kill specific project sessions with `C-u C-c c k`

### Configuration Conventions
- All settings use lexical binding
- Packages configured immediately after declaration
- File organization: early-init.el for UI and backups, init.el for packages and behavior
- Custom settings isolated to separate custom.el file
- Backup files organized in `backups/backups/` subdirectory (configured in early-init.el)
- Auto-save files organized in `backups/auto-saves/` subdirectory (configured in early-init.el)
- Tab width set to 2 spaces, no tabs mode
- JSON auto-formatting on save (files < 50KB only)
- Global auto-revert mode enabled
- Save-place mode for cursor position memory
- Treesitter auto-mode for enhanced syntax highlighting
- LSP support via Eglot for Python, JS, TS, Go, Rust
- Auto theme switching based on system appearance (checks every 60 seconds)
- Environment variable importing for API keys (OPENAI_API_KEY, GEMINI_API_KEY, DEEPSEEK_API_KEY)
- Multiple AI backend support through GPTel (Gemini default, DeepSeek available)
- Emacs server auto-start for external connections

### Note-Taking with Denote
The configuration includes Denote for structured note-taking:
- Default directory: `~/workspaces/org/`
- Available keywords: emacs, programming, electronics, article, project, journal
- Prompts for title and keywords, infers and sorts keywords
- Journal template: "* Daily Notes\n\n** Tasks\n\n** Notes\n\n"
- Key commands:
  - `C-c n n` - denote (new note)
  - `C-c n c` - denote-region (create note from region)
  - `C-c n N` - denote-type
  - `C-c n d` - denote-date
  - `C-c n z` - denote-signature
  - `C-c n s` - denote-subdirectory
  - `C-c n t` - denote-template
  - `C-c n i` - denote-link (insert link)
  - `C-c n I` - denote-add-links
  - `C-c n b` - denote-backlinks
  - `C-c n f f` - denote-find-link
  - `C-c n f b` - denote-find-backlink
  - `C-c n r` - denote-rename-file
  - `C-c n R` - denote-rename-file-using-front-matter
  - `C-c n o` - denote-org-capture

### Org Mode Enhancements
- Startup with indented view and inline images
- Pretty entities display for UTF8 characters
- Hidden emphasis markers for cleaner appearance

### AI Integration Features
The configuration provides comprehensive AI support:

#### GPTel Integration
- Multiple backend support: Gemini (default) and DeepSeek
- Easy backend switching via key bindings
- Environment variable based API key management
- Streaming support for real-time responses

#### Claude Code Integration
- Full Claude Code CLI integration
- Project and directory-specific sessions
- Context-aware command sending
- Transient menu interface for all features

### Development Environment
- **LSP Support**: Automatic language server activation for major languages
- **Treesitter**: Advanced syntax highlighting and parsing
- **Auto-formatting**: JSON files formatted on save (files < 50KB only)
- **Smart Completion**: Context-aware completion with Corfu
- **Project Navigation**: Enhanced file finding and buffer management

### Emacs Lisp Tasks
- Use Emacs Lisp to accomplish Emacs-related tasks for better customization and integration
- Execute Emacs Lisp code via `emacsclient --eval '(expression)'` for external integration

### Auto-start Behavior
- Configuration automatically starts Claude Code on Emacs startup
- Cleans up non-essential buffers and focuses on Claude interface
- Provides seamless AI assistance integration from first launch

### General Development Workflows
This configuration supports diverse development tasks:
- **Web Applications**: Full-stack development with LSP support
- **Automation Scripts**: Python, shell scripting with enhanced completion
- **AI/ML Projects**: Jupyter integration, data science workflows
- **Hardware Projects**: Microcontroller programming, PCB design documentation
- **Documentation**: Org-mode for technical writing, Denote for knowledge management

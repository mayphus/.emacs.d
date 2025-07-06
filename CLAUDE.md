# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs configuration directory that provides a modern, minimalist setup focused on enhanced completion and integration with Claude Code. This configuration serves as a general-purpose development environment suitable for various programming tasks, web development, automation scripts, and AI/ML experiments.

## Key Configuration Architecture

### Core Components
- `init.el` - Main configuration file with package setup, key bindings, and most settings
- `early-init.el` - Minimal early initialization for package system, UI elements, and native compilation
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
- **copilot** - GitHub Copilot integration with tab completion
- **diff-hl** - Git diff highlighting in buffers
- **mini-echo** - Minimal echo area enhancements
- **cape** - Completion at point extensions
- **vterm** - Full terminal emulator
- **eat** - Emulate A Terminal (eshell integration)
- **org** - Enhanced org-mode with pretty entities and inline images
- **markdown-mode** - Enhanced markdown editing
- **modus-themes** - Light theme (modus-operandi)
- **exec-path-from-shell** - Environment variable importing

### Key Bindings Structure
- `C-c c` - Claude Code command map (primary interface for AI assistance)
  - `C-c c p` - Switch between Claude project sessions
  - `C-c c P` - List all active Claude project sessions
  - `C-u C-c c k` - Kill specific project's Claude session
- `C-c M-c` - Toggle Copilot mode
- `C-c M-n/p` - Next/previous Copilot completion
- `C-c M-f` - Accept Copilot completion by line
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
- `C-s-f` - toggle fullscreen

### Directory Structure
- `backups/backups/` - Backup files (auto-created in init.el)
- `backups/auto-saves/` - Auto-save files (auto-created in init.el)
- `elpa/` - Package installation directory
- `lisp/` - Custom Elisp packages (auto-created when needed)
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
- File organization: early-init.el for minimal essential startup, init.el for packages and behavior
- Custom settings isolated to separate custom.el file
- Backup files organized in `backups/backups/` subdirectory (configured in init.el)
- Auto-save files organized in `backups/auto-saves/` subdirectory (configured in init.el)
- Tab width set to 2 spaces, no tabs mode
- JSON auto-formatting on save (files < 50KB only)
- Global auto-revert mode enabled
- Save-place mode for cursor position memory
- Treesitter auto-mode for enhanced syntax highlighting
- LSP support via Eglot for Python, JS, TS, Go, Rust
- Fixed light theme (modus-operandi)
- Emacs server auto-start for external connections


### Org Mode Enhancements
- Startup with indented view and inline images
- Pretty entities display for UTF8 characters
- Hidden emphasis markers for cleaner appearance

#### Org Capture Integration
- **Note Capture**: `C-c n` to capture notes with structured metadata
- **Smart Tagging**: Handles comma/space-separated tags with automatic processing
- **File Organization**: Saves to `~/workspace/notes/` with slugified filenames
- **Template Structure**: Matches existing org files with title, date, and filetags
- **Finalization**: Use `C-c C-c` to save, `C-c C-k` to abort capture

### AI Integration Features
The configuration provides comprehensive AI support:

#### GitHub Copilot Integration
AI-powered code completion and suggestions:
- **Tab Completion**: Accept suggestions with Tab
- **Line-by-line**: Accept partial completions
- **Multi-language Support**: Works across programming languages
- **Context-aware**: Understands current code context

Key Commands:
- `Tab` - Accept Copilot completion
- `C-Tab` - Accept completion by word
- `C-c M-c` - Toggle Copilot mode
- `C-c M-n/p` - Next/previous completion
- `C-c M-f` - Accept completion by line

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
- **Documentation**: Org-mode for technical writing

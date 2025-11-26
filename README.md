```markdown
# emacs-config

My Emacs Configurations for Laptop and Android Devices

## Overview

This repository contains a modular, portable Emacs configuration optimized for:

- **Multi-device support**: Laptop, Termux (Android), and tablet via device.el.
- **Fast startup**: Lazy loading, optimized hooks, and straight.el for packages.
- **Literate programming**: Single `config.org` with tangled .el files for readability.
- **Reliable & reproducible**: Declarative packages with use-package; no external dependencies beyond Emacs 30.
- **Synced workflow**: GitHub + Syncthing for cross-device consistency.

Tested on Emacs 30+ as of November 26, 2025.

---

## 🛠 Installation Instructions

### Prerequisites

- **Emacs Version**: 30 or later (tested on 30.0.50+).
- **Dependencies**:
  - Git (`sudo apt install git` on Ubuntu).
  - Hunspell for spell-checking (`sudo apt install hunspell`).
  - Zotero + Better BibTeX add-on for bibliography (optional, for Citar/Org-roam integration).
  - TeX Live + latexmk for LaTeX/PDF export (`sudo apt install texlive-full latexmk`).
  - Initial internet for straight.el bootstrap (subsequent runs use local copies).

### Step 1: Clone the Repository

```bash
git clone https://github.com/chaicurioquest/emacs-config.git ~/.emacs.d
```

This places the config in Emacs' default directory for seamless integration.

### Step 2: Customize device.el

Detects device type for paths/settings. Edit `~/.emacs.d/device.el`:

```elisp
(defvar my-device-configs
  (let ((table (make-hash-table :test 'equal)))
    (puthash "your-laptop-hostname" 'laptop table)  ; Replace with (system-name)
    (puthash "termux" 'termux table)                ; For Android Termux
    (puthash "your-tablet-hostname" 'tablet table)  ; Replace with tablet's (system-name)
    table)
  "Map system names to device types.")

(defvar my-device
  (or (gethash system-name my-device-configs)
      (if (string-match "termux" system-configuration) 'termux 'laptop))
  "Current device type.")
(provide 'device)
```

- Get hostname: `M-: system-name`.
- Save and restart Emacs.

### Step 3: Set Up Zotero Integration (Optional)

For Citar/Org-roam-bibtex:
- Install Zotero + Better BibTeX.
- Export library as Better BibLaTeX to `/wspace/org/bib/references.bib` (adjust path for devices).
- Enable automatic export in Zotero: Preferences > Better BibTeX > Automatic Export ("On Change").
- PDFs: Store in `/wspace/src/zotero-kbase/storage` (update in `config.org` if needed).

### Step 4: Install Packages and Tangle Config

- Start Emacs — straight.el bootstraps packages (initial run ~1-2 minutes).
- Open `~/.emacs.d/config.org` (`C-x C-f`).
- Tangle: `C-c t` (or `M-x my-tangle-config-org`) for config.org only, or `C-c T` for all modular files.

### Step 5: Verify Setup

- Check `*Messages*` for "✅ Citar ready." and similar confirmations.
- Test: `C-c r n` (new note), `C-c i c` (insert citation), `C-c v` (PDF preview).

---

## 📦 Configurations

### General Configurations
- **Startup**: No splash screen, garbage collection reset, UI elements disabled in early-init.el.
- **Package Management**: straight.el + use-package for declarative installs; lazy/deferred loading for speed.
- **Keybindings**: Centralized in keymaps.el; prefixes like `C-c i` (citations/notes), `C-c r` (Org-roam), `C-c g` (Git).
- **Backups**: Device-specific `.backups/` and `.autosaves/` (git-ignored for clean repos).

### Org-Roam for Note-Taking
- In `org/roam.org`: Zettelkasten with templates (default, fleeting, permanent, journal).
- Dailies: In `roam/daily/`.
- Keybindings: `C-c r f` (find), `C-c r n` (capture), `C-c r d` (daily), `C-c r g` (UI, laptop only).
- Backlinks: Auto `:ID:` in templates.
- Tags: `C-c r t` (add), `C-c r r` (remove) via filetags.

### Citar for Bibliography/Zotero
- Bib: `references.bib` in bib/.
- PDFs: `/wspace/src/zotero-kbase/storage`.
- Keybindings: `C-c i c` (insert), `C-c i N` (open note), `C-c i o` (open PDF).
- Integration: Org-roam for literature notes, Org-noter for annotations.

### Tags and Snippets
- Tags: In `org/filetags.org`; `C-c f t` for completion.
- Snippets: Yankpad + Yasnippet; `C-c y` (insert from yankpad.org).

### GTD Workflow
- In `org/workflow.org`: Device-specific alerts (libnotify on laptop, termux-notification on mobile).
- Integrates with Org-agenda.

### Mail Integration
- In `org/mail.org`: mbsync for Gmail, msmtp for sending.
- Keybindings: `C-c C-c` in mu4e-view for Org capture.
- Alerts via mu4e-alert.

### LaTeX Export
- In `org/latextn.org`: latexmk for PDF, bibliography support.
- Keybindings: `C-c C-e l p` (export).

---

## 🧪 Testing and Troubleshooting

### Test Setup
- Check `*Messages*` for "Device: laptop", "Citar ready", etc.
- Note: `C-c r n`.
- Citation: `C-c i c`.
- PDF annotation: `M-x org-noter`.
- PDF export: `C-c C-e l p`.

### Common Issues
- **Path Errors**: Verify `M-: default-directory` → fix in device.el.
- **Packages Not Installed**: `M-x straight-pull-all`.
- **Org-Roam Db Errors**: Delete `org-roam.db`, `M-x org-roam-db-sync`.
- **Debug**: `export MY_DEBUG_DEVICE=1` for logs.
- **LaTeX Issues**: Ensure TeX Live/latexmk installed.

### Customization
- Templates: Edit `~/.emacs.d/template/` (e.g., generic-note.org).
- Capture: Add templates in `org/orgxtn.org`.
- Enable Consult (uncomment in config.org for advanced search).

Happy hacking! Open issues on GitHub for feedback.  
License: MIT
```
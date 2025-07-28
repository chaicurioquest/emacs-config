# emacs-config
My Emacs Configurations on Laptop and Android Devices

# Emacs Configuration Setup

This repository contains a modular and portable Emacs configuration designed for:

* Multi-device support (laptop, Termux, tablet)
* Fast startup with optimized hooks and lazy loading
* Literate config via `config.org`
* Declarative package management with `straight.el` and `use-package`

---

## 🛠 Installation Instructions

### Prerequisites

- **Emacs Version**: Emacs 30 or later (tested on 30).
- **Dependencies**: 
  - Git for cloning the repo.
  - Aspell for spell-checking (install via package manager, e.g., `sudo apt install aspell` on Linux).
  - Zotero with Better BibTeX add-on for bibliography integration (optional but recommended for Citar/Org-roam-bibtex).
- **No Internet for Packages**: The config uses `straight.el` to bootstrap packages, so an initial internet connection is needed for installation. Subsequent launches use local copies.

### Step 1: Clone the Repository

Clone the repository to your Emacs directory:

```bash
git clone https://github.com/chaicurioquest/emacs-config.git ~/.emacs.d
```

- **Why ~/.emacs.d?**: This is the default Emacs configuration directory. The config is self-contained within it.

### Step 2: Customize device.el

The config uses `device.el` to detect your device type (laptop, Termux, tablet) and adjust paths (e.g., default-directory for notes).

- Open `~/.emacs.d/device.el` and edit the hash table to match your system names:
  ```elisp
  (defvar my-device-configs
    (let ((table (make-hash-table :test 'equal)))
      (puthash "ram" 'laptop table)  ;; Replace "ram" with your laptop's (system-name)
      (puthash "termux" 'termux table)  ;; For Termux on Android
      (puthash "tablet-hostname" 'tablet table)  ;; Replace with your tablet's (system-name)
      table)
    "Map system names to symbolic device types.")

  (defvar my-device
    (or (gethash system-name my-device-configs)
        (if (string-match "termux" system-configuration) 'termux 'generic))
    "Current device type.")
  (provide 'device)
  ```

- **How to Get system-name**: In Emacs, run `M-: system-name` to get your device's hostname.
- **Save and Restart**: After editing, restart Emacs to load the changes.

### Step 3: Set Up Zotero Integration (Optional but Recommended)
For bibliography and citations (using Citar and Org-roam-bibtex):
- Install Zotero and the Better BibTeX add-on.
- In Zotero, export your library as Better BibLaTeX to `~/wspace/org/notes/references.bib` (laptop path; adjust for other devices).
- Set automatic export in Zotero Preferences > Better BibTeX > Automatic Export ( "On Change" trigger).
- PDFs are stored in `~/wspace/src/zotero-kbase/storage` (update in config.org if needed).

### Step 4: Install Packages and Tangle Config
- Start Emacs—the config will auto-install packages via `straight.el` (initial run may take time).
- Manually tangle if needed: Open `config.org` (`C-x C-f ~/.emacs.d/config.org`), press `C-c t` (or `M-x my-tangle-config-org`).

### Step 5: Customize Templates and Setup Files
- **Templates**: Located in `~/.emacs.d/template/` (e.g., `roam-default.org` for Org-roam notes, `generic-note.org` for regular Org notes). Edit them to add headers/metadata (e.g., LaTeX packages).
- **Setup Files**: Located in `~/.emacs.d/setup/` (e.g., `setup-latex.org` for LaTeX exports, `acronyms.org` for glossaries). Update `setup-latex.org` with your LaTeX headers:
  ```org
  #+LATEX_HEADER: \pdfminorversion=7
  #+LATEX_CLASS_OPTIONS: [a4paper,12pt,fleqn]

  #+LATEX_HEADER: \usepackage{amsmath}
  #+LATEX_HEADER: \usepackage{graphicx}
  #+LATEX_HEADER: \usepackage{hyperref}
  #+LATEX_HEADER: \usepackage[acronym]{glossaries}
  #+LATEX_HEADER: \hypersetup{colorlinks=true, linkcolor=blue}
  #+LATEX_HEADER: \makeglossaries
  #+LATEX_HEADER: \usepackage{booktabs}
  #+LATEX_HEADER: \usepackage{siunitx}

  #+OPTIONS: toc:2 num:t
  #+LATEX: \printglossaries

  #+INCLUDE: "acronyms.org"
  ```
- **Acronyms**: Edit `acronyms.org` with your terms (e.g., BJT, JFET for engineering notes).

### Step 6: (Optional) Enable Debug Logs
To log startup details (e.g., paths, device detection):
- Set environment variable: `export MY_DEBUG_DEVICE=1` (add to shell profile, e.g., `.bashrc` for persistence).
- View logs in `*Messages*` buffer.

---

## 📦 Configurations

### General Configurations
- **Startup Optimizations**: Inhibits startup screen, resets garbage collection, disables UI elements for speed (early-init.el if present).
- **Package Management**: Uses `straight.el` for reproducible installs; `use-package` for declarations. Packages include Org-roam for note-taking, Citar for bibliography, PDF-tools/Org-noter for annotations.
- **Keybindings**: Minimal, e.g., `C-c t` for tangling, `C-c i t` for timestamps, `C-c r n` for Org-roam capture.

### Org-Roam for Note-Taking
- Configured in `org/roam.org`: Zettelkasten with templates (default, fleeting, permanent, journal, bib).
- Dailies in `roam/daily/` for daily notes.
- Keybindings: `C-c r f` (find note), `C-c r n` (capture), `C-c r d` (dailies), `C-c r g` (graph UI on laptop).
- Backlinking: Manual `:ID:` addition for regular Org files; auto in templates.

### Citar for Bibliography/Zotero
- Bib file: `references.bib` in default-directory (device-aware).
- PDFs: `~/wspace/src/zotero-kbase/storage`.
- Keybindings: `C-c r c` to open note/resource.
- Integration: Org-roam-bibtex for bib notes; Org-noter for PDF annotations.

### Tags and Snippets
- Tags: `org/filetags.org` for dynamic tagging, `C-c f t` to add.
- Snippets: Yankpad with Yasnippet, `C-c y` to insert.

### UI and General Settings
- Theme: tsdh-dark.
- Hooks: Auto-update LAST_MODIFIED on save; timestamp insertion.
- Backups: Device-specific in `.backups/` and `.autosaves/`.

---

## 🧪 Testing and Troubleshooting

### Test Setup
- Open Emacs—check `*Messages*` for "✅ config.el loaded successfully on device: [device]".
- Create a note: `C-c r n` (Org-roam capture).
- Add citation: `M-x citar-insert-cite` (bind if needed).
- Annotate PDF: Open PDF, `M-x org-noter`.

### Common Issues
- **Path Errors**: Verify default-directory (`M-: default-directory`) matches your device (e.g., "~/wspace/org/notes/" for laptop). Fix in device.el.
- **Packages Not Installed**: Run `M-x straight-pull-all` on first start.
- **Org-Roam Db Errors**: Delete `~/.emacs.d/org-roam.db`, run `M-x org-roam-db-sync`.
- **Debug**: Set `export MY_DEBUG_DEVICE=1` for detailed logs in `*Messages*`.

### Customization
- Edit templates in `~/.emacs.d/template/` (e.g., add headers to `generic-note.org`).
- Add more capture templates in `config.org` for specific workflows (e.g., "e" for engineering notes).
- Extend for Consult/Ivy if needed (currently commented in workflows.org).

---

Happy hacking! Open an issue on GitHub for questions.
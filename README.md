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
  - Git for cloning the repo (`sudo apt install git` on Ubuntu).
  - Aspell for spell-checking (`sudo apt install aspell`).
  - Zotero with Better BibTeX add-on for bibliography integration (optional, for Citar/Org-roam-bibtex).
  - For general LaTeX/PDF export: TeX Live (`sudo apt install texlive-full`) and latexmk (`sudo apt install latexmk`).
  - No Internet for Packages: `straight.el` bootstraps packages, requiring an initial internet connection. Subsequent launches use local copies.

### Step 1: Clone the Repository

```bash
git clone https://github.com/chaicurioquest/emacs-config.git ~/.emacs.d
```

- **Why ~/.emacs.d?**: This is the default Emacs configuration directory. The config is self-contained.

### Step 2: Customize device.el

The config uses `device.el` to detect device type (laptop, Termux, tablet) and adjust paths.

- Edit `~/.emacs.d/device.el`:
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

- **Get system-name**: Run `M-: system-name` in Emacs.
- **Save and Restart**: Restart Emacs to apply changes.

### Step 3: Set Up Zotero Integration (Optional)
For bibliography and citations (Citar, Org-roam-bibtex):
- Install Zotero and Better BibTeX add-on.
- Export library as Better BibLaTeX to `~/wspace/org/notes/references.bib` (adjust for other devices).
- Enable automatic export in Zotero: Preferences > Better BibTeX > Automatic Export ("On Change").
- PDFs: `~/wspace/src/zotero-kbase/storage` (update in `config.org` if needed).

### Step 4: Install Packages and Tangle Config
- Start Emacs—`straight.el` auto-installs packages (initial run may take time).
- Manually tangle: Open `config.org` (`C-x C-f ~/.emacs.d/config.org`), press `C-c t` (or `M-x my-tangle-config-org`).

### Step 5: Customize Templates and Setup Files
- **Templates**: In `~/.emacs.d/template/` (e.g., `roam-default.org`, `generic-note.org`). Edit to add headers/metadata.
- **Setup Files**: In `~/wspace/org/notes/latex/` (e.g., `setup-latex.org`, `acronyms.org`). Update `setup-latex.org`:
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
- **Acronyms**: Edit `acronyms.org` with terms (e.g., BJT, JFET).

### Step 6: Enable Debug Logs (Optional)
- Set environment variable: `export MY_DEBUG_DEVICE=1` (add to `.bashrc`).
- View logs in `*Messages*` buffer.

### Step 7: Optional LaTeX Export Support
For users needing LaTeX/PDF export:
- Tangle/load `org/latextn.org` to `org/latextn.el` (enabled by default in `config.org`).
- Dependencies:
  - TeX Live (`sudo apt install texlive-full`).
  - latexmk (`sudo apt install latexmk`).
- Usage: Enables LaTeX export (`C-c C-e l p`) with latexmk, bibliography support, and device-aware paths.

### Step 8: Optional VLSI Diagram Support (TikZ/CircuitTikZ)
For circuit diagrams (e.g., VLSI engineers):
- Tangle/load `org/customxtn.org` to `org/customxtn.el` by uncommenting lines in `config.org`'s "* ACTIVE Modular Configs".
- Dependencies:
  - TeX Live with `tikz` and `circuitikz` (`sudo apt install texlive-full texlive-science`).
  - ImageMagick (`sudo apt install imagemagick`; enable PDF/PS in `/etc/ImageMagick-6/policy.xml`: change `rights="none"` to `rights="read|write"` for "PDF", "PS", "EPS").
  - Ghostscript (`sudo apt install ghostscript`).
  - Ditaa (`sudo apt install ditaa` for `/usr/share/ditaa/ditaa.jar`).
- Usage: Enables LaTeX and ditaa Babel blocks, previews for TikZ/CircuitTikZ, ditaa output to `images/`.

*NOTE*: ImageMagick-based LaTeX preview for TikZ / CircuitTikZ

Security note (Ubuntu/Debian):
- Recent ImageMagick packages disable PDF processing by default.
  To allow PDF→PNG conversion, you must locally enable PDF coder in ImageMagick's policy:
  Edit /etc/ImageMagick-6/policy.xml or /etc/ImageMagick-7/policy.xml and change:
    <policy domain="coder" rights="none" pattern="PDF" />
  to:
    <policy domain="coder" rights="read|write" pattern="PDF" />
  (same for "PS"/"EPS" if present). This is a local security decision — enabling PDF processing allows Ghostscript to be invoked and may increase attack surface. Do this only if you trust the LaTeX inputs.

Alternative (safer): install pdftocairo (poppler-utils) and use that instead of ImageMagick — no policy changes required.

---

## 📦 Configurations

### General Configurations
- **Startup Optimizations**: Inhibits startup screen, resets garbage collection, disables UI elements.
- **Package Management**: `straight.el` for installs; `use-package` for declarations. Packages: Org-roam, Citar, PDF-tools, Org-noter, Vertico, Orderless, Yasnippet, Yankpad, Flyspell, AUCTeX, RefTeX, Ace-Window, Org-agenda.
- **Keybindings**: `C-c t` (tangle), `C-c i t/d/o` (timestamps), `C-c r n` (Org-roam capture), `C-c a` (Org-agenda), `C-c f t` (tagging).

### Org-Roam for Note-Taking
- In `org/roam.org`: Zettelkasten with templates (default, fleeting, permanent, journal, bib).
- Dailies in `roam/daily/`.
- Keybindings: `C-c r f` (find), `C-c r n` (capture), `C-c r d` (dailies), `C-c r g` (graph UI, laptop).
- Backlinking: Manual `:ID:` or auto in templates.
- Tags: `C-c r t` (add), `C-c r r` (remove) via `.filetags`, `.filetags-org`.

### Citar for Bibliography/Zotero
- Bib file: `references.bib` in default-directory.
- PDFs: `~/wspace/src/zotero-kbase/storage`.
- Keybindings: `C-c r c` (open note/resource).
- Integration: Org-roam-bibtex, Org-noter for annotations.

### Tags and Snippets
- Tags: `org/filetags.org`, `C-c f t` to add (uses `.filetags`, `.filetags-org`).
- Snippets: Yankpad with Yasnippet, `C-c y` to insert (`org/yankpad.org`).

### UI and General Settings
- Theme: tsdh-dark.
- Hooks: Auto-update LAST_MODIFIED; timestamp insertion.
- Backups: `.backups/`, `.autosaves/` (git-ignored).
- Git Sync: `C-c g p` (pull), `C-c g u` (push).

### GTD Workflow (org-alert)
- In `org/workflow.org`: Device-specific alerts (libnotify on laptop, termux-notification on mobile).
- Integrates with Org-agenda.

### Mail Integration
- In `org/mail.org`: mbsync for Gmail, msmtp for sending.
- Keybindings: `C-c C-c` in mu4e-view for Org capture.
- Alerts via mu4e-alert.

### LaTeX Export
- In `org/latextn.org`: LaTeX/PDF export with latexmk, bibliography support, device-aware paths.
- Keybindings: `C-c C-e l p` (export PDF).

---

## 🧪 Testing and Troubleshooting

### Test Setup
- Check `*Messages*` for "✅ config.el loaded successfully on device: [device]".
- Note: `C-c r n`.
- Citation: `C-c i c`.
- PDF annotation: `M-x org-noter`.
- PDF export: `C-c C-e l p`.
- VLSI test (if enabled): Run CircuitTikZ block with `C-c C-c`, preview with `C-c C-x C-l`.

### Common Issues
- **Path Errors**: Verify `default-directory` (`M-: default-directory`) matches device (e.g., "~/wspace/org/notes/"). Fix in `device.el`.
- **Packages Not Installed**: `M-x straight-pull-all`.
- **Org-Roam Db Errors**: Delete `~/.emacs.d/org-roam.db`, run `M-x org-roam-db-sync`.
- **Debug**: `export MY_DEBUG_DEVICE=1` for logs in `*Messages*`.
- **LaTeX/VLSI Issues**: Ensure TeX Live, latexmk (`latextn.org`), or additional dependencies (`customxtn.org`) are installed.

### Customization
- Edit templates in `~/.emacs.d/template/` (e.g., `generic-note.org`).
- Add capture templates in `orgxtn.org` (e.g., "e" for engineering).
- Enable Consult/Ivy (commented in `config.org`).

Happy hacking! Open an issue on GitHub for questions.

[Download README.md](data:text/markdown;base64,...)
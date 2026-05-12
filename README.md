# emacs-config

Modular, literate Emacs configuration for **laptop (Ubuntu/Debian)** and
**Android (Termux)**. Built around a structured IC-design and VLSI research
workflow: PDF annotation, Zotero bibliography, Zettelkasten notes, and LaTeX
export — all from Org mode.

**Emacs 30+ · Branch: `testing`**

---

## What This Configuration Is

This is not a starter kit. It is a personal, opinionated configuration for a
specific research and writing workflow:

- Reading and annotating technical PDFs (datasheets, papers, Vivado UGs) with `org-noter`
- Maintaining a structured IC-design knowledge base in Org files
- Writing and exporting LaTeX documents with bibliography, acronyms, and glossaries
- Capturing and refiling notes, citations, and tasks across multiple devices

If that matches your use case, this gives you a working baseline. The modular
structure makes it easy to take only what you need.

---

## Repository Layout

```
~/.emacs.d/
├── config.org           # Master literate config — single source of truth
├── config.el            # Tangled from config.org — do not edit directly
├── device.el            # Device detection: laptop / termux / tablet / phone
├── private.el           # Identity only (name, email, mu4e creds) — git-ignored
├── early-init.el        # UI suppression + GC tuning — loads before packages
│
├── org/
│   ├── keymaps.org      # All custom keybindings → keymaps.el
│   ├── orgxtn.org       # Org extensions → orgxtn.el
│   ├── notextn.org      # PDF/noter/Calibre extensions → notextn.el
│   ├── roam.org         # Org-Roam setup → roam.el
│   ├── filetags.org     # Tag taxonomy → filetags.el
│   ├── workflow.org     # GTD / org-alert → workflow.el
│   ├── latextn.org      # LaTeX/AUCTeX/CDLaTeX → latextn.el
│   ├── engxtn.org       # Engineering extensions → engxtn.el
│   ├── mail.org         # mu4e (laptop only) → mail.el
│   └── inbox.org        # Capture landing zone (runtime, not config)
│
├── template/
│   ├── generic-note.org   # Template for C-c c → [n] KB files
│   ├── sidecar-note.org   # Template for C-c c → [s] sidecar files
│   ├── roam-default.org   # Roam default capture template
│   ├── roam-fleeting.org  # Fleeting note template
│   ├── roam-permanent.org # Permanent note template
│   ├── roam-journal.org   # Journal entry template
│   ├── roam-dailies.org   # Daily note template
│   └── roam-bib.org       # Literature note template (Citar integration)
│
└── latex/               # LaTeX support files for export
    ├── setup-latex.org  # Shared #+SETUPFILE for all exportable Org files
    ├── gen-acronyms.tex # Auto-generated — do not edit
    └── tech-acronyms.tex# Auto-generated — do not edit
```

> **Rule**: always edit `.org` source files, never the generated `.el` files
> directly. Generated files are overwritten on the next tangle (`C-c t` / `C-c T`).

---

## Prerequisites

| Requirement | Purpose | Install (Ubuntu/Debian) |
|---|---|---|
| Emacs 30+ | Core | `sudo apt install emacs` |
| Git | straight.el package management | `sudo apt install git` |
| Hunspell | Spell checking | `sudo apt install hunspell` |
| TeX Live + latexmk | LaTeX/PDF export | `sudo apt install texlive-full latexmk` |
| ripgrep (`rg`) | `C-c k` full-text search | `sudo apt install ripgrep` |
| poppler-utils | Full-page PDF capture (`pdftoppm`) | `sudo apt install poppler-utils` |
| Zotero + Better BibTeX | Bibliography (optional) | [zotero.org](https://www.zotero.org) |
| Calibre | Book management, laptop only (optional) | `sudo apt install calibre` |
| mbsync + msmtp | Email, laptop only (optional) | `sudo apt install isync msmtp` |
| Piper TTS + aplay | PDF text-to-speech (optional) | see `notextn.el` for model paths |

Internet is required only for the **first run** (straight.el bootstraps
packages). All subsequent startups are fully offline.

---

## Installation

### 1 — Clone

```bash
git clone https://github.com/chaicurioquest/emacs-config.git ~/.emacs.d
cd ~/.emacs.d && git checkout testing
```

### 2 — Configure `device.el`

`device.el` controls all device-specific paths. Edit it before starting Emacs:

```elisp
(defvar my-device-configs
  (let ((table (make-hash-table :test 'equal)))
    (puthash "your-laptop-hostname" 'laptop table)  ; get via M-: (system-name)
    (puthash "termux"               'termux table)
    table))

(defvar my-device
  (or (gethash system-name my-device-configs)
      (if (string-match "termux" system-configuration) 'termux 'laptop)))

(provide 'device)
```

Device-dependent paths set automatically from `my-device`:

| Path variable | Laptop | Termux / tablet |
|---|---|---|
| `my/notes-root-dir` | `/wspace/org/` | `~/org/` |
| `bib-path` | `<notes>/bib/references.bib` | same relative |
| `org-roam-directory` | `<notes>/roam/` | same relative |
| `my-citar-library-paths` | `/wspace/src/zotero-kbase/storage` | adjust |
| `my-calibre-library-dir` | `/wspace/src/calibre-ebooks` | N/A |

All paths are derived from `my/notes-root-dir` — change that one variable to
relocate everything.

### 3 — Create `private.el`

Create `~/.emacs.d/private.el` (git-ignored). Contains only identity:

```elisp
(setq user-full-name  "Your Name"
      user-mail-address "you@example.com")
;; mu4e credentials go here too (laptop only)
```

If missing, Emacs starts cleanly with a warning — nothing breaks.

### 4 — First Emacs Start

```bash
emacs
```

straight.el bootstraps on first run (~2–5 minutes with internet). Watch
`*Messages*` for:

```
Device: laptop
Default directory: /wspace/org/
Tangled: org/keymaps.org → org/keymaps.el
...
```

If `*Messages*` shows `my-device not set → defaulting to 'laptop`, your
hostname is not in `device.el` — add it and restart.

### 5 — Tangle the Config

The `.el` files are generated from `.org` sources. Tangle once after cloning:

```
C-x C-f ~/.emacs.d/config.org   (open config.org)
C-c T                            (tangle all modular files)
```

Then restart Emacs. Subsequent tangles are incremental — only files newer than
their `.el` output are retangled.

### 6 — Zotero Setup (Optional)

1. Install [Zotero](https://www.zotero.org) + [Better BibTeX](https://retorque.re/zotero-better-bibtex/).
2. Export library: **File → Export Library → Better BibLaTeX** → save to `bib-path`.
3. Enable auto-export: **Preferences → Better BibTeX → Automatic Export → On Change**.
4. PDFs stored in `my-citar-library-paths` (set in `device.el`).

---

## Custom Keybinding Prefixes

| Prefix | Domain |
|---|---|
| `C-c i` | Citations, PDF, org-noter, timestamps, refile, glossary |
| `C-c r` | Org-Roam: notes, dailies, tags, graph, transclusion |
| `C-c g` | Git: pull (`p`) and push (`u`) for Org repo |
| `C-c f t` | Set filetags with completion |
| `C-c m` | mu4e mail (laptop only) |
| `C-c t` | Tangle current config file |
| `C-c T` | Tangle all modular config files |
| `C-c v` | Open corresponding PDF in vertical split |
| `C-c a` | Org agenda |
| `C-c c` | Org capture |
| `C-c h` | Search headings across all agenda files |
| `C-c k` | Full-text search with ripgrep |
| `C-c b` | Switch buffer |
| `C-c R` | Open recent file |
| `C-c O` | Open recent `.org` file |
| `M-o` | Jump to any window (ace-window) |
| `C-x g` | Magit status |
| `C-z` | Undo |

---

## Package Architecture

All packages are managed by [straight.el](https://github.com/raxod502/straight.el)
with `use-package` for declaration. Packages load lazily (`:defer t`) or on
hooks for fast startup.

| Package | Role |
|---|---|
| `org` (built-in) | Notes, tasks, export, Babel |
| `org-roam` | Zettelkasten with backlinks, dailies, graph |
| `citar` + `citar-org-roam` | Bibliography UI — Zotero ↔ Org |
| `org-noter` + `org-noter-pdftools` | Synchronized PDF annotation |
| `pdf-tools` | PDF rendering in Emacs |
| `consult` + `vertico` + `orderless` | Completion and incremental search |
| `embark` | Contextual actions on completions |
| `magit` | Git interface |
| `cdlatex` + `auctex` + `reftex` | LaTeX authoring in Org and `.tex` files |
| `org-glossary` | Acronyms and glossaries for Org + LaTeX export |
| `mu4e` + `mu4e-alert` | Email (laptop only) |
| `calibredb` | Calibre book management (laptop only) |
| `ace-window` | Label-based window switching (`M-o`) |
| `winner-mode` (built-in) | Window layout undo/redo |
| `flyspell` + `flyspell-correct` | Spell checking (`M-$`) |
| `yasnippet` + `yankpad` | Snippet expansion in Org |
| `org-transclusion` | Embed content from other Org nodes |
| `org-roam-ui` | Interactive graph (laptop only) |

---

## Important Design Decisions

**Tangle is incremental.** `my/tangle-if-needed` compares `.org` vs `.el`
mtimestamps — only files that changed are retangled. `C-c T` is safe to run
at any time.

**Agenda file scanning is dynamic.** `my/update-agenda-files` scans
`my/notes-root-dir` (top-level `.org` files) and `roam/` (recursive) on
startup and after every Org save. Excluded dirs: `build`, `ltximg`, `images`,
`.attach`, `.autosaves`, `.backups`, `bib`, `latex`, `.git`. No hardcoded
file list to maintain.

**org-alert uses a timer, not `org-alert-enable`.** `(org-alert-enable)` opens
every agenda file at startup — never use it. Use
`run-with-timer interval interval #'org-alert-check` instead. Files opt in
individually with `#+ALERT: yes` in the header.

**Every PDF opens on the right.** `display-buffer-alist` routes all
`pdf-view-mode` buffers to a right-side vertical split, regardless of how
they are opened (link, `C-x C-f`, org-noter, Calibre, or export preview).

**Glossary `.tex` files are auto-generated.** Edit only
`glossary/gen_acronyms.org` and `glossary/tech_acronyms.org`. The `.tex` files
in `latex/` regenerate automatically on `C-x C-s` via `my/org-glossary-sync-tex`.
Duplicate keys between `* Acronyms` and `* Glossary` sections abort generation
with a warning.

**Org-noter uses relative paths.** `notextn.el` advises `org-noter--add-doc`
to store `NOTER_DOCUMENT` as a relative path when the sidecar and PDF are in
the same directory tree — making notes portable across machines.

---

## Device-Specific Features

| Feature | Laptop | Termux (Android) | Tablet |
|---|---|---|---|
| mu4e mail (`C-c m`) | ✓ | — | — |
| Org-Roam graph UI (`C-c r g`) | ✓ | — | — |
| Calibre integration (`C-c i b`) | ✓ | — | — |
| Calibre auto-bib export | ✓ | — | — |
| LaTeX/PDF export | ✓ | — | ✓ |
| PDF annotation (org-noter) | ✓ | limited | ✓ |
| Piper TTS (`C-c C-v s/p`) | ✓ | — | — |
| Git sync (`C-c g u`) | ✓ | ✓ | ✓ |

---

## Customising This Config

| Task | How |
|---|---|
| Add a capture template | Edit `org/orgxtn.org` → add to `org-capture-templates` → `C-c t` |
| Add a Roam template | Edit `org/roam.org` → add to `org-roam-capture-templates` → `C-c t` |
| Add a refile target | `M-x my-org-add-tag-target` on the target heading |
| Add a note snippet | Edit `yankpad.org` in your notes root |
| Change a keybinding | Edit `org/keymaps.org` → `C-c t` to retangle |
| Add a new device | `(puthash "hostname" 'device-type table)` in `device.el` |
| Add an acronym/glossary entry | Edit `glossary/gen_acronyms.org` or `glossary/tech_acronyms.org` |

---

## Troubleshooting

| Symptom | Fix |
|---|---|
| `my-device not set → defaulting to 'laptop` | Add hostname to `device.el`; check `M-: (system-name)` |
| Wrong notes path | Verify `M-: my/notes-root-dir`; fix `device.el` |
| Packages not installed | `M-x straight-pull-all` |
| Org-Roam DB errors | `C-c r s` (`my/org-roam-safe-rebuild`) |
| LaTeX export fails | Check `texlive-full` and `latexmk` are installed |
| Glossary `.tex` not updating | Check `*Messages*` for duplicate-key warning; fix `.org` source |
| Alert fires at startup, opens files | Remove `(org-alert-enable)`; use timer pattern (see `workflow.org`) |
| Emacs crashed, lock file prompt | `M-x recover-this-file`; press `s` at lock prompt |
| Config change not taking effect | `C-c t` to retangle → `M-x load-file ~/.emacs.d/config.el` |
| `*Messages*` full of `ORG-OPEN` traces | Uncommented diagnostic `advice-add` in `config.el` — re-comment it |
| Debug device/path detection | `MY_DEBUG_DEVICE=1 emacs` |

---

## Syncing Across Devices

- **Git** (`C-c g p` pull / `C-c g u` push): config files and Org KB files.
- **Syncthing** (recommended for notes): real-time sync of `roam/`, `bib/`,
  `archive/` between devices without going through GitHub.

The `.gitignore` excludes: `.backups/`, `.autosaves/`, `org-roam.db`,
`private.el`, generated `.tex` files, and `*.org_archive` files.

---

## License

MIT. Open issues or PRs on GitHub for feedback.

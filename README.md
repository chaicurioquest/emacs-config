# emacs-config
My Emacs Configurations on Laptop and Android Devices

# Emacs Configuration Setup

This repository contains a modular and portable Emacs configuration designed for:

* Multi-device support (laptop, Termux, etc.)
* Fast startup (using `early-init.el`)
* Literate config via `config.org`
* Declarative package management with `straight.el`

---

## 🗂 Directory Structure

```text
~/.emacs.d/
├── config.org          # Main Org-mode configuration file
├── config.el           # Auto-generated from config.org
├── init.el             # Bootstrapper: tangles config.org if needed
├── early-init.el       # Pre-GUI startup performance and UI tweaks
├── device.el           # Shared device detection logic
├── org/                # Optional additional .org config snippets
└── backups/            # Emacs backup directory
```

---

## 🔄 Usage Instructions

### 1. Clone and Use This Config

```bash
git clone https://github.com/yourusername/emacs-config.git ~/.emacs.d
```

### 2. Customize `device.el`

Replace placeholders with your actual system names:

```elisp
(puthash "my-laptop" 'laptop table)  ;; Replace with output of M-x eval-expression RET system-name
```

### 3. (Optional) Enable Debug Logs

```bash
export MY_DEBUG_DEVICE=1  # Add to shell profile to persist
```

### 4. Start Emacs

Emacs will:

* Load `early-init.el` to optimize startup
* Run `init.el`, which tangles `config.org` if `config.el` is missing
* Load `config.el`

You can manually tangle config with:

```elisp
M-x my-tangle-config-org
```

---

## 📦 Package Management

This config uses `straight.el`:

* Reproducible, version-pinned
* Packages declared via `use-package`

---

## 🧠 Device Awareness

Both `early-init.el` and `config.org` use:

```elisp
my-device => 'laptop | 'termux | 'generic
```

Paths, UI, and keybindings adapt per device.

---

## 🛠 Recommended Setup for Syncthing

To sync config across devices:

* Exclude `config.el`, `.elc` files, `backups/`:

### .stignore

```
config.el
*.elc
backups/
straight/
elpa/
auto-save-list/
```

---

## 🧪 GitHub Actions (optional)

To auto-tangle and validate config changes:

* Create `.github/workflows/tangle.yml`

```yaml
name: Tangle Config
on:
  push:
    paths:
      - 'config.org'
      - 'org/**.org'

jobs:
  tangle:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - name: Set up Emacs
      run: sudo apt install emacs
    - name: Run Emacs tangler
      run: emacs --batch -l init.el
```

---

## ✅ Credits

* `early-init.el` for startup performance
* `init.el` as minimal tangler/loader
* `config.org` for literate, versioned customization

---

## 📬 Questions?

Open an issue or ping [@chaicurioquest](https://github.com/chaicurioquest).

Happy hacking!

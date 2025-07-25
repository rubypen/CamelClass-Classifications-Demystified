# 🐫 CamelClass: Classifications Demystified

As a group project, for our 3110 Final Project, we built an interactive K-Means Classification Visualizer using **OCaml**. The project includes both a GUI and an I/O version, allowing users to classify 1D, 2D, 3D, and higher-dimensional points with K-Means. Users can upload their own CSV files or run the program on built-in or randomly generated datasets. The GUI supports dynamic data visualization and statistics, while the I/O version includes additional support for **k-Nearest Neighbors (k-NN)**.

---

## 👩‍💻 What I Worked On

I directly worked on:

- Developing the full I/O interface, integrating K-Means and k-NN for interactive classification and data analysis
- Enhancing I/O usability and aesthetics, including real-time feedback and CSV output support
- Leading planning and communication efforts across platforms like Slack to keep the team organized and on-track
- Debugging various parts of the codebase and supporting integration across features
- Helping distribute tasks and maintain clear development workflows

---

## 🗂️ Example Files

Example CSV files for 1D, 2D, and 3D datasets can be found in the `data/` folder, including:

- `test_data.csv`
- `test_data_2d.csv`
- `test_data_3d.csv`

---

## 🛠️ Installation & Running

To build and run the project, follow these steps:

### 1. Install Dependencies

```bash
opam update
opam upgrade
opam install csv ansiterminal batteries
opam install lablgtk3
opam install plplot

```

When prompted, choose `y` for yes and option `1` for Homebrew (on Mac) when applicable.

### 2. Fix Library Paths (if needed)

If you get path-related errors when linking:

```bash
export PKG_CONFIG_PATH=/opt/homebrew/Cellar/pango/1.55.0/lib/pkgconfig:/opt/homebrew/lib/pkgconfig
export LIBRARY_PATH=/opt/homebrew/Cellar/pango/1.55.0/lib:/opt/homebrew/lib
export DYLD_LIBRARY_PATH=/opt/homebrew/Cellar/pango/1.55.0/lib:/opt/homebrew/lib

```

### 3. Build the Project

```bash
dune build

```

### 4. Run the Program

```bash
dune exec bin/main.exe

```

---

## ✅ Notes

You may see harmless warnings like:

```
ld: warning: ignoring duplicate libraries: '-lcairo'

```

These are expected due to overlapping OCaml GUI dependencies and do not affect functionality.

# Smart Rooftop Micro-Farm
### Crop Rotation, Space & Resource Planning System

A local, offline, rule-based Shiny web application for planning a rooftop
vegetable garden: what to grow, where, when, and whether your rooftop's
space, water and sunlight can actually support it.

**No API. No database server. No machine learning.** Everything runs from
one local CSV file (`data/crops.csv`) and plain R logic.

---

## 1. How to run it

1. Open the `SmartRooftopFarm` folder as a project in RStudio (or just set
   it as your working directory).
2. Make sure the `shiny` package is installed: `install.packages("shiny")`
3. Open `app.R` and click **Run App**, or from the console:
   ```r
   setwd("path/to/SmartRooftopFarm")
   shiny::runApp()
   ```
4. The app opens in your browser / RStudio viewer, starting on the Home page.

No internet connection is required at any point.

---

## 2. Project architecture

```
SmartRooftopFarm/
├── app.R                     # entry point: sources everything, launches the app
├── ui.R                      # one builder function per wizard page + the app shell
├── server.R                  # application state (rv) + all render*/observeEvent logic
│
├── data/
│   └── crops.csv             # local crop knowledge base (18 crops, 20 attributes each)
│
├── www/
│   ├── style.css              # all visual styling
│   ├── script.js               # tiny UX touch (scroll to top on page change)
│   └── images/                 # one local image per crop (see section 6)
│
├── R/
│   ├── crop_data.R             # 4a-4e: CSV -> R list, access, add, modify, length()
│   ├── rooftop_calculations.R  # total area / farming area arithmetic
│   ├── zone_functions.R        # dynamic zone list (add / modify / remove / length)
│   ├── resource_analysis.R     # space / water / sunlight / duration comparisons
│   ├── rotation_logic.R        # crop-rotation reasoning + companion-crop checks
│   ├── zone_assignment.R       # smart crop -> zone matching algorithm
│   └── schedule.R              # planting/harvest date calculations
│
└── README.md
```

**Why both `app.R` and `ui.R`/`server.R`?** Shiny normally auto-detects
either style. Here `ui.R` and `server.R` are plain R scripts that define an
`app_ui` object and a `server` function (not the literal names `ui`/`server`
that trigger Shiny's auto-loading), and `app.R` explicitly sources every
file and calls `shinyApp(ui = app_ui, server = server)`. This keeps the
"one file, one job" structure you asked for, while avoiding the ambiguity of
having both loading styles active at once.

---

## 3. Page-by-page functionality

The app is a 9-step wizard. A fixed top bar shows a progress bar and every
step's name (past steps marked done, current step highlighted). A fixed
bottom bar has **Back** / **Next** (or **Start Planning** / **Start Over**
at the two ends). Moving between pages never loses data — every value the
user enters is written into a central `rv` (reactiveValues) state object in
`server.R` as soon as it changes, not only when Next is clicked.

1. **Home** — explains the problem, how the system works, and feature cards.
2. **Rooftop Setup** — length, width, % usable for farming, daily water,
   number of zones, average sunlight hours. Total area and farming area are
   calculated live with real R arithmetic (`R/rooftop_calculations.R`).
3. **Growing Zones** — add any number of zones (not fixed to A/B/C/D). Each
   zone has its own dimensions, sunlight, water, soil type and *previous
   crop* (this last field is what powers the rotation logic later). Zones
   are stored as a dynamic R list (`R/zone_functions.R`).
4. **Crop Selection** — a searchable, filterable grid of image cards from
   the local crop database. Click a card to select/deselect it (multi-select).
5. **Crop Details** — a full spec sheet for every crop you selected, pulled
   live from `crops.csv` (nothing hardcoded per crop).
6. **Resource Analysis** — compares required vs. available space, water,
   sunlight and growing duration, with ✓ / ⚠ / ✗ statuses computed from your
   actual numbers, plus a companion/incompatible-crop check.
7. **Crop Rotation** — for every zone, explains *why* each selected crop is
   Suitable / Less suitable / Not suitable given that zone's previous crop,
   plus a Smart Zone Assignment that matches each crop to its best zone.
8. **Smart Plan** — a rooftop layout that is generated from your actual zone
   count/sizes (not one fixed image), a planting & harvest schedule with
   real dates, a rotation timeline, and a resource-utilization dashboard.
9. **Final Report** — one scrollable summary combining all of the above,
   ending in plain-English status sentences (never a made-up score).

---

## 4. Data model

### 4.1 `data/crops.csv`
One row per crop, 20 columns: id, name, category, scientific name, min/preferred
sunlight, water requirement (L/day for a standard planting), space requirement
(m² for a standard planting), min/max temperature, growth duration, harvest
duration, expected yield, root type, crop family, rotation group, companion
crops (`;`-separated), incompatible crops (`;`-separated), suitable season,
difficulty. 18 crops are included across vegetables, root crops, legumes,
leafy greens, bulbs and herbs — easy to extend by adding more rows.

### 4.2 R LIST usage (Week 4 requirement)
This is implemented in `R/crop_data.R` and `R/zone_functions.R` and used as
real application logic, not a standalone demo:

| Requirement | Where | What it does |
|---|---|---|
| 4a. CSV → list | `load_crop_list()` | Reads `crops.csv`, loops over every row, and builds `crops_list`, a **list of named lists** (one list per crop) |
| 4b. Access by index/name | `get_crop_by_id()`, `get_crop_by_name()` | Looks a crop up either by numeric id or by the name attached via `names(crops_list)` |
| 4c. Add a record | `add_zone()` (used live, every time a user adds a growing zone) and `add_crop()` (available for extending the crop database) | Appends a new named-list element at the end of the list |
| 4d. Modify a record | `modify_zone()`, `modify_crop_field()` | Replaces one element (or one field of one element) of the list in place |
| 4e. `length()` | used throughout — zone count on the Zones page, crop count on Selection, "N crops checked" on Resource Analysis, loops in the rotation/assignment/schedule logic | Drives real UI text and loop bounds, not just printed once |

Zones (`rv$zones`) are a second, independent list built the same way, so the
app genuinely has two live, user-editable list structures at once.

---

## 5. Core calculations (all rule-based, no ML)

- **Space**: `sum(selected crop space requirements)` vs. farming area.
- **Water**: `sum(selected crop water requirements)` vs. daily water input;
  if there's a shortfall, the top 3 thirstiest selected crops are named.
- **Sunlight**: each crop's minimum sunlight need vs. the best sunlight
  level among your zones (`R/resource_analysis.R`), and per-zone in the
  Smart Zone Assignment (`R/zone_assignment.R`).
- **Rotation**: same crop → "Not suitable"; same crop family or rotation
  group as the zone's previous crop → "Less suitable"; otherwise →
  "Suitable" (`R/rotation_logic.R`). Every verdict comes with a reason.
- **Companion planting**: pairwise check of every two selected crops against
  each other's `companion_crops` / `incompatible_crops` fields.
- **Zone assignment**: a greedy matcher that scores every zone for every
  crop (remaining space, sunlight fit, rotation history) and assigns the
  best-fit zone, reducing that zone's remaining space as crops are placed.
- **Schedule**: `Sys.Date() + growth_days`, `+ harvest_days`, `+ rotation_gap`
  — real R Date arithmetic, no hardcoded calendar dates.

---

## 6. About the crop images

Each crop has its own local image at `www/images/<crop_name>.svg` — a small
vector icon generated entirely offline (no image API, no internet). This
satisfies the "one real local image per crop, no API" requirement while
keeping the project self-contained.

**To use real photographs instead:** just drop a `.jpg`/`.jpeg`/`.png` file
into `www/images/` with the crop's name in lowercase and underscores instead
of spaces — e.g. `bell_pepper.jpg` for "Bell Pepper" — and the app will pick
it up automatically (`crop_image_path()` in `R/crop_data.R` checks for a
real photo before falling back to the vector icon). No other code changes
are needed.

---

## 7. Extending the project

- **Add a crop**: add a row to `data/crops.csv` (and optionally an image
  with the matching filename) — no code changes needed.
- **Add a crop via code** (to explicitly exercise 4c/4d during a viva):
  call `add_crop(rv$crops_list, list(name=..., category=..., ...))` or
  `modify_crop_field(rv$crops_list, id, "water_l_day", 20)`.
- **Change the planning horizon**: `analyze_duration(crops, horizon_days = 120)`.
- **Change the rotation "rest" period**: `build_schedule(crops, rotation_gap_days = 7)`.

# linkeR: Linking Interactive Plots and Tables in Shiny Applications

**ALWAYS follow the instructions in this file first and fallback to additional search and context gathering only if the information in the instructions is incomplete or found to be in error.**

## About linkeR

linkeR is an R package that provides effortless linked views for Shiny applications. It enables synchronized, interactive dashboards where one click updates multiple components (maps, tables, charts). The package supports Leaflet maps, DT data tables, sf spatial objects, and custom components.

## Working Effectively

### Environment Setup
Install R and essential system dependencies:
```bash
sudo apt update && sudo apt install -y r-base r-base-dev build-essential libcurl4-openssl-dev libssl-dev libxml2-dev
```

Install core R packages:
```bash
sudo apt install -y r-cran-devtools r-cran-testthat r-cran-remotes r-cran-shiny r-cran-magrittr r-cran-dt r-cran-sf r-cran-knitr r-cran-rmarkdown r-cran-later r-cran-lintr
```

### Package Development Workflow

#### Install Dependencies and Build Package
```bash
cd /path/to/linkeR
sudo R -e "library(devtools); install()"
```
Build time: **~3 seconds**. NEVER CANCEL - set timeout to 30+ seconds.

#### Run Tests
```bash
cd /path/to/linkeR
R -e "library(devtools); test()"
```
Test time: **~3 seconds**. Results: 120+ passing tests (6 tests fail without leaflet package, which is expected).
NEVER CANCEL - set timeout to 30+ seconds.

#### Package Check (Full Validation)
```bash
cd /path/to/linkeR
R -e "library(devtools); check()"
```
Check time: **~6 seconds**. NEVER CANCEL - set timeout to 60+ seconds.
This builds the package, creates vignettes, and runs R CMD check validation.

#### Linting
```bash
cd /path/to/linkeR
R -e "library(lintr); lint_package()"
```
**Note:** The current .lintr configuration may have formatting issues. If linting fails, the package code still follows good practices.

## Testing and Validation

### Core Functionality Testing
Test the package works without optional dependencies:
```bash
cd /path/to/linkeR
# Test basic package loading
R -e "library(linkeR); cat('Package loads correctly\n')"

# Test registry creation (core functionality)
R -e "library(linkeR); registry <- create_link_registry(NULL); cat('Registry created successfully\n')"
```

### Running Example Applications
**Important:** Many examples require the `leaflet` package which may not be available in restricted environments.

Test basic functionality (DT tables only):
```bash
cd /path/to/linkeR
# Create a simple test without leaflet dependencies
# (See validation commands above)
```

### Manual Validation Scenarios
After making changes to linkeR:

1. **Test Core Linking**: Create two DT tables with shared ID column, link them with `link_plots()`, verify clicking one table highlights the corresponding row in the other.

2. **Test API Functions**: 
   - Verify `create_link_registry()` creates a registry
   - Verify `register_dt()` registers tables correctly
   - Test error handling for missing packages

3. **Test Module Integration**: Check that the registry pattern works with Shiny modules (see `inst/examples/modularized_example/`)

## Build and CI Information

### GitHub Actions
The repository has three workflow files:
- **R-CMD-check.yaml**: Runs on Ubuntu, Windows, macOS with current and old R versions
- **test-coverage.yaml**: Generates code coverage reports  
- **pkgdown.yaml**: Builds package documentation website

### Package Structure
```
linkeR/
├── R/                     # Core package functions
│   ├── simple_api.R       # Main link_plots() function
│   ├── registry.R         # Registry system for component management
│   ├── leaflet.R          # Leaflet map integration
│   ├── dt.R              # DataTable integration
│   └── imports.R         # Package imports
├── inst/examples/        # Example Shiny applications
├── tests/testthat/       # Test suite
├── vignettes/           # Package documentation
├── man/                 # Generated documentation
└── .github/workflows/   # CI/CD configuration
```

## Common Development Tasks

### Adding New Component Support
1. Create new component registration function in `R/` (follow pattern of `register_dt.R`)
2. Add observer setup functions  
3. Add tests in `tests/testthat/`
4. Update `link_plots()` in `simple_api.R` to detect new component type

### Updating Documentation
1. Edit roxygen2 comments in R files
2. Rebuild docs: `R -e "devtools::document()"`
3. Update vignettes in `vignettes/` folder
4. Regenerate README: `R -e "rmarkdown::render('README.Rmd')"`

### Before Committing Changes
Always run this validation sequence:
```bash
cd /path/to/linkeR
R -e "library(devtools); test()"     # 3 seconds
R -e "library(devtools); check()"    # 6 seconds  
```

## Dependencies and Network Limitations

### Core Dependencies (Always Available)
- shiny, magrittr, later (required)
- DT, sf, knitr, rmarkdown, testthat (suggested)

### Optional Dependencies (May Not Be Available)
- **leaflet**: Required for map functionality. Tests will skip/fail without it.
- **plotly**: For advanced plot integration  
- **bslib**: For modern Bootstrap themes

### Network Access Issues
If you encounter "cannot access" errors for CRAN repositories, this is normal in restricted environments. The package will still build and function correctly with available dependencies.

## Key Package Features

### Main API Functions
- **`link_plots()`**: One-line setup for linking components in simple apps
- **`create_link_registry()`**: Advanced registry-based approach for modular apps
- **`register_leaflet()`**: Register Leaflet maps with the linking system
- **`register_dt()`**: Register DT data tables with the linking system

### Data Requirements
- All linked components must share a common ID column
- Leaflet maps need `layerId = ~your_id_column` in markers/shapes
- DT tables work automatically with row-based selection
- sf objects are supported - coordinates extracted automatically

### Timing Expectations
- Package installation: ~3 seconds
- Full test suite: ~3 seconds  
- Package check: ~6 seconds
- Example app startup: ~2 seconds

**Critical**: Always set timeouts of 30+ seconds for any R package operations. R package builds can vary significantly in timing.

## Troubleshooting

### Common Issues
1. **"leaflet package required"**: Expected in minimal environments. Core DT functionality still works.
2. **"cannot access CRAN"**: Network restriction, does not affect local development.
3. **Linting configuration errors**: Package code quality is still maintained.

### Emergency Commands  
If tests hang or fail unexpectedly:
```bash
# Quick core functionality test
R -e "library(linkeR); cat('Package loads correctly\n')"

# Test basic registry creation
R -e "library(linkeR); registry <- create_link_registry(NULL); cat('Registry created\n')"
```

Remember: Focus on testing and validating the core linking functionality between interactive Shiny components. The package is designed to make complex multi-component dashboards simple to implement.
---
title: "linkeR: Effortless Linked Views for 'Shiny' Applications"
tags:
  - R
  - tool
  - developer-tools
authors:
  - given-names: Jake
    surname: Wagoner
    orcid: 0009-0000-5053-2281
    affiliation: "1" # (Multiple affiliations must be quoted)
affiliations:
 - name: The University of Utah, United States
   index: 1
bibliography: paper.bib
---

## Summary

`linkeR` is an R package designed to simplify the creation of interactive, multi-view dashboards within the `shiny` web application framework. When researchers explore complex datasets, they often need to view the same data through multiple lenses simultaneously. For example, selecting a geographic region on a map to instantly view the corresponding demographic data in a table. Typically, synchronizing these distinct visualizations requires developers to write and maintain complex, repetitive logic to track user interactions. `linkeR` solves this problem by providing a centralized registry system that automatically coordinates these interactive components. With minimal setup, developers can create applications where interacting with a single element seamlessly and bidirectionally updates all related charts, maps, and tables. This drastically lowers the technical barrier for scientists and researchers seeking to build robust exploratory data analysis tools.

## Statement of need

Coordinated Multiple Views is a specific visualization interaction technique which enables users to actively explore the data through coordinated interactions across visualizations[@CMV2007]. In R, building dashboards with `Shiny`[@shiny] often involves combining `htmlwidgets`[@htmlwidgets] like `leaflet`[@leaflet] and `DT`[@DT]. However, synchronizing server-side state and custom interactions between widgets such as these requires the writing and maintenance of complex, highly repetitive `observeEvent` boilerplate within `Shiny`.

The development of `linkeR` was motivated by the UI/UX requirements of collaboration between the University of Utah and the Utah Department of Health and Human Services (DHHS) Wastewater Surveillance dashboard developers. The DHHS developers needed robust, bidirectional synchronization between the `leaflet` and `DT` click interactions. Manually capturing of click events, mapping IDs, and explicitly updating sibling htmlwidgets proved error-prone and quite complex for non-developer scientists and specialists.

`linkeR` addresses this by abstracting server-side event observation into a centralized registry system which automatically creates and tracks `observeEvent` calls to syncronize widgets. This eliminates the need for manual event listeners for supported components, allowing developers to implement robust multi-component interactivity in `Shiny` dashboards.

## State of the field

Within the `R` and `Shiny` ecosystem, the primary established solution for implementing Coordinated Multiple Views (CMV) across `htmlwidgets` is the `crosstalk` package [@crosstalk]. `crosstalk` employs a client-side architecture, utilizing shared JavaScript state (`SharedData`) to link data elements across multiple output components. This approach excels in server-less environments and static HTML reports, providing highly responsive visual highlighting and data filtering.

However, the strictly client-side nature of `crosstalk` presents structural limitations for complex research applications like the DHHS dashboard. First, interactions are generally restricted to data subsetting and CSS-based visual brushing. It cannot natively trigger complex widget manipulations via server proxies, such as dynamically updating a `leaflet` map's bounding box (zooming) or conditionally rendering UI elements based on a user's selection in a `DT` table. Second, `crosstalk` does not readily expose its interaction events to the `shiny` server to trigger arbitrary R code, which is often necessary when executing remote database queries or intermediate spatial operations during data exploration.

Attempting to contribute these server-centric features directly to `crosstalk` was not theoretically or technically viable, as it would require fundamentally altering its intended, highly-portable client-side design.

Instead, `linkeR` fills a distinct gap by providing a purely server-side architecture. By maintaining state within a centralized registry, `linkeR` allows developers to utilize `shiny`'s native reactive environment and proxy methods to synchronize widget state. This enables complex, server-side execution upon user interaction, providing a depth of programmatic interactivity that existing client-side tools cannot sufficiently support, while drastically lowering the barrier to entry for scientists building complex exploratory tools

## Software Design

`linkeR` is built upon a centralized state management architecture operating entirely within the `shiny` reactive environment. Rather than relying on peer-to-peer communication between individual HTML widgets, `linkeR` utilizes a central registry and an observer pattern to manage complex, multi-component synchronization.

The software design is organized around three core principles: centralized state, event dispatching, and a tiered user API.

### 1. The Centralized Registry and Event Dispatch

At the core of `linkeR` is the component registry. When an interactive `htmlwidget` (e.g., a leaflet map or DT table) is instantiated, it can be registered within this central hub. The registry acts as the single source of truth, tracking component IDs (`shared_id_column`), their shared data relationships, and their current interaction states.

When a user interacts with a visualization (e.g., clicking a map marker), the source component sends a native `shiny` input event to the server. The `linkeR` registry intercepts this event, resolves the corresponding shared data identifier, and systematically dispatches update commands to all explicitly linked target components. These updates are executed using widget-specific server proxies (e.g., `leafletProxy`), ensuring that views are updated dynamically without requiring a full re-render of the UI.

### 2. Tiered API: From Declarative to Granular Control

To accommodate researchers with varying levels of shiny expertise, the package exposes a two-tiered API:

* The Simple API: For standard Coordinated Multiple Views (CMV), such as linking a map, a table, and a plot via a common data column, `linkeR` provides a high-level, declarative wrapper. This API automatically configures the registry, maps the shared IDs, and establishes the default interaction behaviors (e.g., highlight and zoom) in a single function call, abstracting away the underlying reactive boilerplate.

* The Advanced API: For specialized dashboards requiring non-standard interactions, developers can bypass the high-level wrappers and interact directly with the registry. The advanced API allows developers to manually register components, define custom trigger conditions, and explicitly map out the event propagation pathways.

### 3. Extensibility via Custom Hooks

A defining design feature of `linkeR` is its extensibility. Recognizing that researchers often need interactions to trigger analytical workflows rather than just visual updates, the registry architecture allows developers to "latch onto" the event dispatch cycle. Users can provide custom callback functions that execute alongside the default visual updates. This mechanism allows a single click on a widget to trigger arbitrary, complex server-side behaviors—such as querying a remote database, running a spatial intersection using `sf`, or recalculating an epidemiological model—before pushing the updated data back to the linked components.

## Research Impact Statement

Since its release, `linkeR` has demonstrated immediate utility and community readiness within the R ecosystem, accumulating over 1,200 downloads on the Comprehensive R Archive Network (CRAN).

The package was originally developed to fulfill the complex user interface requirements of the Utah Department of Health and Human Services (DHHS) Wastewater Surveillance dashboard, a collaborative effort with the University of Utah. While the public-facing deployment of this specific dashboard is pending internal organizational review, the successful delivery of the underlying linkeR-driven architecture demonstrates the software's credible near-term significance and its capability to meet rigorous, real-world public health reporting constraints.

Furthermore, the scientific utility of linkeR is discussed in our recent publication[@vegayonPracticalGuidelinesReflections2026], illustrating its broader applications in software development and exploratory data analysis within public health applications.

## AI Usage Disclosure

Generative AI tools were used in the development of this work. GitHub Copilot was used to assist with portions of the software implementation. Google Gemini was used to assist with drafting and editing parts of this manuscript. All AI-assisted content was reviewed and validated by the authors.

## Acknowledgements

This work was supported by the Centers for Disease Control and Prevention's Center for Forecasting and Outbreak Analytics Cooperative agreement CDC-RFA-FT-23-0069. I acknowledge the crucial feedback and collaborative efforts of Nathan Lacross and Kerry Regan from the Utah Department of Health and Human Services (DHHS), whose work on the DHHS Wastewater Surveillance dashboard provided the foundational use case and practical motivation for this package. I also extend my sincere thanks to George Vega Yon and Paul Rosen at the University of Utah for their invaluable guidance, support, and technical insights throughout the design and development of `linkeR` as well as this manuscript.

## References

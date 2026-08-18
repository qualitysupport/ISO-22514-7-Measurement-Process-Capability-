# ISO-22514-7-Measurement-Process-Capability-


# ISO 22514-7 Capability of Measurement Processes App

An R Shiny tool for evaluating measuring system and measurement process capability (Q_MS/C_MS, Q_MP/C_MP) following the procedure described in ISO 22514-7:2021 — Statistical methods in process management — Capability and performance — Part 7: Capability of measurement processes.

Built as course material for teaching AIAG-VDA / ISO 22514 harmonization concepts. Enter your own uncertainty components, or use the built in example dataset (ISO 22514-7:2021, Annex A.1 / ISO 11095) to explore how the standard's procedure works end to end.

## What it does

* Measuring system uncertainty (Clause 6.2, Table 9): combines calibration, linearity, bias, repeatability, resolution, and other components into u_MS, using either the experimental route or a Maximum Permissible Error (MPE) value (Clause 5.3, Table 1).
* Measurement process uncertainty (Clause 6.2.3, Table 9): extends u_MS with workpiece repeatability, operator/equipment reproducibility, stability, interactions, part inhomogeneity, and temperature effects to give u_MP.
* Combined and expanded uncertainty (Clause 8): u_MS/u_MP expanded to U_MS/U_MP with a chosen coverage factor k.
* Performance ratios and capability indices (Clause 9.1, 9.2): Q_MS, C_MS, Q_MP, and C_MP, checked against the standard's recommended acceptance criteria (Q_MS ≤ 15 %, C_MS ≥ 1.33; Q_MP ≤ 30 %, C_MP ≥ 1.33).
* One-sided specifications (Clause 9.3): capability calculation when only one specification limit exists, from a direct C_p × Δ value or from an operating point X_nom.
* Linearity & repeatability study (Clause 7.1.2, 7.1.3, Annex A.1, Annex B.1–B.2): paste in repeated measurements on several reference standards and the app runs a one-way random-effects ANOVA to derive u_BI, u_LIN, and u_EVR directly from the data, reproducing the standard's own worked example.

## Running it

Requires R with the following packages:

```
install.packages("shiny")
```

Then:

```
shiny::runApp("app.R")
```

## A note on the two routes to u_MS

The standard allows two ways to establish the measuring system's uncertainty: build it up experimentally from its individual components (calibration, linearity, bias, repeatability, resolution — Clause 6.2.2), or use a documented Maximum Permissible Error value straight from a calibration certificate (Clause 5.3, Table 1). The MPE route is faster and is recommended when a population of interchangeable equipment will be used for the measurement task; the experimental route is preferable — and normally gives a smaller combined uncertainty — when a single, specific measuring system is dedicated to the task. This app supports both, selectable from the sidebar.

## Disclaimer

This tool implements calculation methods described in ISO 22514-7:2021 but is not affiliated with, endorsed by, or reviewed by ISO. It does not reproduce the standard itself. For authoritative guidance, consult the official ISO 22514-7:2021 document, available for purchase from [ISO](https://www.iso.org/) or your national standards body.

Provided as is, for educational and reference use. This is a calculation aid only — it does not check the standard's preconditions (resolution adequacy, sufficient number of measurements, independence between uncertainty components, suitability of the linear model) and does not replace professional judgement in selecting or validating a measurement process.

## Author

Dan Lay Jr. Metrologist | ASQ Certified Calibration Technician | Calibration Support LLC
[www.calibrationsupport.com](https://www.calibrationsupport.com) · [LinkedIn](https://linkedin.com/in/dlayjr)

## License

MIT — see [LICENSE](https://github.com/qualitysupport/iso-22514-7-capability-calculator/blob/main/LICENSE).

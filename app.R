# ISO 22514-7:2021 — Capability of Measurement Processes Calculator
#
# Implements the procedure described in ISO 22514-7:2021(E):
#   Statistical methods in process management — Capability and
#   performance — Part 7: Capability of measurement processes.
#
# Covers:
#   Clause 5.2-5.3   - Resolution check and uncertainty from Maximum
#                       Permissible Error, u_MPE (Table 1)
#   Clause 6.2.2      - Measuring-system uncertainty components:
#                       calibration (u_CAL), linearity (u_LIN), bias
#                       (u_BI), repeatability on standard (u_EVR),
#                       resolution (u_RE), other (u_MS-REST)
#   Clause 6.2.3       - Additional measurement-process components:
#                       repeatability on workpiece (u_EVO), operator
#                       reproducibility (u_AV), reproducibility of
#                       measuring system (u_GV), stability (u_STAB),
#                       interactions (u_IAi), part inhomogeneity
#                       (u_OBJ), temperature (u_T), other (u_REST)
#   Clause 7.1.2-7.1.3 - Repeatability, bias, and linearity from a
#                       reference-standard study, incl. the ANOVA
#                       method of 7.1.3.4
#   Clause 8           - Combination of uncertainty components into
#                       u_MS / u_MP (Table 9, Table 10) and expansion
#                       to U_MS / U_MP with coverage factor k
#   Clause 9.1-9.2      - Performance ratios Q_MS / Q_MP and capability
#                       indices C_MS / C_MP, two-sided specifications
#   Clause 9.3          - Capability calculation for one-sided
#                       (unilateral) specification limits
#   Annex A.1 / B.1-B.2 - Worked linearity/repeatability example
#                       reproduced as a one-way random-effects ANOVA
#                       (Table B.1) on repeated measurements of
#                       several reference standards
#
# This app implements the standard's calculation FORMULAS in original
# code; it does not reproduce any text or figures from the standard
# itself.
#
# Required packages: shiny
#
# Disclaimer: This tool implements calculation methods described in
# ISO 22514-7:2021 but is not affiliated with, endorsed by, or reviewed
# by ISO. It does not reproduce the standard itself; consult the official
# ISO 22514-7:2021 document (available for purchase from ISO or your
# national standards body) for authoritative guidance. It is a
# calculation aid only and does not replace professional judgement in
# selecting or validating a measurement process. Provided as-is, for
# educational and reference use.
########################################################################

library(shiny)

# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

# Vectorized division guarding against a zero-width specification
# interval (U - L = 0), which would otherwise divide by zero when
# computing Q_MS / Q_MP (Clause 9.1.2-9.1.3).
safe_div <- function(num, den) ifelse(den == 0, NA, num / den)

# Vectorized numeric formatter used throughout the result tables.
# Must be vectorized (ifelse, not if) because it is applied to whole
# table columns (e.g. per-standard reference values), not just single
# scalars.
fmt <- function(x, digits = 5) {
  ifelse(is.na(x), "\u2014", formatC(x, digits = digits, format = "f"))
}

# Simple pass/fail banner used after each capability result to flag
# whether the recommended acceptance criteria of Clause 9.1/9.2 are met
# (Q_MS <= 15 %, C_MS >= 1.33; Q_MP <= 30 %, C_MP >= 1.33).
verdict_box <- function(pass, pass_text, fail_text) {
  col  <- if (isTRUE(pass)) "#d4edda" else "#f8d7da"
  bcol <- if (isTRUE(pass)) "#28a745" else "#dc3545"
  txt  <- if (isTRUE(pass)) pass_text else fail_text
  div(style = sprintf(
        "padding:10px;border-radius:6px;background:%s;border:1px solid %s;margin-top:8px;font-weight:600;",
        col, bcol),
      txt)
}

# Default example data set for the Linearity & Repeatability tab:
# reproduces ISO 11095 / ISO 22514-7 Annex A.1, Table A.1 (10 reference
# materials, K = 4 repeated measurements each). Used to let users
# verify the app reproduces the standard's own worked example
# (u_BI ~= 0.0878, u_LIN ~= 0.0335, u_EVR ~= 0.0641, per A.1.4).
default_lin_data <- paste(
  "6.19,6.31,6.27,6.31,6.28",
  "9.17,9.27,9.21,9.34,9.23",
  "1.99,2.21,2.19,2.22,2.20",
  "7.77,8.00,7.81,7.95,7.84",
  "4.00,4.27,4.15,4.15,4.15",
  "10.77,10.93,10.73,10.92,10.89",
  "4.78,4.95,4.87,5.00,5.00",
  "2.99,3.24,3.17,3.21,3.21",
  "6.98,7.14,7.07,7.18,7.20",
  "9.98,10.23,10.02,10.07,10.17",
  sep = "\n"
)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
#
# Four tabs, one per calculation path of the standard:
#   1. Capability Calculator          - Clauses 8, 9.1-9.2 (two-sided)
#   2. One-Sided Specification        - Clause 9.3
#   3. Linearity & Repeatability      - Annex A.1 / B.1-B.2 (ANOVA)
#   4. About                          - scope, references, disclaimer
# ---------------------------------------------------------------------------

ui <- navbarPage(
  title = "ISO 22514-7 Capability Calculator",
  theme = NULL,

  # ---- TAB 1: Main capability calculator ---------------------------------
  # Combines uncertainty components per Table 9, expands them (Clause 8.2),
  # and reports Q_MS/C_MS and Q_MP/C_MP per Clause 9.1-9.2.
  tabPanel("Capability Calculator",
    sidebarLayout(
      sidebarPanel(width = 4,

        # -- Two-sided specification interval [L, U] and coverage factor k --
        h4("1. Specification"),
        numericInput("L", "Lower specification limit L", value = 2),
        numericInput("U", "Upper specification limit U", value = 11),
        numericInput("k", "Coverage factor k (default 2 for ~95%)", value = 2, step = 0.01),
        helpText("Use Student's t (k = t_{1-\u03b1/2,\u03bd}) instead of 2 if the number ",
                 "of repeated measurements used to derive the uncertainty is small (Clause 8.2)."),
        hr(),

        # -- Measuring system uncertainty: two alternative routes --
        # (a) build up from experimental components per Table 9, or
        # (b) use a manufacturer/calibration Maximum Permissible Error
        #     value per Clause 5.3 / Table 1 / Table 10.
        h4("2. Measuring system uncertainty"),
        radioButtons("ms_method", NULL,
                     choices = c("From experimental components (Table 9)" = "exp",
                                 "From Maximum Permissible Error, MPE (Table 1/10)" = "mpe")),

        conditionalPanel("input.ms_method == 'exp'",
          numericInput("u_CAL",    "u_CAL  \u2014 calibration uncertainty of standard", 0, step = 0.001),
          numericInput("u_LIN",    "u_LIN  \u2014 linearity deviation", 0, step = 0.001),
          numericInput("u_BI",     "u_BI   \u2014 bias", 0, step = 0.001),
          numericInput("u_EVR",    "u_EVR  \u2014 repeatability on standard", 0, step = 0.001),
          numericInput("u_RE",     "u_RE   \u2014 resolution (= R_E/\u221a12)", 0, step = 0.001),
          numericInput("u_MSREST", "u_MS-REST \u2014 other measuring-system components", 0, step = 0.001)
        ),
        conditionalPanel("input.ms_method == 'mpe'",
          numericInput("MPE1", "MPE\u2081 (maximum permissible error, characteristic 1)", 0, step = 0.001),
          numericInput("MPE2", "MPE\u2082 (0 if only one characteristic applies)", 0, step = 0.001)
        ),
        hr(),

        # -- Optional extension to the full measurement process (Clause
        # 6.2.3 / Table 9): adds workpiece repeatability, operator/
        # equipment reproducibility, stability, interactions, part
        # inhomogeneity and temperature effects on top of u_MS.
        checkboxInput("do_mp", "3. Also compute Measurement Process capability (Q_MP, C_MP)", TRUE),
        conditionalPanel("input.do_mp == 1",
          numericInput("u_EVO",  "u_EVO  \u2014 repeatability on workpiece", 0, step = 0.001),
          numericInput("u_AV",   "u_AV   \u2014 operator reproducibility", 0, step = 0.001),
          numericInput("u_GV",   "u_GV   \u2014 reproducibility of measuring system", 0, step = 0.001),
          numericInput("u_STAB", "u_STAB \u2014 reproducibility over time (stability)", 0, step = 0.001),
          numericInput("u_OBJ",  "u_OBJ  \u2014 inhomogeneity of the part/measurand", 0, step = 0.001),
          numericInput("u_T",    "u_T    \u2014 temperature influence", 0, step = 0.001),
          numericInput("u_REST", "u_REST \u2014 other measurement-process components", 0, step = 0.001),
          textInput("u_IAi", "u_IAi \u2014 interaction terms (comma separated, e.g. 0.01,0.02)", "")
        )
      ),

      mainPanel(width = 8,
        h3("Measuring system"),
        tableOutput("ms_table"),
        uiOutput("ms_verdict"),
        hr(),
        conditionalPanel("input.do_mp == 1",
          h3("Measurement process"),
          tableOutput("mp_table"),
          uiOutput("mp_verdict")
        ),
        hr(),
        helpText("Recommended acceptance criteria (9.1-9.2): Q_MS \u2264 15 %, C_MS \u2265 1.33; ",
                 "Q_MP \u2264 30 %, C_MP \u2265 1.33.")
      )
    )
  ),

  # ---- TAB 2: One-sided specification -------------------------------------
  # Clause 9.3: when only one specification limit exists (no tolerance
  # zone), the process spread term C_p x Delta_U (or Delta_L) stands in
  # for (U - L) in the two-sided formulas. That term can be supplied
  # directly, or derived from an operating point X_nom (NOTE under 9.3).
  tabPanel("One-Sided Specification (9.3)",
    sidebarLayout(
      sidebarPanel(width = 4,
        radioButtons("side", "Which limit is given?",
                     choices = c("Upper specification limit (U)" = "upper",
                                 "Lower specification limit (L)" = "lower")),
        radioButtons("delta_mode", "How is the process half-spread defined?",
                     choices = c("Directly as C_p \u00d7 \u0394" = "direct",
                                 "Via operating point X_nom and the limit" = "xnom")),
        conditionalPanel("input.delta_mode == 'direct'",
          numericInput("Cp_delta", "C_p \u00d7 \u0394 (i.e. C_p times \u0394\u1d64 or \u0394\u2097)", value = 1, step = 0.001)
        ),
        conditionalPanel("input.delta_mode == 'xnom'",
          numericInput("Xnom", "X_nom \u2014 operating point / nominal value", value = 0, step = 0.001),
          numericInput("side_limit", "Value of the given specification limit (U or L)", value = 1, step = 0.001),
          helpText("C_p \u00d7 \u0394\u1d64 = U \u2212 X_nom (upper) or C_p \u00d7 \u0394\u2097 = X_nom \u2212 L (lower).")
        ),
        hr(),
        numericInput("k_os", "Coverage factor k", value = 2, step = 0.01),
        numericInput("u_MS_os", "u_MS \u2014 combined measuring system uncertainty", value = 0, step = 0.001),
        numericInput("u_MP_os", "u_MP \u2014 combined measurement process uncertainty", value = 0, step = 0.001),
        helpText("Copy u_MS / u_MP from the Capability Calculator tab, or enter them directly.")
      ),
      mainPanel(width = 8,
        h3("Results (Clause 9.3)"),
        tableOutput("os_table"),
        uiOutput("os_verdict"),
        helpText("Formulas used (upper-limit case shown; lower-limit case is symmetric):"),
        tags$pre(
"C_hat_MS = 0.2 * (Cp*Delta_U) / (k * u_MS)      Q_hat_MS = k*u_MS / (Cp*Delta_U)
C_hat_MP = 0.4 * (Cp*Delta_U) / (k * u_MP)      Q_hat_MP = k*u_MP / (Cp*Delta_U)"
        )
      )
    )
  ),

  # ---- TAB 3: Linearity / repeatability ANOVA ------------------------------
  # Annex A.1 / Annex B.1-B.2: given K repeated measurements on each of
  # N reference standards spanning the application range, a one-way
  # random-effects ANOVA separates the between-standard variation
  # (linearity, u_LIN) from the within-standard variation
  # (repeatability, u_EVR), and the mean offset gives the bias (u_BI)
  # per 7.1.3.4.
  tabPanel("Linearity & Repeatability (ANOVA)",
    sidebarLayout(
      sidebarPanel(width = 4,
        helpText("Enter one row per reference standard/part: first column = ",
                 "conventional true (reference) value x_n, remaining columns = the K ",
                 "repeated measured values y_n1 ... y_nK. Comma-separated, one row per line. ",
                 "Default data reproduce the Annex A.1 (ISO 11095) worked example."),
        textAreaInput("lin_data", "Reference value, repeated measurements", value = default_lin_data,
                       rows = 12, resize = "vertical"),
        numericInput("u_CAL_lin", "u_CAL \u2014 calibration uncertainty of the reference standards", 0, step = 0.0001),
        actionButton("compute_lin", "Compute", class = "btn-primary")
      ),
      mainPanel(width = 8,
        h3("Per-standard results"),
        tableOutput("lin_residuals"),
        h3("ANOVA table (Table B.1)"),
        tableOutput("lin_anova"),
        h3("Derived uncertainty components (A.1.4)"),
        tableOutput("lin_uncertainty"),
        helpText("u_BI = mean(B\u1d62) / \u221a3   |   u_LIN = \u03c3\u0302_A (between-standard, beyond repeatability)  |   u_EVR = \u03c3\u0302_Res (repeatability)")
      )
    )
  ),

  # ---- TAB 4: About ---------------------------------------------------------
  tabPanel("About",
    fluidPage(
      h3("About this tool"),
      p("This app implements the calculation steps of ISO 22514-7:2021, ",
        em("Statistical methods in process management \u2014 Capability and performance \u2014 ",
           "Part 7: Capability of measurement processes"), "."),
      tags$ul(
        tags$li("Tab 1 follows Clause 8 (combined/expanded uncertainty) and Clause 9.1\u20139.2 ",
                "(performance ratios Q and capability indices C) for both the measuring system ",
                "(u_MS) and the full measurement process (u_MP), including the MPE-based route ",
                "of Table 10."),
        tags$li("Tab 2 follows Clause 9.3 for characteristics with only one specification limit."),
        tags$li("Tab 3 follows Annex A.1 / Annex B.1\u2013B.2: a one-way random-effects ANOVA on ",
                "repeated measurements of several reference standards, giving the bias, ",
                "linearity, and repeatability uncertainty components (u_BI, u_LIN, u_EVR).")
      ),
      p(strong("Limitations: "), "this tool performs the arithmetic only. It does not check the ",
        "preconditions of the standard (resolution adequacy, number of measurements, ",
        "independence/no correlation between components, suitability of the linear model, etc.) ",
        "\u2014 these must still be verified by the user, as described in Clauses 5\u20137 of the standard."),
      p("Reference: ISO 22514-7:2021(E), \u00a9 ISO 2021.")
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # ---- Measuring system uncertainty (reactive) ----
  # u_MS per Table 9:  u_MS = sqrt(u_CAL^2 + u_LIN^2 + u_BI^2 + u_EV^2 + u_MS-REST^2)
  #   where u_EV = max(u_EVR, u_RE)   (repeatability, unless resolution
  #   dominates it - Clause 6.2.2.3 / 6.2.3.5).
  # Alternatively, per Table 1/Table 10, when an MPE value is used
  # instead of the experimental route:  u_MS = u_MPE = sqrt(MPE1^2/3 + MPE2^2/3)
  # (rectangular-distribution assumption for the permissible-error band).
  u_MS_val <- reactive({
    if (input$ms_method == "exp") {
      u_EV <- max(input$u_EVR, input$u_RE)
      sqrt(input$u_CAL^2 + input$u_LIN^2 + input$u_BI^2 + u_EV^2 + input$u_MSREST^2)
    } else {
      sqrt(input$MPE1^2 / 3 + input$MPE2^2 / 3)
    }
  })

  # Clause 8.2: expanded uncertainty U_MS = k * u_MS
  # Clause 9.1.2: performance ratio  Q_MS = 2*U_MS / (U-L) * 100 [%]
  # Clause 9.2:   capability index   C_MS = 0.2*(U-L) / (2*k*u_MS)
  #               (0.2 and k=2, Q_MS<=15% are calibrated so that
  #               C_MS = 1.33 at the recommended acceptance limit)
  output$ms_table <- renderTable({
    u_MS <- u_MS_val()
    U_MS <- input$k * u_MS
    Q_MS <- safe_div(2 * U_MS, input$U - input$L) * 100
    C_MS <- 0.2 * (input$U - input$L) / (2 * input$k * u_MS)
    data.frame(
      Quantity = c("Combined standard uncertainty  u_MS",
                   "Expanded uncertainty  U_MS = k\u00b7u_MS",
                   "Performance ratio  Q_MS  [%]",
                   "Capability index  C_MS"),
      Value = c(fmt(u_MS), fmt(U_MS), fmt(Q_MS, 2), fmt(C_MS, 3))
    )
  }, striped = TRUE, bordered = TRUE)

  # Recommended acceptance criteria per Clause 9.1.1/9.2: Q_MS <= 15 %
  # and C_MS >= 1.33 (the two are equivalent at k = 2).
  output$ms_verdict <- renderUI({
    u_MS <- u_MS_val()
    Q_MS <- safe_div(2 * input$k * u_MS, input$U - input$L) * 100
    C_MS <- 0.2 * (input$U - input$L) / (2 * input$k * u_MS)
    pass <- !is.na(Q_MS) && Q_MS <= 15 && C_MS >= 1.33
    verdict_box(pass,
      "Measuring system meets the recommended criteria (Q_MS \u2264 15 %, C_MS \u2265 1.33).",
      "Measuring system does NOT meet the recommended criteria \u2014 review/improve the measuring system.")
  })

  # ---- Measurement process uncertainty (reactive) ----

  # u_IAi (interactions) is entered as a comma-separated list because
  # several interaction terms can exist (Table 9: "sum_i u_IAi^2");
  # this sums their squares as required by the combination formula.
  u_IAi_sumsq <- reactive({
    v <- suppressWarnings(as.numeric(strsplit(input$u_IAi, ",")[[1]]))
    v <- v[!is.na(v)]
    sum(v^2)
  })

  # u_MP per Table 9:
  #   u_MP = sqrt(u_CAL^2 + u_LIN^2 + u_BI^2 + u_EV^2 + u_MS-REST^2
  #               + u_AV^2 + u_GV^2 + u_STAB^2 + u_OBJ^2 + u_T^2
  #               + u_REST^2 + sum_i u_IAi^2)
  #   where u_EV = max(u_EVR, u_EVO, u_RE)  (repeatability on standard,
  #   repeatability on workpiece, or resolution - whichever is largest).
  # When the MPE route is used instead (Table 10), the calibration/
  # linearity/bias/repeatability/resolution/MS-REST block collapses to
  # u_MPE, and the same additional MP components are added on top.
  u_MP_val <- reactive({
    if (input$ms_method == "exp") {
      u_EV_mp <- max(input$u_EVR, input$u_EVO, input$u_RE)
      base_sq <- input$u_CAL^2 + input$u_LIN^2 + input$u_BI^2 + input$u_MSREST^2 + u_EV_mp^2
    } else {
      base_sq <- input$MPE1^2 / 3 + input$MPE2^2 / 3
    }
    extra_sq <- input$u_AV^2 + input$u_GV^2 + input$u_STAB^2 +
                input$u_OBJ^2 + input$u_T^2 + input$u_REST^2 + u_IAi_sumsq()
    sqrt(base_sq + extra_sq)
  })

  # Clause 8.2: U_MP = k * u_MP
  # Clause 9.1.3: Q_MP = 2*U_MP / (U-L) * 100 [%]
  # Clause 9.2:   C_MP = 0.4*(U-L) / (2*k*u_MP)
  #               (0.4 and Q_MP<=30% are calibrated so that C_MP = 1.33
  #               at the recommended acceptance limit)
  output$mp_table <- renderTable({
    u_MP <- u_MP_val()
    U_MP <- input$k * u_MP
    Q_MP <- safe_div(2 * U_MP, input$U - input$L) * 100
    C_MP <- 0.4 * (input$U - input$L) / (2 * input$k * u_MP)
    data.frame(
      Quantity = c("Combined standard uncertainty  u_MP",
                   "Expanded uncertainty  U_MP = k\u00b7u_MP",
                   "Performance ratio  Q_MP  [%]",
                   "Capability index  C_MP"),
      Value = c(fmt(u_MP), fmt(U_MP), fmt(Q_MP, 2), fmt(C_MP, 3))
    )
  }, striped = TRUE, bordered = TRUE)

  # Recommended acceptance criteria per Clause 9.1.1/9.2: Q_MP <= 30 %
  # and C_MP >= 1.33.
  output$mp_verdict <- renderUI({
    u_MP <- u_MP_val()
    Q_MP <- safe_div(2 * input$k * u_MP, input$U - input$L) * 100
    C_MP <- 0.4 * (input$U - input$L) / (2 * input$k * u_MP)
    pass <- !is.na(Q_MP) && Q_MP <= 30 && C_MP >= 1.33
    verdict_box(pass,
      "Measurement process meets the recommended criteria (Q_MP \u2264 30 %, C_MP \u2265 1.33).",
      "Measurement process does NOT meet the recommended criteria \u2014 review the measurement process.")
  })

  # ---- One-sided specification (Clause 9.3) ----
  # For an upper-only limit: C_hat_MS = 0.2*(Cp*Delta_U)/(k*u_MS),
  #                          Q_hat_MS = k*u_MS/(Cp*Delta_U), and
  #                          symmetric formulas with 0.4 for C_MP/Q_MP.
  # For a lower-only limit the same formulas apply with Delta_L in
  # place of Delta_U. The "C_p x Delta" term plays the role that
  # (U - L) plays in the two-sided case. Per the NOTE under 9.3, when
  # an operating point X_nom is defined, Cp*Delta_U = U - X_nom (or
  # Cp*Delta_L = X_nom - L).
  output$os_table <- renderTable({
    CpDelta <- if (input$delta_mode == "direct") {
      input$Cp_delta
    } else {
      if (input$side == "upper") input$side_limit - input$Xnom else input$Xnom - input$side_limit
    }
    k <- input$k_os
    u_MS <- input$u_MS_os
    u_MP <- input$u_MP_os

    C_MS <- 0.2 * CpDelta / (k * u_MS)
    Q_MS <- safe_div(k * u_MS, CpDelta)
    C_MP <- 0.4 * CpDelta / (k * u_MP)
    Q_MP <- safe_div(k * u_MP, CpDelta)

    data.frame(
      Quantity = c("C_p \u00d7 \u0394 (one-sided process spread term)",
                   "C_MS", "Q_MS [%]",
                   "C_MP", "Q_MP [%]"),
      Value = c(fmt(CpDelta), fmt(C_MS, 3), fmt(Q_MS * 100, 2),
                fmt(C_MP, 3), fmt(Q_MP * 100, 2))
    )
  }, striped = TRUE, bordered = TRUE)

  output$os_verdict <- renderUI({
    CpDelta <- if (input$delta_mode == "direct") {
      input$Cp_delta
    } else {
      if (input$side == "upper") input$side_limit - input$Xnom else input$Xnom - input$side_limit
    }
    k <- input$k_os
    C_MS <- 0.2 * CpDelta / (k * input$u_MS_os)
    C_MP <- 0.4 * CpDelta / (k * input$u_MP_os)
    pass <- !is.na(C_MS) && !is.na(C_MP) && C_MS >= 1.33 && C_MP >= 1.33
    verdict_box(pass,
      "Both C_MS and C_MP meet the recommended 1.33 threshold.",
      "One or both of C_MS / C_MP fall below the recommended 1.33 threshold.")
  })

  # ---- Linearity / repeatability ANOVA (Annex A.1 / B.1-B.2) ----
  lin_results <- eventReactive(input$compute_lin, {

    # Parse the pasted table: column 1 = reference (true) value x_n,
    # remaining K columns = repeated measurements y_n1...y_nK
    # (mirrors Table A.1's layout of N reference materials x K reps).
    txt <- input$lin_data
    con <- textConnection(txt)
    raw <- tryCatch(read.csv(con, header = FALSE), error = function(e) NULL)
    close(con)
    validate(need(!is.null(raw) && ncol(raw) >= 2, "Please provide at least one reference value and one repeated measurement column."))

    x_n <- raw[[1]]
    Y <- as.matrix(raw[, -1, drop = FALSE])
    N <- nrow(Y)   # number of reference standards
    K <- ncol(Y)   # number of repeated measurements per standard

    # Per-standard mean and bias, per 7.1.2.3:
    #   B_in = mean(y_n) - x_n  (bias of standard n)
    ybar_n <- rowMeans(Y)
    B_in <- ybar_n - x_n

    # Reshape to long format for a one-way random-effects ANOVA with
    # "reference standard" as the grouping factor (Annex B.2, Table B.1
    # / B.4): SS_A captures variation *between* standards (beyond
    # repeatability) - i.e. linearity - while SS_Res captures variation
    # *within* a standard's repeated measurements - i.e. repeatability.
    long <- data.frame(
      value = as.vector(t(Y)),
      grp   = factor(rep(seq_len(N), each = K))
    )
    fit <- aov(value ~ grp, data = long)
    tab <- summary(fit)[[1]]
    SS_A   <- tab["grp", "Sum Sq"]
    df_A   <- tab["grp", "Df"]
    SS_Res <- tab["Residuals", "Sum Sq"]
    df_Res <- tab["Residuals", "Df"]
    MS_A   <- SS_A / df_A
    MS_Res <- SS_Res / df_Res
    Fstat  <- MS_A / MS_Res
    Fcrit  <- qf(0.95, df_A, df_Res)   # 95% critical F, Table B.1

    # Table B.1 estimators of the random-effects variance components:
    #   sigma_A^2   = (MS_A - MS_Res) / K   (variance between standards)
    #   sigma_Res^2 = MS_Res                (residual/repeatability variance)
    # Clamped at 0 in case sampling noise makes MS_A < MS_Res.
    sigma_A_sq <- max(0, (MS_A - MS_Res) / K)
    sigma_A    <- sqrt(sigma_A_sq)
    sigma_Res  <- sqrt(MS_Res)

    # Clause 7.1.3.4 / A.1.4 uncertainty components:
    #   u_BI  = mean(B_i) / sqrt(3)   (mean bias, rectangular a-priori
    #                                  distribution assumption)
    #   u_LIN = sigma_hat_A            (variable/linearity portion of bias)
    #   u_EVR = sigma_hat_Res          (repeatability on the reference part)
    u_BI  <- abs(mean(B_in)) / sqrt(3)
    u_LIN <- sigma_A
    u_EVR <- sigma_Res

    list(x_n = x_n, ybar_n = ybar_n, B_in = B_in, resid_matrix = Y - ybar_n,
         N = N, K = K,
         anova = data.frame(
           Source = c("Reference standards (Factor A)", "Residual error", "Total"),
           SS = c(fmt(SS_A), fmt(SS_Res), fmt(SS_A + SS_Res)),
           df = c(df_A, df_Res, df_A + df_Res),
           MS = c(fmt(MS_A), fmt(MS_Res), ""),
           F  = c(fmt(Fstat, 3), "", ""),
           F_crit_0.05 = c(fmt(Fcrit, 3), "", "")
         ),
         u_BI = u_BI, u_LIN = u_LIN, u_EVR = u_EVR,
         u_CAL = input$u_CAL_lin)
  })

  # Table A.2-style per-standard breakdown: mean measured value and
  # bias for each reference standard.
  output$lin_residuals <- renderTable({
    r <- lin_results()
    data.frame(
      x_n        = fmt(r$x_n, 4),
      mean_ybar_n = fmt(r$ybar_n, 4),
      B_in       = fmt(r$B_in, 4)
    )
  }, striped = TRUE, bordered = TRUE)

  # ANOVA table matching the layout of Table B.1.
  output$lin_anova <- renderTable({
    lin_results()$anova
  }, striped = TRUE, bordered = TRUE)

  # Final derived uncertainty components (A.1.4), ready to be copied
  # into the "From experimental components" fields on Tab 1.
  output$lin_uncertainty <- renderTable({
    r <- lin_results()
    data.frame(
      Component = c("u_BI  (bias)", "u_LIN (linearity)", "u_EVR (repeatability on standard)", "u_CAL (as entered)"),
      Value = c(fmt(r$u_BI), fmt(r$u_LIN), fmt(r$u_EVR), fmt(r$u_CAL))
    )
  }, striped = TRUE, bordered = TRUE)
}

# Launch the application.

shinyApp(ui, server)

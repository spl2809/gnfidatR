test_that("build_wzp joins metrics by complete inventory-specific intkeys", {
    trees <- gnfidatR_tree_fixture()
    derived <- gnfidatR_derived_fixture()[c(3L, 1L, 2L), ]
    wrong_inventory <- derived[1L, ]
    wrong_inventory$intkey <- "bwi2012-101-1-1"
    wrong_inventory$dbh <- 999
    derived <- rbind(derived, wrong_inventory)

    local_mocked_bindings(
        get_derived_tree = function(...) {
            derived
        },
        .package = "gnfidatR"
    )

    connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    withr::defer(DBI::dbDisconnect(connection))
    api <- gnfidatR_test_api()

    build_wzp(
        url = api$url,
        apikey = api$apikey,
        conn = connection,
        plots_and_trees = trees,
        rename = TRUE,
        page_size = 2L
    )
    actual <- DBI::dbReadTable(connection, "WEHAMI_WZP")

    expected_columns <- c(
        "Tnr",
        "Enr",
        "Bnr",
        "Bs",
        "Ba",
        "Kh",
        "Kst",
        "Bhd",
        "D03",
        "Hoe",
        "StHoe",
        "Al_ba",
        "N_ha"
    )

    expect_named(actual, expected_columns)
    expect_equal(actual$Bhd, c(300, 250, 200))
    expect_equal(actual$D03, c(220, 180, 140))
    expect_equal(actual$Tnr, trees$cluster_name)
    expect_equal(actual$Bnr, trees$tree_number)
})

test_that("build_wzp supports original column names", {
    local_mocked_bindings(
        get_derived_tree = function(...) {
            gnfidatR_derived_fixture()
        },
        .package = "gnfidatR"
    )

    connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    withr::defer(DBI::dbDisconnect(connection))
    api <- gnfidatR_test_api()

    build_wzp(
        url = api$url,
        apikey = api$apikey,
        conn = connection,
        plots_and_trees = gnfidatR_tree_fixture(),
        rename = FALSE,
        page_size = 2L
    )
    actual <- DBI::dbReadTable(connection, "WEHAMI_WZP")

    expected_columns <- c(
        "cluster_name",
        "plot_name",
        "tree_number",
        "stand_layer",
        "tree_species",
        "stem_breakage",
        "stem_form",
        "dbh",
        "diameter_30perc",
        "tree_height",
        "stem_height",
        "tree_age",
        "trees_per_hectare"
    )

    expect_named(actual, expected_columns)
})

test_that("build_wzp rejects ambiguous and missing tree intkeys", {
    derived <- gnfidatR_derived_fixture()

    local_mocked_bindings(
        get_derived_tree = function(...) {
            derived
        },
        .package = "gnfidatR"
    )

    connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    withr::defer(DBI::dbDisconnect(connection))
    api <- gnfidatR_test_api()
    trees <- gnfidatR_tree_fixture()
    derived <- rbind(derived, derived[1L, ])

    expect_error(
        build_wzp(
            url = api$url,
            apikey = api$apikey,
            conn = connection,
            plots_and_trees = trees,
            rename = TRUE,
            page_size = 2L
        ),
        "unique"
    )

    derived <- gnfidatR_derived_fixture()
    trees$intkey[1L] <- NA_character_

    expect_error(
        build_wzp(
            url = api$url,
            apikey = api$apikey,
            conn = connection,
            plots_and_trees = trees,
            rename = TRUE,
            page_size = 2L
        ),
        "non-missing"
    )

    trees$intkey <- NULL

    expect_error(
        build_wzp(
            url = api$url,
            apikey = api$apikey,
            conn = connection,
            plots_and_trees = trees,
            rename = TRUE,
            page_size = 2L
        ),
        "Full tree intkeys"
    )
})

test_that("build_wzp warns for partial matches and rejects missing matches", {
    derived <- gnfidatR_derived_fixture()[-1L, ]

    local_mocked_bindings(
        get_derived_tree = function(...) {
            derived
        },
        .package = "gnfidatR"
    )

    connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    withr::defer(DBI::dbDisconnect(connection))
    api <- gnfidatR_test_api()

    expect_warning(
        build_wzp(
            url = api$url,
            apikey = api$apikey,
            conn = connection,
            plots_and_trees = gnfidatR_tree_fixture(),
            rename = TRUE,
            page_size = 2L
        ),
        "1 tree rows"
    )
    expect_equal(nrow(DBI::dbReadTable(connection, "WEHAMI_WZP")), 2L)

    derived$intkey <- paste0("other-", derived$intkey)

    expect_error(
        build_wzp(
            url = api$url,
            apikey = api$apikey,
            conn = connection,
            plots_and_trees = gnfidatR_tree_fixture(),
            rename = TRUE,
            page_size = 2L
        ),
        "No derived metrics match"
    )
})

test_that("build_wzp keeps empty schemas without downloading metrics", {
    local_mocked_bindings(get_derived_tree = gnfidatR_no_download, .package = "gnfidatR")

    connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    withr::defer(DBI::dbDisconnect(connection))
    api <- gnfidatR_test_api()

    build_wzp(
        url = api$url,
        apikey = api$apikey,
        conn = connection,
        plots_and_trees = gnfidatR_tree_fixture()[FALSE, ],
        rename = TRUE,
        page_size = 2L
    )
    actual <- DBI::dbReadTable(connection, "WEHAMI_WZP")

    expect_equal(nrow(actual), 0L)
    expect_true(all(c("Tnr", "Bnr", "Bhd", "N_ha") %in% names(actual)))
})

test_that("build_jung preserves distinct records with equal projected values", {
    regeneration <- gnfidatR_regeneration_fixture()
    duplicate <- regeneration[1L, ]
    duplicate$intkey <- "bwi2022-101-1-r2"
    regeneration <- rbind(regeneration, duplicate)
    connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    withr::defer(DBI::dbDisconnect(connection))

    build_jung(
        conn = connection,
        plots_and_trees_regeneration = regeneration,
        rename = TRUE
    )
    actual <- DBI::dbReadTable(connection, "WEHAMI_Jung")

    expected_columns <- c("Tnr", "Enr", "Ba", "Bs", "Gr", "Anz")

    expect_named(actual, expected_columns)
    expect_equal(nrow(actual), 3L)
    expect_equal(actual$Anz, c(4, 7, 4))

    build_jung(
        conn = connection,
        plots_and_trees_regeneration = regeneration,
        rename = FALSE
    )

    expected_columns <- c(
        "cluster_name",
        "plot_name",
        "tree_species",
        "stand_layer_regeneration",
        "tree_size_class",
        "tree_count"
    )

    expect_named(DBI::dbReadTable(connection, "WEHAMI_Jung"), expected_columns)
})

test_that("build_eck uses the supplied share and exported plot keys", {
    # Regression: the supplied build_eck reads a missing data column
    # instead of using its share_of_natural_regeneration argument.
    connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    withr::defer(DBI::dbDisconnect(connection))
    DBI::dbWriteTable(connection, "WEHAMI_WZP", data.frame(Tnr = 101L, Enr = 1L))
    DBI::dbWriteTable(connection, "WEHAMI_Jung", data.frame(Tnr = 205L, Enr = 1L))

    build_eck(
        conn = connection,
        plots_and_trees = gnfidatR_tree_fixture(),
        plots_and_trees_regeneration = gnfidatR_regeneration_fixture(),
        share_of_natural_regeneration = 37L,
        rename = TRUE
    )
    actual <- DBI::dbReadTable(connection, "WEHAMI_Eck")

    expect_equal(actual$Tnr, c(101, 205))
    expect_equal(actual$Enr, c(1, 1))
    expect_identical(actual$NatVjProz, c(37L, 37L))
    expect_equal(actual$Grkl, c(-1, 3))
    expect_equal(actual$BsP100, actual$BsP175)
    expect_equal(anyDuplicated(actual[c("Tnr", "Enr")]), 0L)
})

test_that("build_eck supports original plot key names", {
    connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    withr::defer(DBI::dbDisconnect(connection))
    DBI::dbWriteTable(
        connection,
        "WEHAMI_WZP",
        data.frame(cluster_name = 101L, plot_name = 1L)
    )
    DBI::dbWriteTable(
        connection,
        "WEHAMI_Jung",
        data.frame(cluster_name = 205L, plot_name = 1L)
    )

    build_eck(
        conn = connection,
        plots_and_trees = gnfidatR_tree_fixture(),
        plots_and_trees_regeneration = gnfidatR_regeneration_fixture(),
        share_of_natural_regeneration = 0L,
        rename = FALSE
    )
    actual <- DBI::dbReadTable(connection, "WEHAMI_Eck")

    expect_equal(actual$cluster_name, c(101, 205))
    expect_identical(actual$NatVjProz, c(0L, 0L))
    expect_false("Tnr" %in% names(actual))
})

test_that("build_eck rejects conflicting metadata for the same plot", {
    connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    withr::defer(DBI::dbDisconnect(connection))
    DBI::dbWriteTable(connection, "WEHAMI_WZP", data.frame(Tnr = 101L, Enr = 1L))
    DBI::dbWriteTable(connection, "WEHAMI_Jung", data.frame(Tnr = 205L, Enr = 1L))
    trees <- gnfidatR_tree_fixture()
    trees$federal_state[2L] <- 9L

    expect_error(
        build_eck(
            conn = connection,
            plots_and_trees = trees,
            plots_and_trees_regeneration = gnfidatR_regeneration_fixture(),
            share_of_natural_regeneration = 100L,
            rename = TRUE
        ),
        "Conflicting plot metadata"
    )
})

test_that("get_cluster handles training clusters and normalizes IDs", {
    captured_arguments <- NULL

    local_mocked_bindings(
        fetch_single_table = function(...) {
            captured_arguments <<- list(...)
            data.frame(cluster_name = c(205, 101, 101))
        },
        .package = "gnfidatR"
    )

    api <- gnfidatR_test_api()

    expect_equal(get_cluster(url = api$url, apikey = api$apikey, limit = 2L), c(101, 205))
    expect_identical(captured_arguments[[1]]$is_training, "eq.false")

    get_cluster(
        url = api$url,
        apikey = api$apikey,
        limit = 2L,
        include_test_cluster = TRUE
    )

    expect_null(captured_arguments[[1]]$is_training)
})

test_that("plots_and_trees_fetch preserves inventory, batching and records", {
    captured_arguments <- NULL

    local_mocked_bindings(
        fetch_join_table = function(...) {
            captured_arguments <<- list(...)
            gnfidatR_nested_fixture(child = "tree")
        },
        .package = "gnfidatR"
    )

    api <- gnfidatR_test_api()

    actual <- plots_and_trees_fetch(
        url = api$url,
        apikey = api$apikey,
        cluster_liste = c(101, 205),
        interval_name = "bwi2022",
        step = 1L,
        page_size = 2L
    )

    expect_identical(actual$intkey, gnfidatR_tree_fixture()$intkey)
    expect_identical(actual$plot_id, c("p1", "p1", "p2"))
    expect_identical(captured_arguments$params$interval_name, "eq.bwi2022")
    expect_identical(captured_arguments$step, 1L)
    expect_identical(captured_arguments$page_size, 2L)
})

test_that("regeneration_fetch expands all nested source records", {
    local_mocked_bindings(
        fetch_join_table = function(...) {
            gnfidatR_nested_fixture(child = "regeneration")
        },
        .package = "gnfidatR"
    )

    api <- gnfidatR_test_api()

    actual <- regeneration_fetch(
        url = api$url,
        apikey = api$apikey,
        cluster_liste = c(101, 205),
        interval_name = "bwi2022",
        step = 1L,
        page_size = 2L
    )

    expect_identical(actual$intkey, gnfidatR_regeneration_fixture()$intkey)
    expect_identical(actual$tree_count, c(4L, 7L))
})

test_that("get_derived_tree keeps full intkeys and uses the derived schema", {
    captured_arguments <- NULL

    local_mocked_bindings(
        fetch_single_table = function(...) {
            captured_arguments <<- list(...)
            gnfidatR_derived_fixture()
        },
        .package = "gnfidatR"
    )

    api <- gnfidatR_test_api()

    actual <- get_derived_tree(
        url = api$url,
        apikey = api$apikey,
        limit = 2L
    )

    expect_identical(actual$intkey, gnfidatR_tree_fixture()$intkey)
    expect_identical(captured_arguments$accept_profile, "derived")
    expect_identical(captured_arguments[[1]]$order, "intkey.asc")
})

test_that("nested fetch helpers preserve the selected empty plot schema", {
    local_mocked_bindings(
        fetch_join_table = function(...) {
            data.frame()
        },
        .package = "gnfidatR"
    )

    api <- gnfidatR_test_api()
    trees <- plots_and_trees_fetch(
        url = api$url,
        apikey = api$apikey,
        cluster_liste = 101,
        interval_name = "bwi2022",
        step = 1L,
        page_size = 2L
    )
    regeneration <- regeneration_fetch(
        url = api$url,
        apikey = api$apikey,
        cluster_liste = 101,
        interval_name = "bwi2022",
        step = 1L,
        page_size = 2L
    )
    invalid_values <- list(trees, regeneration)

    for (actual in invalid_values) {
        expect_equal(nrow(actual), 0L)

        missing_columns <- setdiff(
            c("plot_id", "cluster_name", "plot_name", "sampling_stratum", "intkey"),
            names(actual)
        )

        expect_identical(missing_columns, character())
    }

    expect_error(empty_plot_records(child = "unknown"), "Unknown embedded table")
})

test_that("get_weham_data writes three consistent WEHAM tables", {
    # Prepare controlled downloads while keeping the real table builders.
    local_mocked_bindings(
        get_cluster = function(...) {
            c(101, 205)
        },
        plots_and_trees_fetch = function(...) {
            gnfidatR_tree_fixture()
        },
        regeneration_fetch = function(...) {
            gnfidatR_regeneration_fixture()
        },
        get_derived_tree = function(...) {
            gnfidatR_derived_fixture()
        },
        .package = "gnfidatR"
    )

    path <- file.path(withr::local_tempdir(), "weham.sqlite")
    api <- gnfidatR_test_api()

    expect_invisible(
        get_weham_data(
            db_path = path,
            interval_name = "bwi2022",
            share_of_natural_regeneration = 42L,
            print_progress = FALSE,
            url = api$url,
            apikey = api$apikey
        )
    )

    connection <- DBI::dbConnect(RSQLite::SQLite(), path)
    withr::defer(DBI::dbDisconnect(connection))

    expect_setequal(
        DBI::dbListTables(connection),
        c("WEHAMI_WZP", "WEHAMI_Jung", "WEHAMI_Eck")
    )
    expect_equal(nrow(DBI::dbReadTable(connection, "WEHAMI_WZP")), 3L)
    expect_equal(nrow(DBI::dbReadTable(connection, "WEHAMI_Jung")), 2L)

    eck_table <- DBI::dbReadTable(connection, "WEHAMI_Eck")

    expect_equal(nrow(eck_table), 3L)
    expect_true(all(eck_table$NatVjProz == 42L))
})

test_that("get_weham_data rolls back earlier writes after a late failure", {
    # Seed every output table so rollback must restore existing data.
    path <- file.path(withr::local_tempdir(), "weham.sqlite")
    connection <- DBI::dbConnect(RSQLite::SQLite(), path)
    withr::defer(DBI::dbDisconnect(connection))
    tables <- c("WEHAMI_WZP", "WEHAMI_Jung", "WEHAMI_Eck", "unrelated")

    for (table in tables) {
        DBI::dbWriteTable(connection, table, data.frame(original = 99L))
    }

    local_mocked_bindings(
        get_cluster = function(...) {
            c(101, 205)
        },
        plots_and_trees_fetch = function(...) {
            gnfidatR_tree_fixture()
        },
        regeneration_fetch = function(...) {
            gnfidatR_regeneration_fixture()
        },
        get_derived_tree = function(...) {
            gnfidatR_derived_fixture()
        },
        build_eck = function(...) {
            # Fail after the earlier output tables have been written.
            stop("Simulated late failure", call. = FALSE)
        },
        .package = "gnfidatR"
    )

    api <- gnfidatR_test_api()

    expect_error(
        get_weham_data(
            db_path = path,
            interval_name = "bwi2022",
            print_progress = FALSE,
            url = api$url,
            apikey = api$apikey
        ),
        "Simulated late failure"
    )

    for (table in tables) {
        expect_identical(DBI::dbReadTable(connection, table), data.frame(original = 99L))
    }
})

test_that("get_weham_data validates input before creating a database", {
    directory <- withr::local_tempdir()
    path <- file.path(directory, "weham.sqlite")
    api <- gnfidatR_test_api()

    local_mocked_bindings(get_cluster = gnfidatR_no_download, .package = "gnfidatR")

    expect_error(
        get_weham_data(
            db_path = path,
            interval_name = "unknown",
            print_progress = FALSE,
            url = api$url,
            apikey = api$apikey
        ),
        "must be one of"
    )
    expect_false(file.exists(path))

    local_mocked_bindings(
        get_cluster = function(...) {
            numeric()
        },
        .package = "gnfidatR"
    )

    expect_error(
        get_weham_data(
            db_path = path,
            interval_name = "bwi2022",
            print_progress = FALSE,
            url = api$url,
            apikey = api$apikey
        ),
        "list of clusters is empty"
    )
    expect_false(file.exists(path))
})

test_that("get_weham_data leaves no database for empty or unmatched data", {
    api <- gnfidatR_test_api()
    path <- file.path(withr::local_tempdir(), "weham.sqlite")
    trees <- gnfidatR_tree_fixture()[FALSE, ]

    local_mocked_bindings(
        get_cluster = function(...) {
            101
        },
        plots_and_trees_fetch = function(...) {
            trees
        },
        regeneration_fetch = function(...) {
            gnfidatR_regeneration_fixture()
        },
        get_filtered_plots = function(...) {
            data.frame(plot_id = character())
        },
        .package = "gnfidatR"
    )

    expect_error(
        get_weham_data(
            db_path = path,
            interval_name = "bwi2022",
            print_progress = FALSE,
            url = api$url,
            apikey = api$apikey
        ),
        "No tree or regeneration"
    )
    expect_false(file.exists(path))

    trees <- gnfidatR_tree_fixture()

    expect_error(
        get_weham_data(
            db_path = path,
            interval_name = "bwi2022",
            filtering_options = list(federal_states = 9),
            print_progress = FALSE,
            url = api$url,
            apikey = api$apikey
        ),
        "After filtering"
    )
    expect_false(file.exists(path))
})

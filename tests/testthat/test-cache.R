# Tests for caching functionality (cache.R)

# Access internal objects
get_tezr_env <- function() {
  get("tezr_env", envir = asNamespace("tezr"))
}

# Deep-copy an environment's contents into a fresh environment
snapshot_env <- function(src_env) {
  if (is.null(src_env)) {
    return(NULL)
  }
  dst <- new.env(parent = emptyenv())
  for (key in ls(src_env)) {
    dst[[key]] <- src_env[[key]]
  }
  dst
}

# Restore an environment's contents from a snapshot (in-place)
restore_env <- function(target, snapshot) {
  rm(list = ls(target), envir = target)
  if (!is.null(snapshot)) {
    for (key in ls(snapshot)) {
      target[[key]] <- snapshot[[key]]
    }
  }
}

# Helper: snapshot full cache state and restore on exit
local_clean_cache <- function(env = parent.frame()) {
  tezr <- get_tezr_env()

  old_enabled <- tezr$cache_enabled
  old_search_ttl <- tezr$search_ttl
  old_detail_ttl <- tezr$detail_ttl
  old_search_cache <- snapshot_env(tezr$search_cache)
  old_range_cache <- snapshot_env(tezr$range_cache)
  old_detail_cache <- snapshot_env(tezr$detail_cache)

  withr::defer(
    {
      tezr$cache_enabled <- old_enabled
      tezr$search_ttl <- old_search_ttl
      tezr$detail_ttl <- old_detail_ttl
      restore_env(tezr$search_cache, old_search_cache)
      restore_env(tezr$range_cache, old_range_cache)
      restore_env(tezr$detail_cache, old_detail_cache)
    },
    envir = env
  )
}

test_that("cache_enabled returns TRUE by default", {
  local_clean_cache()
  env <- get_tezr_env()
  env$cache_enabled <- NULL
  init_cache()
  expect_true(cache_enabled())
})

test_that("cache_config enables/disables cache", {
  local_clean_cache()
  cache_config(enable = FALSE)
  expect_false(cache_enabled())

  cache_config(enable = TRUE)
  expect_true(cache_enabled())
})

test_that("cache_config sets TTL values", {
  local_clean_cache()
  cache_config(search_ttl = 1800, detail_ttl = 7200)

  env <- get_tezr_env()
  expect_equal(env$search_ttl, 1800)
  expect_equal(env$detail_ttl, 7200)
})

test_that("cache_info returns correct structure", {
  local_clean_cache()
  info <- cache_info()

  expect_type(info, "list")
  expect_true("enabled" %in% names(info))
  expect_true("search_count" %in% names(info))
  expect_true("range_count" %in% names(info))
  expect_true("detail_count" %in% names(info))
  expect_true("search_ttl" %in% names(info))
  expect_true("detail_ttl" %in% names(info))
})

test_that("cache_clear clears search cache", {
  local_clean_cache()
  init_cache()
  env <- get_tezr_env()
  env$search_cache[["test_key"]] <- list(value = "test", timestamp = Sys.time())

  expect_true("test_key" %in% ls(env$search_cache))

  cache_clear("searches")

  expect_false("test_key" %in% ls(env$search_cache))
})

test_that("cache_clear clears range cache", {
  local_clean_cache()
  init_cache()
  env <- get_tezr_env()
  env$range_cache[["test_key"]] <- list(value = "test", timestamp = Sys.time())

  expect_true("test_key" %in% ls(env$range_cache))

  cache_clear("searches")

  expect_false("test_key" %in% ls(env$range_cache))
})

test_that("cache_clear clears detail cache", {
  local_clean_cache()
  init_cache()
  env <- get_tezr_env()
  env$detail_cache[["test_key"]] <- list(value = "test", timestamp = Sys.time())

  expect_true("test_key" %in% ls(env$detail_cache))

  cache_clear("details")

  expect_false("test_key" %in% ls(env$detail_cache))
})

test_that("make_search_key creates consistent keys", {
  key1 <- make_search_key(query = "test", field = "all")
  key2 <- make_search_key(query = "test", field = "all")
  key3 <- make_search_key(query = "different", field = "all")

  expect_equal(key1, key2)
  expect_false(key1 == key3)
})

test_that("make_detail_key creates expected format", {
  key <- make_detail_key("abc123", "xyz789")
  expect_equal(key, "d_abc123_xyz789")
})

test_that("get_cached returns NULL when cache disabled", {
  local_clean_cache()
  cache_config(enable = FALSE)
  init_cache()

  env <- get_tezr_env()
  env$search_cache[["test"]] <- list(value = "data", timestamp = Sys.time())

  cached_value <- get_cached(env$search_cache, "test", NULL)
  expect_null(cached_value)
})

test_that("get_cached returns NULL for missing key", {
  local_clean_cache()
  cache_config(enable = TRUE)
  init_cache()

  env <- get_tezr_env()
  cached_value <- get_cached(env$search_cache, "nonexistent", NULL)
  expect_null(cached_value)
})

test_that("get_cached respects TTL", {
  local_clean_cache()
  cache_config(enable = TRUE)
  init_cache()

  env <- get_tezr_env()

  # Add item with old timestamp
  env$search_cache[["old_item"]] <- list(
    value = "old_data",
    timestamp = Sys.time() - 100 # 100 seconds ago
  )

  # Should be expired with 50 second TTL
  expired_value <- get_cached(env$search_cache, "old_item", ttl = 50)
  expect_null(expired_value)

  # Add fresh item
  env$search_cache[["new_item"]] <- list(
    value = "new_data",
    timestamp = Sys.time()
  )

  # Should not be expired
  cached_value <- get_cached(env$search_cache, "new_item", ttl = 50)
  expect_equal(cached_value, "new_data")
})

test_that("set_cached stores value with timestamp", {
  local_clean_cache()
  cache_config(enable = TRUE)
  init_cache()

  env <- get_tezr_env()
  set_cached(env$search_cache, "store_test", "my_value")

  cached <- env$search_cache[["store_test"]]
  expect_equal(cached$value, "my_value")
  expect_s3_class(cached$timestamp, "POSIXct")
})

test_that("set_cached does nothing when cache disabled", {
  local_clean_cache()
  cache_config(enable = FALSE)
  init_cache()

  env <- get_tezr_env()

  # Clear any existing key
  if ("disabled_test" %in% ls(env$search_cache)) {
    rm("disabled_test", envir = env$search_cache)
  }

  set_cached(env$search_cache, "disabled_test", "value")

  expect_false("disabled_test" %in% ls(env$search_cache))
})

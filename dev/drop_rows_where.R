library(data.table)
library(magrittr)
system("/bin/authenticate.sh")


# anon_GROUPING_VARIANT ----
cs9::add_schema_v8(
  name_access = c("anon"),
  name_grouping = "test",
  name_variant = NULL,
  db_configs = cs9::config$db_configs,
  field_types =  c(
    "uuid" = "INTEGER",
    "n" = "INTEGER"
  ),
  keys = c(
    "uuid"
  ),
  censors = list(
    anon = list(
    )
  ),
  info = "This db table is used for..."
)

cs9::config$schemas$anon_test

d = data.table(uuid = 1:1000000)
d$n = 1

cs9::config$schemas$anon_test$tbl()
cs9::config$schemas$anon_test$drop_all_rows_and_then_insert_data(d)

cs9::config$schemas$anon_test$tbl() %>%
  dplyr::collect()

cs9::config$schemas$anon_test$drop_rows_where(condition = "uuid<=50000")

table = "anon_test"

# Copyright 2015-2025 Province of British Columbia
# Copyright 2021 Environment and Climate Change Canada
# Copyright 2023-2025 Australian Government Department of Climate Change,
# Energy, the Environment and Water
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#       https://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.

# Bootstrap confidence limits and reports run on mirai daemons, so they do not
# block the R process that serves every session (see task_runner()). Set the
# option shinyssdtools.daemons to 0 to run them in the session instead.
n_daemons <- getOption("shinyssdtools.daemons", 2L)
if (n_daemons > 0 && !mirai::daemons_set()) {
  mirai::daemons(n_daemons)
  # The jobs are shinyssdtools functions, so each daemon loads the same copy of
  # the package as the app: from source when the app was loaded with
  # pkgload::load_all() (as app.R does for deployment), otherwise installed.
  source_path <- if (pkgload::is_dev_package("shinyssdtools")) {
    pkgload::pkg_path(system.file(package = "shinyssdtools"))
  }
  mirai::everywhere(
    if (is.null(source_path)) {
      loadNamespace("shinyssdtools")
    } else {
      pkgload::load_all(source_path, quiet = TRUE)
    },
    source_path = source_path
  )
  shiny::onStop(function() mirai::daemons(0))
}

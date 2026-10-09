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

# Bootstrap confidence limits and reports run on mirai daemons started when
# first needed (see task_runner()), so they do not block the R process that
# serves every session. A deployment sets their number in the file daemons
# (written by scripts/deploy-app.R); 0 runs them in the session. The option
# shinyssdtools.daemons, when set, takes precedence.
if (is.null(getOption("shinyssdtools.daemons"))) {
  setting <- system.file("app", "daemons", package = "shinyssdtools")
  if (nzchar(setting)) {
    options(shinyssdtools.daemons = as.integer(readLines(setting, n = 1)))
  }
}
shiny::onStop(function() if (mirai::daemons_set()) mirai::daemons(0))

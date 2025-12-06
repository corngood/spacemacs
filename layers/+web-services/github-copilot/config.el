;;; config.el --- github-copilot layer configuration file for Spacemacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Aaron Zeng

;; Author: Aaron Zeng <z@bcc32.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defvar github-copilot-enable-commit-messages nil
  "If non-nil, automatically generate commit messages (for magit) using Copilot.
If set to 't', it uses the standard Copilot prompt.
If set to 'golem', it uses the strict G.O.L.E.M. (Tim Pope) standard.")

(defcustom github-copilot-mcp-servers nil
  "A list of MCP servers for `mcp-hub' to manage.
This list is the 'Single Source of Truth' for all MCP servers.
It is used by `mcp-hub' (for the management buffer) and by
`copilot-chat' (to provide tools to the AI).

The format is an alist: '((\"name\" . (:command \"...\" :args (\"...\")))
                          (\"ollama\" . (:url \"http://localhost:11434\")))"
  :group 'github-copilot
  :type '(list (cons string (list symbol string))))

(defconst github-copilot--golem-jokes
  '("# [G.O.L.E.M.:%s] *Grind*... Assessing structural integrity..."
    "# [G.O.L.E.M.:%s] *Crack*... Why did the bug not cross the road? It was null-terminated. *Rumble*."
    "# [G.O.L.E.M.:%s] *Krrzt*... Detecting creative liberties in your diff. Applying statutes..."
    "# [G.O.L.E.M.:%s] *Grind*... Parsing shards of logic... Integrity at 85%..."
    "# [G.O.L.E.M.:%s] *Sigh*... Another commit? The stone is tired. Processing..."
    "# [G.O.L.E.M.:%s] *Rumble*... In the old days, we carved code into mountains. This will suffice.")
  "A collection of G.O.L.E.M. status messages (jokes) for commit generation.
Must contain %s if you want to display the model name, or be safe for `format'.")

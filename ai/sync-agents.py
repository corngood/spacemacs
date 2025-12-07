"""Install Spacemacs AI Agents for GitHub Copilot and Gemini CLI."""

import os
import re

# ==========================================
#  SPACEMACS AGENT BUILDER (V10 - UNIFIED GOD MODE)
# ==========================================
# GOAL:
# 1. Parses ALL three .md files (Specialists, Strategists, Simulation).
# 2. Generates CLI commands for the entire crew.
# 3. Injects the correct System Prompt for each type.
# ==========================================

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = ".github"
GEMINI_CMD_DIR = os.path.join(".gemini", "commands")

# Configuration: Source Files and their Start Markers
SOURCES = [
    {
        "file": "coding_ai.md",
        "marker": "### The Specialist Team Roster",
        "type": "specialist"
    },
    {
        "file": "general_ai.md",
        "marker": "### Strategic & Authoring Roles (Your Team)",
        "type": "strategic"
    },
    {
        "file": "stakeholder_ai.md",
        "marker": "## The Team: Personas & Activation",
        "type": "simulation"
    }
]

# Profile Mapping (Specialists need tools, others usually don't)
PROFILE_MAP = {
    "spacky": "ai/profile_elisp.md",
    "bzzrts": "ai/profile_emacs_ui.md",
    "nexus-7": "ai/profile_layers.md",
    "vala_grudge_keeper": "ai/profile_ci_github.md",
    "don_testote": "ai/profile_elisp_testing.md",
    "golem": "ai/profile_doc.md"
}

def ensure_dir(directory):
    if not os.path.exists(directory):
        os.makedirs(directory)

def clean_slug(name):
    # Cleans names for filenames (lowercase, no special characters, snake_case)
    # Remove things like "(Default)" or "(The Flaw-Seer)" from filenames
    simple_name = name.split("(")[0].strip()
    return simple_name.lower().replace(".", "").replace(" ", "_").replace("'", "")

def parse_agents_from_text(roster_content, source_type):
    """Parses the nested list structure."""
    agents = []
    # Generic splitter based on the key (Role or Name)
    raw_splits = re.split(r"(?m)^-\s+\*\*(Role|Name):\*\*\s+", roster_content)

    # raw_splits[0] is intro text

    # We need to iterate in pairs (key, chunk)
    iterator = iter(raw_splits[1:])
    for key, chunk in zip(iterator, iterator):

        role = "Unknown"
        name = "Unknown"

        if key == "Role":
            role = chunk.split("\n")[0].strip()
            # Find name in chunk
            name_match = re.search(r"-\s+\*\*Name:\*\*\s+(.*?)$", chunk, re.MULTILINE)
            name = name_match.group(1).strip() if name_match else "Unknown"
        elif key == "Name":
            name = chunk.split("\n")[0].strip()
            # Find role in chunk or set default
            role_match = re.search(r"-\s+\*\*Role:\*\*\s+(.*?)$", chunk, re.MULTILINE)
            role = role_match.group(1).strip() if role_match else "Simulation Persona"

        # Simplify name for the file slug
        slug_name = clean_slug(name)

        # Clean body (add the Key line back for context)
        full_body = f"- **{key}:** {chunk}"

        agents.append({
            "name": name,
            "slug": slug_name,
            "role": role,
            "body": full_body,
            "type": source_type
        })
    return agents

def generate_copilot_files(global_headers, agents):
    """Generates .github structure for GitHub Copilot"""
    print(f"📝 Generating GitHub Copilot Config in {BASE_DIR}...")
    agents_dir = os.path.join(BASE_DIR, "agents")
    ensure_dir(agents_dir)

    table_rows = []
    for agent in agents:
        cmd = f"`@{agent['slug'].replace('_', '-')}`"
        table_rows.append(f"| **{agent['name']}** | {cmd} | {agent['role']} |")

    agent_table = "\n".join(table_rows)

    # Combine headers for the main instructions
    combined_header = (
        "# Strategic Vision\n" + global_headers.get("strategic", "") +
        "\n\n# Operational Rules\n" + global_headers.get("specialist", "") +
        "\n\n# Simulation Rules\n" + global_headers.get("simulation", "")
    )

    global_content = f"""# Spacemacs Maintainer Framework

{combined_header}

## 🧭 Agent Router

| Agent | Command | Specialization |
| :--- | :--- | :--- |
{agent_table}
"""
    with open(os.path.join(BASE_DIR, "copilot-instructions.md"), "w", encoding="utf-8") as f:
        f.write(global_content)

    for agent in agents:
        path = os.path.join(agents_dir, f"{agent['slug'].replace('_', '-')}.agent.md")

        # Create Agent specific prompt
        yaml = f"---\nname: {agent['slug'].replace('_', '-')}\ndescription: {agent['role']}\n---"

        # Inject specific context rules (The System Prompt for this agent type)
        context = global_headers.get(agent["type"], "")

        content = f"{yaml}\n\n{context}\n\n# Identity: {agent['name']}\n{agent['body']}"

        with open(path, "w", encoding="utf-8") as f:
            f.write(content)

def generate_gemini_commands(global_headers, agents):
    """Generates .toml files for Gemini CLI"""
    print(f"💎 Generating Gemini CLI Commands in {GEMINI_CMD_DIR}...")
    ensure_dir(GEMINI_CMD_DIR)

    for agent in agents:
        slug = agent["slug"]

        # 1. Profile Check (Only for Specialists)
        profile_path = None
        for key, path in PROFILE_MAP.items():
            if key in slug:
                profile_path = path
                break

        # 2. Build TOML Content
        toolbox_section = ""
        if profile_path:
            toolbox_section = f"""
---
TOOLBOX (AUTO-LOADED):
!{{cat {profile_path}}}
"""
        elif agent["type"] == "strategic":
             toolbox_section = """
---
MODE: STRATEGIC PLANNING & ARCHITECTURE
(Focus on high-level design, user stories, and requirements. Use Github MCP if available to read issues.)
"""
        elif agent["type"] == "simulation":
             toolbox_section = """
---
MODE: USER SIMULATION
(Focus on subjective feedback, usability, and constraints. Do not write code.)
"""
        else:
            toolbox_section = """
---
TOOLBOX:
(No specific profile loaded. Ask user to load one if implementation is needed.)
"""

        # Choose the right System Prompt
        system_header = global_headers.get(agent["type"], "")

        prompt_text = f"""
SYSTEM INSTRUCTIONS:
{system_header}

---
AGENT PERSONA:
{agent['body']}
{toolbox_section}
---
USER INPUT:
{{{{args}}}}
"""
        clean_desc = agent['role'].replace('"', "'")
        toml_content = f'description = "{clean_desc}"\n'
        toml_content += 'prompt = """' + prompt_text + '"""\n'

        filename = f"{slug}.toml"
        path = os.path.join(GEMINI_CMD_DIR, filename)

        with open(path, "w", encoding="utf-8") as f:
            f.write(toml_content)

def main():
    all_agents = []
    global_headers = {}

    for source in SOURCES:
        file_path = os.path.join(SCRIPT_DIR, source["file"])
        if not os.path.exists(file_path):
            print(f"❌ Error: {file_path} not found.")
            continue

        print(f"🚀 Reading {source['file']}...")
        with open(file_path, "r", encoding="utf-8") as f:
            full_content = f.read()

        if source["marker"] not in full_content:
            print(f"⚠️ Warning: Marker '{source['marker']}' not found in {source['file']}.")
            continue

        # Split header (System Prompt) and Roster
        header = full_content.split(source["marker"])[0].strip()
        roster = full_content.split(source["marker"])[1].strip()

        global_headers[source["type"]] = header

        # Parse agents
        agents = parse_agents_from_text(roster, source["type"])
        all_agents.extend(agents)
        print(f"   Found {len(agents)} agents in {source['file']}.")

    # Generate files
    generate_copilot_files(global_headers, all_agents)
    generate_gemini_commands(global_headers, all_agents)

    print("\n✅ Done! Unified Framework Active.")
    print("   Total Agents Configured:", len(all_agents))

if __name__ == "__main__":
    main()

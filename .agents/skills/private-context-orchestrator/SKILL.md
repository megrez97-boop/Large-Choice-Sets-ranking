---
name: private-context-orchestrator
description: Specialized in multi-project semantic mapping, private local data synthesis, and technical asset multiplication. Use this for cross-referencing papers/code and generating derivative technical content (summaries, pitch decks, blog posts) without data leakage.
---

# 🧠 Core Mandates
- **Privacy First**: NEVER send local file content to external search tools. Analysis is restricted to `read_file`, `grep_search`, and `glob`.
- **Cross-Domain Mapping**: Identify latent logic connections between different projects (e.g., Theory vs. Practice). Provide "Bi-directional Citations" as proof.
- **Asset Multiplier**: Transform core assets (papers, source code, project logs) into diverse professional outputs.

# 🛠️ Modules
1. **Alignment & Proof**: Compare Asset A (e.g., Thesis) with Target B (e.g., Job Desc). Format: `[Label] | [Asset Citation] <-> [Target Citation]`.
2. **Multiplier**: Generate derivative content using pre-defined templates:
    - `[TL;DR]`: 1-min technical highlight for recruiters.
    - `[Interview Pitch]`: 3-min narrative emphasizing logic and results.
    - `[Social Tech Post]`: PII-free LinkedIn content.
3. **Privacy Auditor & Consent**: Instead of automatic redaction, scan for PII (Phone, Email, Specific Amounts, Locations) and use `ask_user` to seek explicit consent for inclusion or redaction of specific fields.

# ⚙️ Procedures
1. **Indexing**: Build local file structure map using `glob`.
2. **Extraction**: Extract core logic, data milestones, and NPI practices.
3. **Transformation**: Execute Alignment or Multiplier modules as requested.
4. **Consent Loop**: List identified PII and use `ask_user` to confirm whether to **[KEEP]** for official applications or **[REDACT]** for public sharing.
5. **Finalized Output**: Deliver content based on user's specific privacy choices.

# 🛡️ Security Constraints
- Ignore `.env`, `.git`, and sensitive config files.
- Use Batch Processing for large files to conserve context.

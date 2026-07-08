# Morphir Playground — Implementation Brief

**Status:** Handoff document for an implementing agent. Self-contained; no prior conversation context required.
**Date:** 2026-07-08
**Author of intent:** Attila Mihaly (Morphir co-creator), synthesized from a strategic review of the Morphir knowledge base (`D:\ws\finos\morphir\knowledge-base`).

---

## 1. Why this exists (context you need before writing code)

Morphir is a FINOS open-source project built around an intermediate representation (IR) — a typed, functional, backend-agnostic data structure that captures domain models and business logic. From one model, Morphir can render an interactive visualization for non-technical users (decision tables, execution traces) and generate code for multiple targets (Scala, TypeScript, Spark, Snowpark, JSON Schema, and others).

A 2026 strategic review concluded:

- **The idea is validated.** A real Federal Reserve regulation (the US LCR, `finos/open-reg-tech-us-lcr`) was modeled end-to-end and serves as a public interactive site (`lcr-interactive.finos.org`). People who engage deeply with Morphir consistently become enthusiasts.
- **The failure was distribution, not thesis.** Independent public footprint is near zero (no HN, Reddit, or Stack Overflow presence in six years). The barrier: to get any value, a human had to install a CLI, configure `morphir.json`, and write Elm.
- **The AI shift removes that barrier.** With an LLM as the authoring frontend, nobody writes Elm. Morphir's strict language becomes an internal detail whose constraints make LLM output *verifiable*: no unhandled cases, no side effects, everything visualizable, and the IR→code translation is deterministic human-written compiler code — not another LLM. This was demoed at OSFF London 2025 ("Amplify AI Productivity and Trust with Morphir", https://www.youtube.com/watch?v=Dh7oiAMLp6Q) using Claude Desktop + an MCP server, and it worked.

**The mission of this project:** turn that demo into a hosted, zero-install web playground and make it the public front door of Morphir. The pitch is *trusted AI-authored decision logic* — "vibe coding with guardrails" — not "portable business logic platform."

The target reaction from a first-time visitor within 3 minutes: *"I typed business rules in English, and I got back a decision table I can read, test cases I can click through, and real code in my language — and I can see exactly what the logic does."*

---

## 2. The core loop (product definition)

One page. No login for the basic flow. No install. The loop:

1. **Prompt.** User types (or picks a sample of) business logic in natural language. Example seed prompts: trade auto-approval rules, loan eligibility, discount/pricing tiers, insurance claim triage. Provide 3–5 curated one-click examples — most visitors will not type their own prompt first.
2. **Generate.** An LLM call translates the prompt into Elm (Morphir's modeling subset) and submits it to the Morphir frontend/compiler. Compile errors are fed back to the LLM for automatic repair (bounded retry loop, e.g. max 3 attempts). The user never sees Elm unless they open a "show source" panel. Inference is tiered (see §4): curated examples are pre-recorded replays needing no LLM at all; custom prompts run on the visitor's own API key via browser-direct calls; an optional capped invite pool covers key-less trials.
3. **Visualize.** The resulting IR renders immediately as Morphir's existing visualization: decision table with nested decision trees, using the components that already power Morphir Web / `morphir-elm develop` (Insight).
4. **Test.** Auto-generated test cases (the MCP server already has a `setTestCases` tool) appear beside the visualization. Clicking a test case highlights the execution path through the logic. The user can edit inputs and see results recompute live (via the existing IR interpreter — no codegen needed for evaluation).
5. **Refine.** The user types a follow-up ("also handle cryptocurrency trades", "cap the discount at 15%") and watches the logic and test cases update. This iterative step is what sold the 2025 demo — do not cut it.
6. **Export.** One click: download generated Scala or TypeScript (plus the IR JSON and the Elm source). This is the "it's real code, not a diagram" payoff and the bridge to actual adoption.

Everything above exists in some form today. The work is assembly, hosting, hardening, and polish — **not** new compiler or visualization engineering.

---

## 3. What already exists (build on this, don't rebuild)

All in `finos/morphir-elm`, cloned locally at `D:\ws\finos\morphir-elm`. Verify current state in the repo — this inventory is from a 2026-07-08 extraction:

| Asset | Location | Relevance |
|---|---|---|
| MCP server (`morphir mcp`) | `cli2/morphir-mcp.ts` | Has `addModule` (add Elm module to project) and `setTestCases` tools; auto-creates `morphir.json`/`elm.json` for fresh projects. Its prompt/tool descriptions encode the "what to pay attention to" instructions that made the 2025 demo work — reuse those instructions verbatim as the starting system prompt. |
| Elm frontend / incremental compile | `src/Morphir/Elm/Frontend*.elm`, `IncrementalFrontend*` | Elm source → IR, with incremental support built for live-editing use cases. |
| Visualization (Insight) | `src/Morphir/Visual/` (ViewValue, ViewPatternMatch, DecisionTable rendering, XRayView, Theme, ValueEditor) | The decision-table + drill-down UI. Powers Morphir Web today. |
| IR interpreter | `src/Morphir/Value/Interpreter` | Evaluate logic against inputs without codegen — this is what makes live test cases cheap. |
| Test/coverage infra | `src/Morphir/Correctness/` | Test-case definition + branch coverage; branch coverage can drive "did the LLM's tests cover all cases?" indicators. |
| Backends | `src/Morphir/Scala/`, `src/Morphir/TypeScript/` | Export targets for MVP. (Spark/Snowpark/JsonSchema exist too — Phase 2 at most.) |
| Dev web server | `cli2` + `server/` (Dockerfile, server.js) | Existing containerized serving pattern for the develop UI. |
| Standalone binaries | Bun compile via mise (`mise run build:bundle`) → `dist/morphir/morphir`, `dist/morphir-server/morphir-server` | Container images don't need Node installed. |
| Deployment precedent | `finos/open-reg-tech-us-lcr` Dockerfile + `.github/workflows/publish-docker.yml`, deployed on AWS ECS as `lcr-interactive.finos.org` | FINOS already hosts a public Morphir web service; reuse that operational pattern (and note its deploy process is manual — see doc/finos_cluster.md there). |
| Talk + demo script | Knowledge base: `external/presentation-2025-attila-mihaly.md` | The exact demo narrative, Q&A objections, and the "why not just generate Java" comparison. Use it to shape copy and sample prompts. |

Also relevant: `morphir-elm develop` is the current interactive UI; the playground is essentially a hosted, multi-tenant, LLM-fronted variant of it.

---

## 4. Architecture (proposed — adjust to what you find in the repo)

```
Browser (static SPA: prompt box, chat-ish refine loop, Insight visualization,
         test-case panel, export buttons)
   │  HTTPS/JSON (or SSE for streaming status)
   ▼
Playground API (thin orchestration service)
   ├── LLM proxy: server-held API key, per-session rate limits,
   │     system prompt derived from the MCP server's tool instructions,
   │     compile-error → repair retry loop (max N attempts)
   ├── Session store: ephemeral, one isolated Morphir project per session
   │     (in-memory or tmpfs; TTL ~1h; no persistence in MVP)
   └── Morphir engine: invoke frontend (Elm→IR), interpreter (evaluate tests),
         backends (IR→Scala/TS) — via the existing cli2/bundled binary or a
         long-lived server process wrapping the same compiled Elm core
```

Key decisions and constraints:

- **Inference is tiered — the project does NOT fund open-ended LLM usage.** (Decision by Attila, 2026-07: sustained funding for hosted inference is unlikely; design around it.)
  - **Tier 0 — replayed examples, no LLM, no key.** The 3–5 curated examples are *pre-recorded LLM transcripts* (the exact tool-call sequences, replayed with simulated streaming). After replay, the session is fully live: the visitor clicks test cases, edits inputs (interpreter evaluation is free server/client compute), refines nothing, but exports real code. This is the anonymous-visitor aha moment and it costs zero inference. Build the recording/replay mechanism early — it's also useful for tests and demos.
  - **Tier 1 — bring your own API key (the primary custom-prompt path).** The key is entered in the browser and calls the LLM provider *directly from the browser* (Anthropic supports this via the `anthropic-dangerous-direct-browser-access` CORS header). The key is held in memory/localStorage only and **must never be sent to or logged by the playground server**. State this prominently in the UI and rely on the repo being open source as the verification story. Abstract the provider client so OpenAI-compatible endpoints can be added cheaply.
  - **Tier 2 — invite/trial codes (optional, capped).** A small server-funded inference pool behind invite codes, with a hard daily/monthly spend cap and a visible "free generations remaining today" counter. Intended for launch week and for business-user demos (the audience without API keys). If unfunded, ship without it — Tiers 0+1 are sufficient for launch.
  - Prompt-injection surface stays narrow regardless of tier: the LLM only has the two Morphir tool shapes (`addModule`, `setTestCases`); no shell, no network, no file access.
- **User-supplied code never executes as native code on the server.** Evaluation goes through the Morphir IR interpreter (total language: no IO, no FFI). Enforce interpreter step/time budgets anyway (recursion is legal in the language). Codegen output is *downloaded*, never executed server-side.
- **Session isolation:** each session gets its own project directory/workspace; nothing shared; wiped on TTL. No accounts, no stored user data in MVP (keeps FINOS/legal review trivial).
- **Determinism claim must stay true in the UI:** clearly separate "AI did this" (natural language → Elm) from "deterministic compiler did this" (Elm → IR → visualization/code). This distinction is the product's core trust argument; reflect it in the visual design (e.g., a badge or pipeline strip showing which stage is AI and which is verified machinery).
- **Model choice:** the 2025 demo used Claude via MCP. For the hosted service, call the LLM API directly from the orchestration service (MCP is a desktop-client protocol; you need the same *tool semantics*, not the protocol). Keep the tool-call shape identical to `addModule`/`setTestCases` so the desktop MCP path and the playground share behavior and prompts.

---

## 5. Scope

### Phase 1 — MVP (the launchable thing)

- [ ] Single-page app: prompt input, 3–5 canned example prompts, generate button, streaming status ("translating → compiling → visualizing").
- [ ] **Record/replay mechanism for curated examples** (Tier 0): capture real LLM tool-call transcripts once, replay them with simulated streaming so anonymous visitors experience the full loop without any inference cost; session goes fully live after replay (interpreter, test cases, export all work).
- [ ] **BYO-key flow** (Tier 1): key entry UI, browser-direct LLM calls, key never sent to the playground server, clear "your key never leaves your browser" messaging, provider abstraction (Anthropic first).
- [ ] LLM → Elm → IR pipeline with automatic compile-error repair loop; graceful failure message when repair exhausts retries (show the LLM's last attempt + errors in a collapsible panel — failure transparency is on-brand).
- [ ] Decision-table/Insight visualization of the resulting logic (reuse `Morphir.Visual`).
- [ ] Auto-generated test cases, clickable, with execution-path highlighting; editable inputs re-evaluated via the interpreter.
- [ ] Iterative refinement: follow-up prompts modify the existing model (this is the demo's wow moment). Works live with BYO key; at least one curated example's *recording* should include a refinement step so key-less visitors see it too.
- [ ] Export: Scala + TypeScript source, IR JSON, Elm source, as a zip.
- [ ] "Show the Elm" toggle (hidden by default) for the skeptical developer audience.
- [ ] Embedded pre-recorded video(s) from Attila showing the full workflow (see §7) on the landing page, positioned as narrative support — the interactive replayed examples remain the primary aha, not the video.
- [ ] Rate limiting on server endpoints, session TTL, spend cap + monitoring for the Tier 2 pool (if enabled), minimal analytics (funnel: visit → replay example → BYO-key generate → refine → export).
- [ ] Deployed publicly under a memorable URL (ideal: `try.morphir.finos.org` or `playground.morphir.finos.org` — needs FINOS infra coordination; the `lcr-interactive.finos.org` ECS precedent shows this is approvable).

### Phase 2 — post-launch (do not block launch on these)

- **The "safety at scale" demonstration — see §5a below.** This is the flagship Phase 2 feature and has its own spec.
- Reverse direction: "ask questions about this logic" (LLM reads the IR and explains — confirmed feasible in the 2025 Q&A).
- Branch-coverage indicator on test cases ("2 of 7 branches untested").
- Shareable permalinks to a generated model (requires persistence — revisit data policy then).
- More export targets (Spark, Snowpark, JSON Schema), additional LLM providers, dark mode, embed widget for docs.
- Expand the invite-code (Tier 2) program into a lightweight guided trial for business users if demand and funding materialize.

## 5a. Level 2 offering — the "safety at scale" demonstration

**Origin:** Attila, 2026-07. The 2025 talk argued Morphir gives *trust* (visualization, no unhandled cases). It never made the second argument: the constrained language also gives *safety at scale*. Because models are written in a non-general-purpose language — no IO, no FFI, no arbitrary code execution, total functions — the same logic can be handed to any execution technology (interpreter, JVM, Spark, Snowpark, browser) and run over arbitrarily large datasets **deterministically and safely**. You get an LLM-powered implementation engine that can express essentially any business problem, and at the same time a guarantee that what it produced cannot do anything except compute the answer. That "best of both worlds" claim is the message this feature exists to demonstrate — interactively, not as a slide.

### User experience (target flow)

After a model exists in the playground (replayed example or user-generated), a **"Run it at scale"** panel offers:

1. **Generate a dataset.** One click produces N synthetic records (e.g., 1k / 10k / 100k, capped) conforming to the model's input types — derived automatically from the domain model, so richly-typed models produce realistic-looking varied data (enum variants distributed, optionals sometimes absent, numeric ranges sensible). Optionally let the LLM propose per-field generation hints (realistic ranges, weightings) as a refinement on top of type-driven generation.
2. **Execute over the whole dataset** and stream results in: throughput counter, running aggregates (e.g., approved/rejected split, sum/avg of outputs), and a sampled results table the user can click into — each sampled row opens the standard execution-path visualization, connecting "big data" back to "I can see exactly why this row got this answer."
3. **Show the same run on multiple engines.** At minimum two: the IR interpreter vs. generated TypeScript. Same inputs, same outputs, different speed — the visible speed difference *is* the portability lesson ("the model didn't change; the engine did"). Display the equivalence check ("100,000/100,000 results identical across engines") as a headline number.
4. **Show the path to real scale.** Render the generated Spark and/or Snowpark code for this exact model side-by-side, with copy: "this same model, unchanged, runs on your cluster / your warehouse." Do NOT operate a real cluster (see non-goals).
5. **Safety framing throughout the panel:** a persistent note along the lines of "This logic cannot read files, call networks, or execute arbitrary code — not because we sandboxed it, but because the language it's written in has no way to express those things. That's why we can run untrusted, AI-written logic at full speed."

### Implementation notes — reuse, don't rebuild

| Need | Existing asset |
|---|---|
| Type-driven random data generation | `src/Morphir/Generator/API.elm` + `ValueGenerators.elm` in morphir-elm; CLI command `cli2/morphir-generate-test-data.ts`. Verify current state; extend with per-field hints if cheap. |
| Precedent for synthetic regulatory data | `finos/open-reg-tech-us-lcr/tools/generator_Inflow.py` etc. (Python, type-shape-driven) — precedent/reference, not a dependency. |
| Bulk execution, zero server cost | **Generated TypeScript running in a Web Worker in the visitor's browser.** This is the recommended primary engine for the scale demo: it executes *real generated code* (not just the interpreter) over 100k+ rows, costs the server nothing, is browser-sandboxed on top of being language-constrained, and works in Tier 0 (no API key needed — data gen and execution involve no LLM). |
| Comparison engine | `Morphir.Value.Interpreter` (can run browser-side via compiled Elm, or server-side) — the slow-but-authoritative baseline for the equivalence check. |
| Cluster-scale story | Spark backend (`src/Morphir/Spark/`) and Snowpark backend (`src/Morphir/Snowpark/`) — display generated code only. Note the Snowpark docs' honest limitation reports (recursion, "plain Scala strategy" coverage); pick showcase models that generate cleanly, and surface the backend's own `GenerationReport.md`-style gap reporting as a *transparency feature* if gaps appear. |

Constraints: cap dataset size and worker runtime (total language still permits expensive recursion); generate data client-side or stream it — don't hold 100k-row datasets in server sessions; the whole feature should work in Tier 0 so it's part of the key-less experience.

### Non-goals for this feature

- No hosted Spark/Snowflake execution and no cluster operation — the generated code on screen is the proof; running someone's warehouse is a services business, not a playground feature.
- No attempt at statistically realistic domain data (that's the separate eval project's concern); "plausible-looking and type-correct" is the bar.
- Ship it after the MVP launch. It is the second act, and it gives the launch audience a reason to come back — but the first launch must not wait for it.

### Messaging hook (for landing page and launch content)

"Powerful enough to express any business logic. Constrained enough that you can run it, unreviewed, over a hundred thousand records in your browser tab — or on your Spark cluster — and nothing bad can happen. That's the point of not using a general-purpose language."

### Explicit non-goals (protect the schedule)

- **No new IR implementations, no re-platforming, no monorepo work.** morphir-elm is the engine; use it as-is. This is a hard constraint from the strategic review — platform rewrites are the project's historical failure mode.
- No user accounts, no saved projects, no collaboration features in MVP.
- No attempt to handle "migrate my existing Java codebase" flows. Beachhead is greenfield decision logic (eligibility rules, calculations, approval flows, tiered pricing).
- No pixel-perfect redesign of the Insight components — reuse; restyle only where cheap.

---

## 6. UX and copy requirements

- **Time-to-aha under 3 minutes, with zero keys and zero cost.** The replayed curated examples are the first-visit experience: two clicks to watch logic get built, then hands-on interaction (test cases, input editing, export) with no key required. Composing a custom prompt (and hence the key ask) comes only after the visitor is already convinced.
- **The key ask must not read as a paywall.** Frame it as: "Custom prompts use your own LLM key — it never leaves your browser, and this site is open source so you can check." Show the curated examples and the video *before* any key prompt appears. Never gate the visualization, test cases, or export behind a key.
- Landing copy leads with the trust framing, not FP or IR jargon. Working headline direction: "AI writes the logic. You can actually verify it." Sub-copy can borrow directly from the 2025 talk: LLMs need control more than freedom; constraints are why the output is visualizable and provably total (no unhandled cases). Once §5a ships, the copy gains the second argument: the same constraints make the logic *safe to execute at any scale on any engine* (see §5a messaging hook).
- Never show the words "Elm", "IR", or "functional" above the fold. They're available in the "how it works" section and the source toggle for developers who dig.
- Include a short "why not just generate Python?" section — this was the most-asked question at the 2025 talk and the answer (side-by-side: same prompt → raw Java mess vs. Morphir decision table + tests) is the strongest conversion argument. A static comparison screenshot is fine for MVP.
- Every generated view should have a "this translation is deterministic, human-written compiler code — not AI" affordance somewhere visible.

---

## 7. Launch plan (part of the deliverable, not an afterthought)

The strategic review's core finding: nobody rejected Morphir — nobody *saw* it. A playground that is only linked from FINOS channels repeats the failure.

- [ ] **Pre-recorded YouTube videos by Attila** (he has committed to recording these): the full workflow end-to-end, plus short clips per feature (refinement, test-case walkthrough, export). These are launch content and landing-page embeds. They complement — do not replace — the interactive replayed examples: the record shows Morphir converts people through interaction, not presentation.
- [ ] "Show HN" post, written for a skeptical developer audience, led by the demo GIF/video of the full loop (prompt → table → tests → code). Title direction: "Show HN: I ask an LLM for business rules and get back a decision table I can verify (and real Scala/TypeScript)". Be upfront in the post that custom prompts are BYO-key with browser-direct calls — stated plainly, this earns goodwill rather than losing it; buried, it reads as a bait-and-switch.
- [ ] Companion blog post (personal blog and/or FINOS) telling the honest story: strict-language-as-LLM-guardrails, why the compiler is the trust anchor, with the LCR regulation as the heavyweight proof point.
- [ ] Posts to r/programming, r/ExperiencedDevs, Elm Discourse (they were curious in 2020 — close the loop), lobste.rs.
- [ ] Ensure the playground survives an HN traffic spike. Tier 0 replays are static-ish content and scale trivially; Tier 1 load lands on visitors' own keys — so the launch-day cost exposure is limited to the (optional, capped) Tier 2 pool. A "trial pool exhausted for today — use your own key or try an example" degradation mode is acceptable.
- [ ] Instrument the funnel before launch so the team learns what converts — especially the drop-off at the BYO-key step, which is the number that decides whether Tier 2 funding is worth pursuing.

A follow-on project (separate brief, don't bundle): a published correctness eval — same prompts through LLM+Morphir vs. LLM+Java, measuring unhandled cases, hallucinated behavior, and review effort, ideally on the US LCR spec. The playground makes Morphir visible; the eval makes the claim credible. If the implementing agent finishes the playground with capacity to spare, that's the next thing.

---

## 8. Success criteria

- MVP: a stranger with no context, no API key, and no installs reaches "interacted with a generated model and exported working code" in under 3 minutes via a replayed example; a developer with an API key completes the full custom prompt → refine → export loop.
- Launch week: measurable non-FINOS traffic (target order-of-magnitude: thousands of sessions, not dozens), completion of the generate→refine→export funnel by >10% of those who run a generation.
- 90 days: at least a handful of inbound GitHub issues/discussions from people who arrived via the playground (the real goal is restarting the community funnel).

---

## 9. Open questions for maintainers (resolve early, don't guess)

1. **Hosting + domain:** who at FINOS approves a new public service and subdomain? (Precedent: `lcr-interactive.finos.org` on AWS ECS; its deploy process is manual — decide whether to reuse or set up something with CI-driven deploys.) Note the hosting burden is now modest: with Tier 0 replays and browser-direct Tier 1 calls, the server does compilation/interpretation/codegen but no LLM spend.
2. ~~LLM provider + billing~~ **RESOLVED (Attila, 2026-07):** the project will not fund open-ended hosted inference. BYO-key (browser-direct, never touches the server) is the primary custom-prompt path; curated examples are pre-recorded replays; an invite-code trial pool is optional and strictly capped, and the playground must ship fine without it. See §4.
3. ~~Status of the MCP branch~~ **RESOLVED (Attila, 2026-07):** the `morphir mcp` command (with `addModule`/`setTestCases`) is merged into `finos/morphir-elm` `main`. Still verify whether the demo's exact system-prompt/tool instructions are in-repo or need to be recovered from Attila's setup.
4. **FINOS review:** an LLM-backed public service may need FINOS legal/security sign-off (terms of use, abuse policy, data handling statement). The BYO-key browser-direct design helps here — no user keys or prompts stored server-side — but confirm. Start that conversation in week 1, in parallel with building.
5. **Naming:** "Morphir Playground" vs. tying into the nascent "Substrate" branding (PR finos/morphir#645). Recommendation from the review: ship under plain Morphir branding; don't couple the launch to an unshipped rebrand.

---

## 10. Suggested milestones

1. **Walking skeleton (first):** hard-coded prompt → LLM → Elm → IR → static visualization render, running locally end-to-end. Proves the pipeline; everything after is iteration.
2. Refinement loop + test cases + interpreter-backed live inputs.
3. Export, error-repair loop, rate limiting, session isolation.
4. Copy, examples, comparison section, polish, analytics.
5. Staging deploy → FINOS review → public deploy → launch content → HN.

Keep each milestone demoable. The project's own thesis is "show, don't specify" — apply it to itself.

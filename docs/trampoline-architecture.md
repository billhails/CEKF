# Trampoline Architecture

## Purpose

The CPS transform uses a trampoline to avoid deep C recursion in Tc/Tk processing.

The core idea is to represent pending work as a `CpsWork` value and execute it in a loop until a terminal result is reached.

## Core Components

- `CpsWork`: tagged union of work items (Tc, Tk, Tsk, Invoke, Result, payload-related tags, and resume tags).
- `cpsStep`: dispatcher that routes work tags to `cpsStepTc` or `cpsStepTk`.
- `runCpsWorkToResult`: main loop for expression-producing work.
- `runCpsWorkToPayload`: main loop for list/payload-producing work.

Primary entrypoint:

- `runCpsTrampolineTc(rootExp, cont0)` in `src/minlam_cpsTrampoline.c`.

## Execution Model

1. Create initial work item (for example `TcRoot`).
2. Enter trampoline loop.
3. Dispatch one step by tag.
4. Return next work item.
5. Repeat until a terminal tag (`Result` or `PayloadResult`).

This keeps stack usage bounded by the loop shape rather than source program nesting depth.

## Nested Trampoline Calls

Some step handlers run child work immediately by creating a new `CpsWork` and calling `runCpsWorkToResult` (or payload equivalent).

This is a deliberate local "sub-trampoline" pattern:

- Parent work item suspends at a sub-problem.
- Child work graph is fully evaluated in the same C frame.
- Parent resumes using the returned value.

This replaces direct recursive Tc/Tk calls with bounded-loop execution while preserving the existing continuation structure.

## Current Invariant

`cpsStepTc` and `cpsStepTk` should treat expected functional `MinExp` variants explicitly.

The inner `default` arm is reserved for truly unexpected or internal-only variants and should fail fast (`cant_happen`).

## Relationship to Proposal

This document summarizes the implemented execution model.

For historical rationale, migration strategy, and tag taxonomy details, see [`docs/trampoline-proposal.md`](trampoline-proposal.md).

#!/usr/bin/env python3
#
# Copyright (C) 2026  Bill Hails
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

"""
git_to_mermaid - Generate Mermaid gitgraph diagrams from git history.

Reads git log via GitPython and outputs Mermaid gitGraph syntax to stdout.
"""

import argparse
import re
import sys

import git


def parseArgs():
    parser = argparse.ArgumentParser(
        description="Generate a Mermaid gitgraph diagram from git history."
    )
    parser.add_argument(
        "--repo", default=".", help="Path to git repository (default: .)"
    )
    parser.add_argument(
        "--start", default=None, help="Start commit (inclusive, oldest bound)"
    )
    parser.add_argument(
        "--end", default=None, help="End commit (inclusive, newest bound)"
    )
    parser.add_argument(
        "--mode",
        choices=["all", "main"],
        default="all",
        help="'all' = all local branches, 'main' = first-parent only (default: all)",
    )
    parser.add_argument(
        "--max-commits",
        type=int,
        default=50,
        help="Maximum number of commits to display (default: 50)",
    )
    parser.add_argument(
        "--tags", action="store_true", help="Include git tags on commits"
    )
    parser.add_argument(
        "--label",
        choices=["hash", "subject"],
        default="hash",
        help="Commit label style: short hash or subject line (default: hash)",
    )
    parser.add_argument(
        "--markdown", action="store_true",
        help="Wrap output in a ```mermaid code fence",
    )
    parser.add_argument(
        "--direction", default="TB",
        help="Graph direction: LR, RL, TB/TD, BT/BU (case-insensitive, default: TB)",
    )
    parser.add_argument(
        "--squash", action="store_true",
        help="Show only merge commits, with one commit per branch to keep the graph valid",
    )
    return parser.parse_args()


def normalizeDirection(direction):
    """Normalize direction aliases to Mermaid-accepted values."""
    mapping = {"TD": "TB", "BU": "BT"}
    d = direction.upper()
    d = mapping.get(d, d)
    if d not in ("LR", "RL", "TB", "BT"):
        print(f"Error: invalid direction '{direction}'. "
              "Use LR, RL, TB/TD, or BT/BU.", file=sys.stderr)
        sys.exit(1)
    return d


def findMainBranch(repo):
    """Return the name of the main branch ('main' or 'master')."""
    for name in ("main", "master"):
        if name in repo.heads:
            return name
    return repo.heads[0].name if repo.heads else "main"


def collectCommits(repo, args, mainBranch):
    """Collect the most recent commits in chronological (oldest-first) order."""
    if args.mode == "main":
        revSpec = args.end if args.end else mainBranch
        kwargs = {"first_parent": True}
    else:
        revSpec = args.end if args.end else [h.commit for h in repo.heads]
        kwargs = {}

    if args.start:
        if isinstance(revSpec, list):
            commits = set()
            startSha = repo.commit(args.start).hexsha
            for head in revSpec:
                for c in repo.iter_commits(head, **kwargs):
                    if c.hexsha == startSha:
                        commits.add(c)
                        break
                    commits.add(c)
        else:
            commits = list(repo.iter_commits(f"{args.start}^..{revSpec}", **kwargs))
    else:
        if isinstance(revSpec, list):
            commits = set()
            for head in revSpec:
                for c in repo.iter_commits(head, **kwargs, max_count=args.max_commits):
                    commits.add(c)
        else:
            commits = list(repo.iter_commits(revSpec, **kwargs))

    ordered = sorted(commits, key=lambda c: c.committed_date)
    if args.squash:
        # collect generously; squash + max_commits applied later
        return ordered
    # take the most recent N commits, not the oldest N
    return ordered[-args.max_commits:]


def buildTagMap(repo):
    """Build a mapping from commit SHA to tag name."""
    tagMap = {}
    for tag in repo.tags:
        try:
            sha = tag.commit.hexsha
        except ValueError:
            continue
        tagMap[sha] = tag.name
    return tagMap


def inferBranchFromMerge(commit):
    """Try to extract a branch name from a merge commit message.

    Handles patterns like:
      "Merge pull request #N from user/branch-name"
      "Merge branch 'branch-name' into ..."
      "Merge branch 'branch-name'"
    """
    msg = commit.summary
    m = re.search(r"from\s+\S+/(\S+)", msg)
    if m:
        return m.group(1)
    m = re.search(r"branch\s+'([^']+)'", msg)
    if m:
        return m.group(1)
    m = re.search(r'branch\s+"([^"]+)"', msg)
    if m:
        return m.group(1)
    return None


def assignBranches(repo, commits, mainBranch):
    """Assign each commit to a branch name.

    Strategy:
    1. Walk each branch's first-parent chain, claiming unclaimed commits.
       Main branch gets priority, then alphabetical.
    2. For merge commits whose second parent isn't claimed by any live branch,
       infer a branch name from the merge message and assign the second-parent
       chain to that inferred branch.
    3. Any remaining unassigned commits go on the main branch.
    """
    commitSet = {c.hexsha for c in commits}
    commitMap = {c.hexsha: c for c in commits}
    assignment = {}

    branches = sorted(repo.heads, key=lambda b: b.name)
    mainRef = None
    others = []
    for b in branches:
        if b.name == mainBranch:
            mainRef = b
        else:
            others.append(b)

    orderedBranches = []
    if mainRef:
        orderedBranches.append(mainRef)
    orderedBranches.extend(others)

    for branch in orderedBranches:
        current = branch.commit
        while current:
            sha = current.hexsha
            if sha in commitSet and sha not in assignment:
                assignment[sha] = branch.name
            parents = current.parents
            current = parents[0] if parents else None

    # infer branches from merge commits for deleted/remote-only branches
    usedNames = set()
    for c in commits:
        if len(c.parents) > 1:
            secondParent = c.parents[1]
            if secondParent.hexsha in commitSet and secondParent.hexsha not in assignment:
                inferred = inferBranchFromMerge(c)
                if inferred is None:
                    inferred = f"branch-{secondParent.hexsha[:7]}"
                # deduplicate inferred names
                name = inferred
                counter = 2
                while name in usedNames or name in {b.name for b in repo.heads}:
                    name = f"{inferred}-{counter}"
                    counter += 1
                usedNames.add(name)
                # walk second-parent first-parent chain
                current = secondParent
                while current:
                    sha = current.hexsha
                    if sha in commitSet and sha not in assignment:
                        assignment[sha] = name
                    parents = current.parents
                    current = parents[0] if parents else None

    for c in commits:
        if c.hexsha not in assignment:
            assignment[c.hexsha] = mainBranch

    return assignment


def squashCommits(commits, assignment, mainBranch, maxCommits):
    """Reduce commits to merges plus one representative per feature branch."""
    mergeSet = {c.hexsha for c in commits if len(c.parents) > 1}

    # find which branches are involved in merges (as source)
    mergeSrcBranches = set()
    for c in commits:
        if len(c.parents) > 1:
            for p in c.parents[1:]:
                if p.hexsha in assignment:
                    mergeSrcBranches.add(assignment[p.hexsha])

    # for each non-main branch, pick one representative commit (the earliest)
    branchRep = {}
    for c in commits:
        branch = assignment.get(c.hexsha, mainBranch)
        if branch != mainBranch and branch not in branchRep:
            if c.hexsha not in mergeSet:
                branchRep[branch] = c.hexsha

    keepSet = set()
    keepSet.update(mergeSet)
    keepSet.update(branchRep.values())

    # also keep commits on main that aren't merges only if main has no
    # commits at all otherwise (need at least one for branching)
    mainCommits = [c for c in commits if assignment.get(c.hexsha, mainBranch) == mainBranch]
    mainNonMerge = [c for c in mainCommits if c.hexsha not in mergeSet]
    mainInKeep = [c for c in mainCommits if c.hexsha in keepSet]
    if not mainInKeep and mainNonMerge:
        keepSet.add(mainNonMerge[-1].hexsha)

    result = [c for c in commits if c.hexsha in keepSet]
    return result[-maxCommits:]


def sanitizeBranchName(name):
    """Wrap branch name in quotes if it contains special characters."""
    if any(ch in name for ch in ' \t/"\''):
        escaped = name.replace('"', '\\"')
        return f'"{escaped}"'
    return name


def makeLabel(commit, style):
    """Generate the label string for a commit."""
    if style == "subject":
        subj = commit.summary
        if len(subj) > 30:
            subj = subj[:27] + "..."
        subj = subj.replace('"', "'")
        return subj
    return commit.hexsha[:7]


def emitGitgraph(commits, assignment, tagMap, args, mainBranch):
    """Emit the Mermaid gitgraph to stdout."""
    direction = normalizeDirection(args.direction)

    if mainBranch != "main":
        print("---")
        print("config:")
        print("  gitGraph:")
        print(f'    mainBranchName: "{mainBranch}"')
        print("---")

    print(f"gitGraph {direction}:")

    createdBranches = {mainBranch}
    currentBranch = mainBranch
    commitSet = {c.hexsha for c in commits}
    # track used labels to avoid Mermaid duplicate id errors
    usedLabels = set()
    # track which branches have had at least one commit emitted
    branchHasCommits = set()

    for commit in commits:
        branch = assignment.get(commit.hexsha, mainBranch)
        isMerge = len(commit.parents) > 1

        if branch not in createdBranches:
            parentBranch = findParentBranch(commit, assignment, commitSet, mainBranch)
            if parentBranch not in createdBranches:
                parentBranch = mainBranch
            # Mermaid requires a branch to have at least one commit before
            # another branch can be created from it
            if parentBranch not in branchHasCommits:
                if currentBranch != parentBranch:
                    print(f"    checkout {sanitizeBranchName(parentBranch)}")
                    currentBranch = parentBranch
                anchor = uniqueLabel("...", usedLabels)
                print(f'    commit id: "{anchor}" type: HIGHLIGHT')
                branchHasCommits.add(parentBranch)
            if parentBranch != currentBranch:
                print(f"    checkout {sanitizeBranchName(parentBranch)}")
                currentBranch = parentBranch
            print(f"    branch {sanitizeBranchName(branch)}")
            createdBranches.add(branch)
            currentBranch = branch
        elif branch != currentBranch:
            print(f"    checkout {sanitizeBranchName(branch)}")
            currentBranch = branch

        label = uniqueLabel(makeLabel(commit, args.label), usedLabels)
        tagAttr = ""
        if args.tags and commit.hexsha in tagMap:
            tagAttr = f' tag: "{tagMap[commit.hexsha]}"'

        if isMerge:
            merged = False
            for parent in commit.parents[1:]:
                mergeBranch = assignment.get(parent.hexsha, mainBranch)
                if mergeBranch in createdBranches and mergeBranch != branch:
                    print(f'    merge {sanitizeBranchName(mergeBranch)} id: "{label}"{tagAttr}')
                    merged = True
                    break
            if not merged:
                print(f'    commit id: "{label}"{tagAttr}')
        else:
            print(f'    commit id: "{label}"{tagAttr}')
        branchHasCommits.add(branch)


def uniqueLabel(label, usedLabels):
    """Ensure label is unique by appending a suffix if needed."""
    if label not in usedLabels:
        usedLabels.add(label)
        return label
    counter = 2
    while f"{label}_{counter}" in usedLabels:
        counter += 1
    unique = f"{label}_{counter}"
    usedLabels.add(unique)
    return unique


def findParentBranch(commit, assignment, commitSet, mainBranch):
    """Find which branch a commit's first parent belongs to."""
    if commit.parents:
        parentSha = commit.parents[0].hexsha
        if parentSha in commitSet:
            return assignment.get(parentSha, mainBranch)
    return mainBranch


def main():
    args = parseArgs()

    try:
        repo = git.Repo(args.repo)
    except git.InvalidGitRepositoryError:
        print(f"Error: '{args.repo}' is not a git repository.", file=sys.stderr)
        sys.exit(1)
    except git.NoSuchPathError:
        print(f"Error: path '{args.repo}' does not exist.", file=sys.stderr)
        sys.exit(1)

    mainBranch = findMainBranch(repo)
    commits = collectCommits(repo, args, mainBranch)

    if not commits:
        print("Error: no commits found in the specified range.", file=sys.stderr)
        sys.exit(1)

    tagMap = buildTagMap(repo) if args.tags else {}
    assignment = assignBranches(repo, commits, mainBranch)
    if args.squash:
        commits = squashCommits(commits, assignment, mainBranch, args.max_commits)
        if not commits:
            print("Error: no commits remain after squash.", file=sys.stderr)
            sys.exit(1)
    if args.markdown:
        print("```mermaid")
    emitGitgraph(commits, assignment, tagMap, args, mainBranch)
    if args.markdown:
        print("```")


if __name__ == "__main__":
    main()

# Implementation Status

- [Program Startup and Shutdown](Program-Startup-and-Shutdown)
- [Image Queries](#Image-Queries)
- [Storage Management](#Storage-Management)
- [Coarray Queries](#Coarray-Queries)
- [Continguous Coarray Access](#Continguous-Coarray-Access)
- [Strided Coarray Access](#Strided-Coarray-Access)
- [SYNC Statements](#SYNC-Statements)
- [Locks and Unlocks](#Locks-and-Unlocks)
- [Critical](#Critical)
- [Events and Notifications](#Events-and-Notifications)
- [Teams](#teams)
- [Collectives](collectives#)
- [Atomic Memory Operations](#Atomic-Memory-Operations)

TODO: For features with all no support, flesh out table, but comment it out

TODO: Add verbage that interfaces for all PRIF procedures exist and are callable but not all semantics of the procedures exist and the status is described below.

TODO: Link latest version of spec here:

### PRIF-specific constants

Yes (update with verbage)

### `stat` and `errmsg` support

No (update with verbage)


## Program Startup and Shutdown
### Support = ...

| Procedure | Status | Notes |
|-----------|--------|-------|
| ......... | ...... | ..... |
| ......... | ...... | ..... |

---

## Image Queries - partial
### Support = partial (no support for `team` and `team_number` arguments)

TODO: Add notes to table

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_this_image`, `prif_num_images`    | yes |
| `prif_this_image_team`, `prif_num_images_team`  | no |
| `prif_this_image_team_number`, `prif_num_images_team_number`  | no |

---

## Storage Management
### Support = partial

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_allocate_coarray`    | yes | |
| `prif_allocate`    | yes |
| `prif_deallocate_coarray`    | partial | no `final_func` arg support |
| `prif_deallocate`    | yes |

---

## Coarray Queries
### Support = ...

| Procedure | Status | Notes |
|-----------|--------|-------|
| ......... | ...... | ..... |
| ......... | ...... | ..... |

---

## Continguous Coarray Access
### Support = ...

| Procedure | Status | Notes |
|-----------|--------|-------|
| ......... | ...... | ..... |
| ......... | ...... | ..... |

---

## Strided Coarray Access
### Support = ...

| Procedure | Status | Notes |
|-----------|--------|-------|
| ......... | ...... | ..... |
| ......... | ...... | ..... |

---

## SYNC Statements
### Support = partial ( support ...)

| Procedure | Status | Notes |
|-----------|--------|-------|
| ......... | ...... | ..... |
| ......... | ...... | ..... |

---

## Locks and Unlocks
### Support = no

| Procedure | Status | Notes |
|-----------|--------|-------|
| ......... | ...... | ..... |
| ......... | ...... | ..... |

---

## Critical
### Support = no

| Procedure | Status | Notes |
|-----------|--------|-------|
| ......... | ...... | ..... |
| ......... | ...... | ..... |

---

## Events and Notifications
### Support = no

| Procedure | Status | Notes |
|-----------|--------|-------|
| ......... | ...... | ..... |
| ......... | ...... | ..... |

---

## Teams
### Support = partial (...)

| Procedure | Status | Notes |
|-----------|--------|-------|
| ......... | ...... | ..... |
| ......... | ...... | ..... |

---

## Collectives
### Support = partial (...)

| Procedure | Status | Notes |
|-----------|--------|-------|
| `co_sum` | partial |  |

---

## Atomic Memory Operations
### Support = no

| Procedure | Status | Notes |
|-----------|--------|-------|
| ......... | ...... | ..... |
| ......... | ...... | ..... |

---

# Implementation Status

Caffeine is an implementation of the Parallel Runtime Interface for Fortran (PRIF). This document
outlines the implementation status in Caffeine of the features defined in the
[latest PRIF revision](https://dx.doi.org/10.25344/S4WG64). Caffeine contains interfaces for all
of the PRIF procedures and are callable, but some procedure definitions are unimplemented. For
more details about the implementation of the various PRIF features, please see the
following sections:

- [PRIF-specific constants](#PRIF-specific-constants)
- [`stat` and `errmsg` support](#`stat`-and-`errmsg`-support)
- [Program Startup and Shutdown](#Program-Startup-and-Shutdown)
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

## PRIF-specific constants

Yes (update with verbage)

## `stat` and `errmsg` support

No (update with verbage)


## Program Startup and Shutdown
### Support = partial

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_init` | yes | n/a |
| `prif_stop`, `prif_error_stop` | partial | Missing support for `quiet=.true.` |
| `prif_fail_image` | no | n/a |

---

## Image Queries
### Support = partial

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_num_images`    | yes | n\a |
| `prif_num_images_with_team`, `prif_num_images_with_team_number`  | no | n\a |
| `prif_this_image_no_coarray` | yes | n\a |
| `prif_this_image_with_coarray`, `prif_this_image_with_dim`  | no | n\a |
| `prif_failed_images` | no | n/a |
| `prif_stopped_images` | no | n/a |
| `prif_image_status` | no | n/a |

---

## Storage Management
### Support = partial (no support for coarray aliases)

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_allocate_coarray`    | yes | n/a |
| `prif_allocate`    | yes | n/a |
| `prif_deallocate_coarray`    | partial | no `final_func` arg support |
| `prif_deallocate`    | yes | n/a |
| `prif_alias_create`    | no | n/a |
| `prif_alias_destroy`    | no | n/a |

---

## Coarray Queries
### Support = partial (only support for `prif_image_index`)

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_set_context_data`, `prif_get_context_data` | no | n\a |
| `prif_size_bytes` | no | n\a |
| `prif_lcobound_no_dim`, `prif_lcobound_with_dim` | no | n\a |
| `prif_ucobound_no_dim`, `prif_ucobound_with_dim` | no | n\a |
| `prif_coshape` | no | n\a |
| `prif_image_index` | yes | n\a |
| `prif_image_index_with_team` | no | n\a |
| `prif_image_index_with_team_number` | no | n\a |

---

## Continguous Coarray Access
### Support = partial

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_get`                               | yes | n\a |
| `prif_get_indirect`                      | yes | n\a |
| `prif_put`                               | yes | n\a |
| `prif_put_indirect`                      | yes | n\a |
| `prif_put_with_notify`                   | no  | n\a |
| `prif_put_with_notify_indirect`          | no  | n\a |
| `prif_put_indirect_with_notify`          | no  | n\a |
| `prif_put_indirect_with_notify_indirect` | no  | n\a |

---

## Strided Coarray Access
### Support = no

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_get_strided`                               | no  | n\a |
| `prif_get_strided_indirect`                      | no  | n\a |
| `prif_put_strided`                               | no  | n\a |
| `prif_put_strided_indirect`                      | no  | n\a |
| `prif_put_strided_with_notify`                   | no  | n\a |
| `prif_put_strided_with_notify_indirect`          | no  | n\a |
| `prif_put_strided_indirect_with_notify`          | no  | n\a |
| `prif_put_strided_indirect_with_notify_indirect` | no  | n\a |

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

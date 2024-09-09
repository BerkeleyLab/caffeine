# Implementation Status

Caffeine is an implementation of the Parallel Runtime Interface for Fortran (PRIF). This document
outlines the implementation status in Caffeine of the features defined in the
[latest PRIF specification, revision 0.4](https://dx.doi.org/10.25344/S4WG64). Caffeine contains interfaces for all
of the PRIF procedures and the symbols are linkable and callable, but some procedures will fail at runtime with an unimplemented error. For
more details about the implementation of the various PRIF features, please see the
following sections:

- [Named Constants](#Named-Constants)
- [`stat` and `errmsg` support](#stat-and-errmsg-support) - NO support yet
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
- [Collectives](#Collectives)
- [Atomic Memory Operations](#Atomic-Memory-Operations)

TODO: For features with all no support, flesh out table, but comment it out

## Named Constants

Caffeine contains definitions for all of the PRIF-relevant constants from ISO_FORTRAN_ENV and for
all of the PRIF-specific constants.

## `stat` and `errmsg` support

Many PRIF procedures have optional arguments `stat`, `errmsg`, and `errmsg_alloc`. There is no
support for these optional arguments in Caffeine yet and this is not represented in the tables
in the following sections.


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

<!---
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
-->

---

## SYNC Statements
### Support = partial ( only support for `prif_sync_all`)

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_sync_memory` | no  | n\a |
| `prif_sync_all`    | yes | n\a |
| `prif_sync_images` | no  | n\a |
| `prif_sync_team`   | no  | n\a |

---

## Locks and Unlocks
### Support = no

<!---
| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_lock`            | no  | n\a |
| `prif_lock_indirect`   | no  | n\a |
| `prif_unlock`          | no  | n\a |
| `prif_unlock_indirect` | no  | n\a |
-->

---

## Critical
### Support = no

<!---
| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_critical`     | no  | n\a |
| `prif_end_critical` | no  | n\a |
-->

---

## Events and Notifications
### Support = no

<!---
| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_event_post`          | no  | n\a |
| `prif_event_post_indirect` | no  | n\a |
| `prif_event_wait`          | no  | n\a |
| `prif_event_query`         | no  | n\a |
| `prif_notify_wait`         | no  | n\a |
-->

---

## Teams
### Support = partial (No support for `prif_get_team` and `prif_team_number`)

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_form_team`   | yes | n\a |
| `prif_get_team`    | no  | n\a |
| `prif_team_number` | no  | n\a |
| `prif_change_team` | yes | n\a |
| `prif_end_team`    | yes | n\a |

---

## Collectives
### Support = partial (...)

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_co_broadcast` | ... | ... |
| `prif_co_max`       | ... | ... |
| `prif_co_min`       | ... | ... |
| `prif_co_sum`       | ... | ... |
| `prif_co_reduce`    | ... | ... |

---

## Atomic Memory Operations
### Support = no

<!---
| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_atomic_add`          | no  | n\a |
| `prif_atomic_add_indirect` | no  | n\a |
| `prif_atomic_and`          | no  | n\a |
| `prif_atomic_and_indirect` | no  | n\a |
| `prif_atomic_or`           | no  | n\a |
| `prif_atomic_or_indirect`  | no  | n\a |
| `prif_atomic_xor`          | no  | n\a |
| `prif_atomic_xor_indirect` | no  | n\a |
| `prif_atomic_cas_int`                 | no  | n\a |
| `prif_atomic_cas_int_indirect`        | no  | n\a |
| `prif_atomic_cas_logical`             | no  | n\a |
| `prif_atomic_cas_logical_indirect`    | no  | n\a |
| `prif_atomic_fetch_add`               | no  | n\a |
| `prif_atomic_fetch_add_indirect`      | no  | n\a |
| `prif_atomic_fetch_and`               | no  | n\a |
| `prif_atomic_fetch_and_indirect`      | no  | n\a |
| `prif_atomic_fetch_or`                | no  | n\a |
| `prif_atomic_fetch_or_indirect`       | no  | n\a |
| `prif_atomic_fetch_xor`               | no  | n\a |
| `prif_atomic_fetch_xor_indirect`      | no  | n\a |
| `prif_atomic_define_int`              | no  | n\a |
| `prif_atomic_define_int_indirect`     | no  | n\a |
| `prif_atomic_define_logical`          | no  | n\a |
| `prif_atomic_define_logical_indirect` | no  | n\a |
| `prif_atomic_ref_int`                 | no  | n\a |
| `prif_atomic_ref_int_indirect`        | no  | n\a |
| `prif_atomic_ref_logical`             | no  | n\a |
| `prif_atomic_ref_logical_indirect`    | no  | n\a |
-->

---
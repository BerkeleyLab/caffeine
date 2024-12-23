# Implementation Status

Caffeine is an implementation of the Parallel Runtime Interface for Fortran (PRIF). This document
outlines the implementation status in Caffeine of the features defined in the
[latest PRIF specification, revision 0.5](https://dx.doi.org/10.25344/S4CG6G). Caffeine contains interfaces for all
of the PRIF procedures (except when stated otherwise below) and the symbols are linkable and callable, but some procedures will fail at runtime with an unimplemented error. For
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
| `prif_init` | **YES** |  |
| `prif_stop`, `prif_error_stop` | *partial* | Missing support for `quiet=.true.` |
| `prif_fail_image` | no |  |
| `prif_register_stop_callback` | **YES** |  |


---

## Image Queries
### Support = partial

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_num_images`    | **YES** |  |
| `prif_num_images_with_team`, `prif_num_images_with_team_number`  | no |  |
| `prif_this_image_no_coarray` | *partial* | team argument is ignored |
| `prif_this_image_with_coarray`, `prif_this_image_with_dim`  | no |  |
| `prif_failed_images` | no |  |
| `prif_stopped_images` | no |  |
| `prif_image_status` | no |  |

---

## Storage Management
### Support = partial (no support for coarray aliases)

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_allocate_coarray`    | **YES** |  |
| `prif_allocate`    | **YES** |  |
| `prif_deallocate_coarray`    | *partial* | no `final_func` arg support |
| `prif_deallocate`    | **YES** |  |
| `prif_alias_create`    | no |  |
| `prif_alias_destroy`    | no |  |

---

## Coarray Queries
### Support = partial (only support for `prif_image_index`)

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_set_context_data`, `prif_get_context_data` | no |  |
| `prif_size_bytes` | no |  |
| `prif_lcobound_no_dim`, `prif_lcobound_with_dim` | no |  |
| `prif_ucobound_no_dim`, `prif_ucobound_with_dim` | no |  |
| `prif_coshape` | no |  |
| `prif_local_data_pointer` | **YES** |  |
| `prif_image_index` | **YES** |  |
| `prif_image_index_with_team` | no |  |
| `prif_image_index_with_team_number` | no |  |

---

## Continguous Coarray Access
### Support = partial

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_get`                               | **YES**   |  |
| `prif_get_indirect`                      | **YES**   |  |
| `prif_put`                               | **YES**   |  |
| `prif_put_indirect`                      | **YES**   |  |
| `prif_put_with_notify`                   | no        |  |
| `prif_put_with_notify_indirect`          | no        |  |
| `prif_put_indirect_with_notify`          | no        |  |
| `prif_put_indirect_with_notify_indirect` | no        |  |

---

## Strided Coarray Access
### Support = no

<!---
| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_get_strided`                               | no  |  |
| `prif_get_strided_indirect`                      | no  |  |
| `prif_put_strided`                               | no  |  |
| `prif_put_strided_indirect`                      | no  |  |
| `prif_put_strided_with_notify`                   | no  |  |
| `prif_put_strided_with_notify_indirect`          | no  |  |
| `prif_put_strided_indirect_with_notify`          | no  |  |
| `prif_put_strided_indirect_with_notify_indirect` | no  |  |
-->

---

## SYNC Statements
### Support = partial ( only support for `prif_sync_all`)

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_sync_memory` | no        |  |
| `prif_sync_all`    | **YES**   |  |
| `prif_sync_images` | no        |  |
| `prif_sync_team`   | no        |  |

---

## Locks and Unlocks
### Support = no

<!---
| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_lock`            | no  |  |
| `prif_lock_indirect`   | no  |  |
| `prif_unlock`          | no  |  |
| `prif_unlock_indirect` | no  |  |
-->

---

## Critical
### Support = no

<!---
| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_critical`     | no  |  |
| `prif_end_critical` | no  |  |
-->

---

## Events and Notifications
### Support = no

<!---
| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_event_post`          | no  |  |
| `prif_event_post_indirect` | no  |  |
| `prif_event_wait`          | no  |  |
| `prif_event_query`         | no  |  |
| `prif_notify_wait`         | no  |  |
-->

---

## Teams
### Support = partial (No support for `prif_get_team` and `prif_team_number`)

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_form_team`   | **YES**   |  |
| `prif_get_team`    | no        |  |
| `prif_team_number` | no        |  |
| `prif_change_team` | **YES**   |  |
| `prif_end_team`    | **YES**   |  |

---

## Collectives
### Support = partial (...)

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_co_broadcast` | *partial* | no support for derived types with `allocatable` components |
| `prif_co_max`       | *partial* | only supports 32-bit and 64-bit numeric types |
| `prif_co_max_character` | no | procedure not yet added to Caffeine |
| `prif_co_min`       | *partial* | only supports 32-bit and 64-bit numeric types |
| `prif_co_min_character` | no | procedure not yet added to Caffeine |
| `prif_co_sum`       | *partial* | only supports 32-bit and 64-bit numeric types |
| `prif_co_reduce`    | *partial* | only supports intrinsic types (no support for derived types), interface not yet updated to v0.5 |

---

## Atomic Memory Operations
### Support = no

<!---
| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_atomic_add`          | no  |  |
| `prif_atomic_add_indirect` | no  |  |
| `prif_atomic_and`          | no  |  |
| `prif_atomic_and_indirect` | no  |  |
| `prif_atomic_or`           | no  |  |
| `prif_atomic_or_indirect`  | no  |  |
| `prif_atomic_xor`          | no  |  |
| `prif_atomic_xor_indirect` | no  |  |
| `prif_atomic_cas_int`                 | no  |  |
| `prif_atomic_cas_int_indirect`        | no  |  |
| `prif_atomic_cas_logical`             | no  |  |
| `prif_atomic_cas_logical_indirect`    | no  |  |
| `prif_atomic_fetch_add`               | no  |  |
| `prif_atomic_fetch_add_indirect`      | no  |  |
| `prif_atomic_fetch_and`               | no  |  |
| `prif_atomic_fetch_and_indirect`      | no  |  |
| `prif_atomic_fetch_or`                | no  |  |
| `prif_atomic_fetch_or_indirect`       | no  |  |
| `prif_atomic_fetch_xor`               | no  |  |
| `prif_atomic_fetch_xor_indirect`      | no  |  |
| `prif_atomic_define_int`              | no  |  |
| `prif_atomic_define_int_indirect`     | no  |  |
| `prif_atomic_define_logical`          | no  |  |
| `prif_atomic_define_logical_indirect` | no  |  |
| `prif_atomic_ref_int`                 | no  |  |
| `prif_atomic_ref_int_indirect`        | no  |  |
| `prif_atomic_ref_logical`             | no  |  |
| `prif_atomic_ref_logical_indirect`    | no  |  |
-->

---
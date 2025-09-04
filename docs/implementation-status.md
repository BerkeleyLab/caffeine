# Implementation Status

Caffeine is an implementation of the Parallel Runtime Interface for Fortran (PRIF). This document
outlines the implementation status in Caffeine of the features defined in the
[latest PRIF specification, revision 0.5](https://dx.doi.org/10.25344/S4CG6G). Caffeine contains interfaces for all
of the PRIF procedures (except when stated otherwise below) and the symbols are linkable and callable, but some procedures will fail at runtime with an unimplemented error. For
more details about the implementation of the various PRIF features, please see the
following sections:

- [Named Constants](#Named-Constants)
- [`stat` and `errmsg` support](#stat-and-errmsg-support)
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

The priorites for feature implementation and addressing known defects is communicated by
the labels in the Caffeine [issue tracker](https://github.com/BerkeleyLab/caffeine/issues).

## Named Constants

Caffeine contains definitions for all of the PRIF-relevant constants from ISO_FORTRAN_ENV and for
all of the PRIF-specific constants.

## `stat` and `errmsg` support

Many PRIF procedures have optional arguments `stat`, `errmsg`, and `errmsg_alloc`. These arguments
are accepted, but in some cases, the associated runtime behavior is not fully implemented.

## Program Startup and Shutdown

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_init` | **YES** |  |
| `prif_stop`, `prif_error_stop` | **YES** |  |
| `prif_fail_image` | no |  |
| `prif_register_stop_callback` | **YES** |  |


---

## Image Queries

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_num_images`                   | **YES** |  |
| `prif_num_images_with_team`         | **YES** |  |
| `prif_num_images_with_team_number`  | *partial* | no support for sibling teams |
| `prif_this_image_no_coarray`        | **YES** |  |
| `prif_this_image_with_coarray`, `prif_this_image_with_dim`  | **YES** |  |
| `prif_failed_images`                | **YES** |  |
| `prif_stopped_images`               | **YES** |  |
| `prif_image_status`                 | **YES** |  |

---

## Storage Management

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_allocate_coarray`    | **YES** |  |
| `prif_allocate`            | **YES** |  |
| `prif_deallocate_coarray`  | *partial* | no `final_func` arg support |
| `prif_deallocate`          | **YES** |  |
| `prif_alias_create`        | **YES** | includes `data_pointer_offset` argument added in PRIF 0.6 |
| `prif_alias_destroy`       | **YES** |  |

---

## Coarray Queries

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_set_context_data`, `prif_get_context_data` | **YES** |  |
| `prif_size_bytes`                                | **YES** |  |
| `prif_lcobound_no_dim`, `prif_lcobound_with_dim` | **YES** |  |
| `prif_ucobound_no_dim`, `prif_ucobound_with_dim` | **YES** |  |
| `prif_coshape`                                   | **YES** |  |
| `prif_local_data_pointer`                        | **YES** |  |
| `prif_image_index`                               | **YES** |  |
| `prif_image_index_with_team`                     | **YES** |  |
| `prif_image_index_with_team_number`              | *partial* | no support for sibling teams |
| `prif_initial_team_index`                        | **YES** | |
| `prif_initial_team_index_with_team`              | **YES** | |
| `prif_initial_team_index_with_team_number`       | *partial* | no support for sibling teams |

---

## Contiguous Coarray Access

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_get`                               | **YES** |  |
| `prif_get_indirect`                      | **YES** |  |
| `prif_put`                               | **YES** |  |
| `prif_put_indirect`                      | **YES** |  |
| `prif_put_with_notify`                   | **YES** |  |
| `prif_put_with_notify_indirect`          | **YES** |  |
| `prif_put_indirect_with_notify`          | **YES** |  |
| `prif_put_indirect_with_notify_indirect` | **YES** |  |

---

## Strided Coarray Access

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_get_strided`                               | **YES** |  |
| `prif_get_strided_indirect`                      | **YES** |  |
| `prif_put_strided`                               | **YES** |  |
| `prif_put_strided_indirect`                      | **YES** |  |
| `prif_put_strided_with_notify`                   | **YES** |  |
| `prif_put_strided_with_notify_indirect`          | **YES** |  |
| `prif_put_strided_indirect_with_notify`          | **YES** |  |
| `prif_put_strided_indirect_with_notify_indirect` | **YES** |  |

---

## SYNC Statements

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_sync_memory` | **YES** |  |
| `prif_sync_all`    | **YES** |  |
| `prif_sync_images` | **YES** |  |
| `prif_sync_team`   | **YES** |  |

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

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_event_post`          | **YES** |  |
| `prif_event_post_indirect` | **YES** |  |
| `prif_event_wait`          | **YES** |  |
| `prif_event_query`         | **YES** |  |
| `prif_notify_wait`         | **YES** |  |

---

## Teams

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_form_team`   | **YES**   |  |
| `prif_get_team`    | **YES**   |  |
| `prif_team_number` | **YES**   |  |
| `prif_change_team` | **YES**   |  |
| `prif_end_team`    | **YES**   |  |

---

## Collectives

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_co_broadcast`     | **YES** |  |
| `prif_co_max`           | **YES** |  |
| `prif_co_max_character` | **YES** |  |
| `prif_co_min`           | **YES** |  |
| `prif_co_min_character` | **YES** |  |
| `prif_co_sum`           | **YES** |  |
| `prif_co_reduce`        | **YES** |  |

---

## Atomic Memory Operations

| Procedure | Status | Notes |
|-----------|--------|-------|
| `prif_atomic_add`                     | **YES** |  |
| `prif_atomic_add_indirect`            | **YES** |  |
| `prif_atomic_and`                     | **YES** |  |
| `prif_atomic_and_indirect`            | **YES** |  |
| `prif_atomic_or`                      | **YES** |  |
| `prif_atomic_or_indirect`             | **YES** |  |
| `prif_atomic_xor`                     | **YES** |  |
| `prif_atomic_xor_indirect`            | **YES** |  |
| `prif_atomic_cas_int`                 | **YES** |  |
| `prif_atomic_cas_int_indirect`        | **YES** |  |
| `prif_atomic_cas_logical`             | **YES** |  |
| `prif_atomic_cas_logical_indirect`    | **YES** |  |
| `prif_atomic_fetch_add`               | **YES** |  |
| `prif_atomic_fetch_add_indirect`      | **YES** |  |
| `prif_atomic_fetch_and`               | **YES** |  |
| `prif_atomic_fetch_and_indirect`      | **YES** |  |
| `prif_atomic_fetch_or`                | **YES** |  |
| `prif_atomic_fetch_or_indirect`       | **YES** |  |
| `prif_atomic_fetch_xor`               | **YES** |  |
| `prif_atomic_fetch_xor_indirect`      | **YES** |  |
| `prif_atomic_define_int`              | **YES** |  |
| `prif_atomic_define_int_indirect`     | **YES** |  |
| `prif_atomic_define_logical`          | **YES** |  |
| `prif_atomic_define_logical_indirect` | **YES** |  |
| `prif_atomic_ref_int`                 | **YES** |  |
| `prif_atomic_ref_int_indirect`        | **YES** |  |
| `prif_atomic_ref_logical`             | **YES** |  |
| `prif_atomic_ref_logical_indirect`    | **YES** |  |

---

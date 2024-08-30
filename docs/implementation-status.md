# Implementation Status

- [Program Startup and Shutdown](Program-Startup-and-Shutdown)
- [Image Queries](#Image-Queries)
- [Storage Management](#Storage-Management)
- [Coarray Queries](#Coarray-Queries)
- [](#)
- [Collectives](collectives#)
- [Teams](#teams)
- [Events](#events)

TODO: Add verbage that interfaces for all PRIF procedures exist and are callable but not all semantics of the procedures exist and the status is described below.

TODO: Link latest version of spec here:

### PRIF-specific constants

Yes (update with verbage)

### `stat` and `errmsg` support

No (update with verbage)

---

## Image Queries - partial (no team argument support)

TODO: Add notes to table

| Procedure | Status |
|---------|--------|
| `prif_this_image`, `prif_num_images`    | yes |
| `prif_this_image_team`, `prif_num_images_team`  | no |
| `prif_this_image_team_number`, `prif_num_images_team_number`  | no |

---

## Storage Management

| Procedure | Status | Notes |
|---------|--------|-------|
| `prif_allocate_coarray`    | yes | |
| `prif_allocate`    | yes |
| `prif_deallocate_coarray`    | partial | no `final_func` arg support |
| `prif_deallocate`    | yes |

---

## Collective - partial

| Procedure | Status | Notes |
|---------|--------|--------|
| `co_sum` | partial |  |




---

## Teams

TODO: Add notes to table

| Procedure | Status |
|---------|--------|
| ... | ... |
| ... | ... |

---

## Events

TODO: For features with all no support, flesh out table, but comment it out

TODO: Add notes to table

| Procedure | Status |
|---------|--------|
| ... | ... |
| ... | ... |

---

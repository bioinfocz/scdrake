---
name: Bug report
about: Describe a bug you've experienced
title: "[BUG]"
labels: ''
assignees: ''

---

Please, briefly describe your problem.

If the problem is something else than a failing pipeline, post the code which failed for you and the output from `traceback()`. You can also include what output you expect.

If the problem is a failing pipeline, please, report the error message from `{drake}`, e.g.:

```
✖ fail cluster_markers_processed
Error : target cluster_markers_processed failed.
diagnose(cluster_markers_processed)$error$message:
  Problem while computing `markers = purrr::pmap(...)`.
```

<h1 align="center">alexandrite</h1>
<p align="center">a language implementation for PureScript</p>

---

Alexandrite is a language implementation for PureScript, powered by an incremental, query-based build
system. Instead of a sequence of compiler phases, Alexandrite models compilation and semantic information
as incrementally computed queries. These queries are used extensively to implement code intelligence
features in the language server.

The build system is designed with interactive editing in mind. To support this, it tracks dependencies
between inputs and queries, caches query results, deduplicates in-progress work across threads, and
supports cooperative cancellation when inputs change. Crucially, many query results are designed to
be incrementally reusable. For example, the compiler uses stable identities in lieu of source ranges 
to enable minimal recomputation across trivial formatting changes.

The language server component implements core code intelligence features such as completion, jump to
definition, hover information, find references, workspace symbol search, and diagnostics.

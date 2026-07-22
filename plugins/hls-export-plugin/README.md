# Export Plugin

The export plugin offers three code actions on a module's export list:

- Export `...` adds a top-level declaration the export list omits.
- Unexport `...` removes a top-level declaration the export list names.
- Export explicitly rewrites the whole list, on the module header, to name only
  what other modules reference. It writes a list when the module has none.

Export and Unexport need an explicit export list. Export explicitly writes one.

## Known limitations

The plugin offers no Export or Unexport action on:

- Class methods (`class C where { m :: ... }`)
- Module re-exports (`module M`)

Export explicitly deletes exports, so the plugin withholds it whenever it cannot
see every consumer:

- A library exposes the module, so a consumer can sit outside the project.
- The export list holds a `module M` re-export, or a CPP directive.
- The cabal file declares a component the session never loaded.
- The hiedb index is missing a reverse dependency.

Under CPP the plugin splices an addition in beside the directives. It declines a
removal, because a reprint erases them.

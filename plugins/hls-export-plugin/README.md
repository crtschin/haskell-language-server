# Export Plugin

The export plugin offers three code actions on a module's export list:

- Export `...` adds a top-level declaration to the export list.
- Unexport `...` removes a top-level declaration from the export list.
- Export explicitly trims an export list to in-package usages.

## Known limitations

The plugin offers no Export or Unexport action on:

- Class methods (`class C where { m :: ... }`)
- Module re-exports (`module M`)

Export explicitly supports in-project usages, so has the following limitations:

- Requires `haskell.componentsLoading` to be `multi: whole-project`.
- A library does not expose the module.
- The export list holds a `module M` re-export, or a CPP directive.

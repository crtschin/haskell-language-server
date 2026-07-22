# Export Plugin

The export plugin provides code actions for working with a module's export list. It provides code actions for:

- Export `...`: on top-level declaration symbols.
- Unexport `...`: on exported top-level declaration symbols.
- Remove unused exports: on the module header. Trims the export list to the names that other modules in the project use. A module without an export list gets one.

Export and Unexport are only offered when the module has an explicit export list.

## Known limitations

Not yet supported:
- Class methods (`class C where { m :: ... }`)
- Type and data families (standalone or associated)
- Pattern synonyms (`pattern P :: ...`)
- Module re-exports (`module M`)

Remove unused exports is only offered when:
- `haskell.componentsLoading` is `multi: whole-project`.
- No public library lists the module in `exposed-modules`.
- The module is not the main module of an executable.
- The export list does not contain `module M` re-exports or CPP directives.
- No module that imports the module uses CPP.

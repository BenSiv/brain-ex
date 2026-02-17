# Publication Artifacts

Use this directory for release-ready publication files, for example:

- `.deb` packages
- `.rpm` packages
- release archives (`.tar.gz`, `.zip`)

The repository keeps this directory, but ignores generated package files by default.

## Build Debian Package

From the project root:

```bash
./bld/build.sh
./pub/package_deb.sh
```

The generated package is written to `pub/`.

### Maintainer

The script uses:

`Maintainer: Ben Sivan <bensiv92@gmail.com>`

### Dependencies

`pub/package_deb.sh` resolves dependencies from:

- shared libraries used by `bin/brex` (via `ldd` + `dpkg-query`)
- command-level dependency: `sqlite3` (used by `brex sql` shell mode)

It also sets:

- `Recommends: git, nano`

because:

- `git` is used by `brex init --git`
- `nano` is the default editor if no custom editor is configured

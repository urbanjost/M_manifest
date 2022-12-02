# M_manifest

Test module for exercising a proposed change to fpm

```text
app
/     fpm-describe.f90  -- create plugin to show keywords "fpm describe license"
/     fpm-dump.f90      -- create plugin to dump processed manifest
/     fpm-manifest.f90  -- create plugin to write routine for Fortran to use snapshot of manifest
/     fpm-version.f90   -- create plugin to show version information of package you are sitting in
/     main.f90
fpm.rsp
fpm.toml
.gitignore
README.md
src
/     M_manifest.F90   -- file that should be able to do everything with preprocessing, but 
                          currently only {version} works
```

# 0.0.9
## New Feature

* Improve error messages
* New syntax: optional field namely `struct { field?: type }`

## Internal

* update combine to 4

# 0.0.8
## Bug Fix

* code generation of `nullable` with reference types are fixed

## Internal

* migrated code base to Rust 2018 Edition


# 0.0.7
## New Feature

* support `TYPE where PRED` synax
  + it looks like `string where 1 <= length && length <= 128 && it =~ /[a-z0-9!"#$%&'()=~|@]+/`

# 0.0.6
## New Feature

* support `url("schema_url")` syntax
* support `--path-prefix` flag

## Internal

* update dependencies

# 0.0.5

???

# 0.0.4
## New Feature

* support format types

# 0.0.3
## New Feature

* support string constant type

# 0.0.2
## Bug Fix

* fix a bug of multiple comment handling

# 0.0.1
initial release

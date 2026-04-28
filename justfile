default:
    @just --list

# run all CI checks
ci: format lint test

# check formatting
format:
    @cargo fmt --all -- --check

# run lints (errors on warnings)
lint:
    @cargo clippy --all-targets --all-features -- -D warnings

# run all tests in the workspace
test:
    @cargo nextest run --all-features

check:
    @cargo check --all-targets

build:
    @cargo build --release

# clean build artifacts
clean:
    @cargo clean

# wasm-gravity

```sh
cargo install --locked trunk

rustup override set nightly
rustup target add wasm32-unknown-unknown

# release performance is necessary
trunk serve --release --open
```